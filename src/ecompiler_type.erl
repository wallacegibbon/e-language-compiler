-module(ecompiler_type).

-export([checktype_ast/2, typeof_expr/2]).

-import(ecompiler_utils, [expr2str/1, flat_format/2]).

-include("./ecompiler_frame.hrl").

checktype_ast({FunMap, StructMap}, GlobalVars) ->
    lists:map(fun (S) ->
		      checktype_struct(S, StructMap)
	      end, maps:values(StructMap)),
    lists:map(fun (F) ->
		      checktype_function(F, FunMap, StructMap, GlobalVars)
	      end, maps:values(FunMap)),
    ok.

checktype_function(#function{vars=Vars, exprs=Exprs, type=Fntype},
		   Functions, Structs, GlobalVars) ->
    %% TODO: check param default values
    lists:map(fun (T) -> checktype_type(T, Structs) end, maps:values(Vars)),
    checktype_type(Fntype#fun_type.ret, Structs),
    CurrentVars = maps:merge(GlobalVars, Vars),
    Ctx = {CurrentVars, Functions, Structs, Fntype#fun_type.ret},
    typeof_exprs(Exprs, Ctx).

checktype_struct(#struct{fields=Fields}, Structs) ->
    %% TODO: check init code
    lists:map(fun (T) -> checktype_type(T, Structs) end, maps:values(Fields)).

typeof_exprs([Expr | Rest], Ctx) ->
    [typeof_expr(Expr, Ctx) | typeof_exprs(Rest, Ctx)];
typeof_exprs([], _) ->
    [].

typeof_expr(#op2{operator=assign, op1=Op1, op2=Op2, line=Line},
	    {_, _, Structs, _} = Ctx) ->
    TypeofOp1 = case Op1 of
		    #op2{operator='.', op1=SubOp1, op2=SubOp2} ->
			typeof_structfield(typeof_expr(SubOp1, Ctx), SubOp2,
					   Structs, Line);
		    #op1{operator='^', operand=SubOperand} ->
			decr_pdepth(typeof_expr(SubOperand, Ctx));
		    #varref{name=_} ->
			typeof_expr(Op1, Ctx);
		    Any ->
			throw({Line, flat_format("invalid left value ~s",
						 [expr2str(Any)])})
		end,
    TypeofOp2 = typeof_expr(Op2, Ctx),
    case compare_type(TypeofOp1, TypeofOp2) of
	false ->
	    throw({Line, flat_format("type mismatch in '=', ~s = ~s",
				     [fmt_type(TypeofOp1),
				      fmt_type(TypeofOp2)])});
	_ ->
	    TypeofOp1
    end;
typeof_expr(#op2{operator='.', op1=Op1, op2=Op2, line=Line},
	    {_, _, Structs, _} = Ctx) ->
    typeof_structfield(typeof_expr(Op1, Ctx), Op2, Structs, Line);
typeof_expr(#op2{operator=Operator, op1=Op1, op2=Op2, line=Line}, Ctx) ->
    TypeofOp1 = typeof_expr(Op1, Ctx),
    TypeofOp2 = typeof_expr(Op2, Ctx),
    case compare_type(TypeofOp1, TypeofOp2) of
	false ->
	    throw({Line, flat_format("type mismatch in '~s', ~s ~s ~s",
				     [Operator, fmt_type(TypeofOp1), Operator,
				      fmt_type(TypeofOp2)])});
	_ ->
	    TypeofOp1
    end;
typeof_expr(#op1{operator='^', operand=Operand, line=Line}, Ctx) ->
    case typeof_expr(Operand, Ctx) of
	#basic_type{type={_, PDepth}} = T when PDepth > 0 ->
	    decr_pdepth(T);
	_ ->
	    throw({Line, flat_format("invalid operator \"^\" on operand ~s",
				     [expr2str(Operand)])})
    end;
typeof_expr(#op1{operator='@', operand=Operand, line=Line},
	    {_, _, Structs, _} = Ctx) ->
    case Operand of
	#op2{operator='.', op1=Op1, op2=Op2} ->
	    T = typeof_structfield(typeof_expr(Op1, Ctx), Op2, Structs, Line),
	    incr_pdepth(T);
	#varref{name=_} ->
	    incr_pdepth(typeof_expr(Operand, Ctx));
	_ ->
	    throw({Line, flat_format("invalid operator \"@\" on operand ~s",
				     [expr2str(Operand)])})
    end;
typeof_expr(#call{fn=FunExpr, args=Args, line=Line}, Ctx) ->
    ArgsTypes = lists:map(fun (A) -> typeof_expr(A, Ctx) end, Args),
    FnType = typeof_expr(FunExpr, Ctx),
    case compare_types(ArgsTypes, FnType#fun_type.params) of
	false ->
	    throw({Line, flat_format("argument type (~s) should be (~s)",
				     [fmt_types(ArgsTypes),
				      fmt_types(FnType#fun_type.params)])});
	true ->
	    FnType#fun_type.ret
    end;
typeof_expr(#if_expr{condition=Condition, then=Then, else=Else}, Ctx) ->
    typeof_expr(Condition, Ctx),
    typeof_exprs(Then, Ctx),
    typeof_exprs(Else, Ctx),
    %% TODO
    none;
typeof_expr(#while_expr{condition=Condition, exprs=Exprs}, Ctx) ->
    typeof_expr(Condition, Ctx),
    typeof_exprs(Exprs, Ctx),
    %% TODO
    none;
typeof_expr(#return{expr=Expr, line=Line}, {_, _, _, FnRetType} = Ctx) ->
    RealRet = typeof_expr(Expr, Ctx),
    case compare_type(RealRet, FnRetType) of
	false ->
	    throw({Line, flat_format("return type (~s) =/= fn ret type (~s)",
				     [fmt_type(RealRet),
				      fmt_type(FnRetType)])});
	true ->
	    RealRet
    end;
typeof_expr(#varref{name=Name, line=Line}, {Vars, Functions, Structs, _}) ->
    Vartype = case maps:find(Name, Vars) of
		  error ->
		      case maps:find(Name, Functions) of
			  error ->
			      throw({Line, flat_format("~s is undefined",
						       [Name])});
			  {ok, T} ->
			      T#function.type
		      end;
		  {ok, T} ->
		      T
	      end,
    checktype_type(Vartype, Structs),
    Vartype;
typeof_expr({float, Line, _}, _) ->
    #basic_type{type={f64, 0}, line=Line};
typeof_expr({integer, Line, _}, _) ->
    #basic_type{type={i64, 0}, line=Line};
typeof_expr({string, Line, _}, _) ->
    #basic_type{type={i8, 1}, line=Line}.

incr_pdepth(#basic_type{type={Tname, Pdepth}} = Type) ->
    Type#basic_type{type={Tname, Pdepth + 1}};
incr_pdepth(#box_type{elemtype=#basic_type{type={Type, N}, line=Line}}) ->
    #basic_type{type={Type, N + 1}, line=Line}.

decr_pdepth(#basic_type{type={Tname, Pdepth}} = Type) ->
    Type#basic_type{type={Tname, Pdepth - 1}};
decr_pdepth(#box_type{line=Line} = Type) ->
    throw({Line, flat_format("pointer - on box type ~s is invalid",
			     [fmt_type(Type)])}).

typeof_structfield(#basic_type{type={StructName, 0}}, #varref{name=FieldName},
		   Structs, Line) ->
    case maps:find(StructName, Structs) of
	{ok, S} ->
	    case maps:find(FieldName, S#struct.fields) of
		{ok, FieldType} ->
		    checktype_type(FieldType, Structs),
		    FieldType;
		error ->
		    throw({Line, flat_format("\"~s.~s\" does not exist",
					     [StructName, FieldName])})
	    end;
	error ->
	    throw({Line, flat_format("struct \"~s\" is not found",
				     [StructName])})
    end;
typeof_structfield(T, _, _, Line) ->
    throw({Line, flat_format("op1 for \".\" is not struct ~s",
			     [fmt_type(T)])}).

compare_types([T1 | Types1], [T2 | Types2]) ->
    case compare_type(T1, T2) of
	true ->
	    compare_types(Types1, Types2);
	_ ->
	    false
    end;
compare_types([], []) ->
    true;
compare_types(_, _) ->
    false.

compare_type(#fun_type{params=P1, ret=R1}, #fun_type{params=P2, ret=R2}) ->
    compare_types(P1, P2) and compare_type(R1, R2);
compare_type(#box_type{elemtype=E1, size=S1},
	     #box_type{elemtype=E2, size=S2}) ->
    compare_type(E1, E2) and (S1 =:= S2);
compare_type(#basic_type{type=A}, #basic_type{type=B}) ->
    A =:= B;
compare_type(_, _) ->
    false.

%% check type, ensure that all struct used by type exists.
checktype_type(#fun_type{params=Params, ret=Rettype}, Structs) ->
    lists:map(fun (P) ->
		      checktype_type(P, Structs)
	      end, Params),
    checktype_type(Rettype, Structs);
checktype_type(#box_type{elemtype=Elemtype}, Structs) ->
    case Elemtype of
	#box_type{line=Line} ->
	    throw({Line, "nested box is not supported"});
	_ ->
	    checktype_type(Elemtype, Structs)
    end;
checktype_type(#basic_type{type={TypeName, _}, line=Line}, Structs) ->
    checktype_typename(TypeName, Structs, Line).

checktype_typename(Type, Structs, Line) when is_atom(Type) ->
    case is_primitive_type(Type) of
	false ->
	    case maps:find(Type, Structs) of
		error ->
		    throw({Line, flat_format("struct \"~s\" is not found",
					     [Type])});
		{ok, _} ->
		    ok
	    end;
	true ->
	    ok
    end.

fmt_types(Types) -> fmt_types(Types, []).

fmt_types([Type | Rest], Result) ->
    fmt_types(Rest, [fmt_type(Type) | Result]);
fmt_types([], Result) ->
    lists:reverse(Result).

fmt_type(#fun_type{params=Params, ret=Rettype}) ->
    io_lib:format("fun(~s): ~s", [lists:join(",", fmt_types(Params)),
				  fmt_type(Rettype)]);
fmt_type(#box_type{elemtype=Type, size=N}) ->
    io_lib:format("<~s, ~w>", [Type, N]);
fmt_type(#basic_type{type={Type, Depth}}) when Depth > 0 ->
    io_lib:format("(~s~s)", [Type, lists:duplicate(Depth, "^")]);
fmt_type(#basic_type{type={Type, 0}}) ->
    atom_to_list(Type).

is_primitive_type(void) -> true;
is_primitive_type(f64) -> true;
is_primitive_type(f32) -> true;
is_primitive_type(u64) -> true;
is_primitive_type(u32) -> true;
is_primitive_type(u16) -> true;
is_primitive_type(u8) -> true;
is_primitive_type(i64) -> true;
is_primitive_type(i32) -> true;
is_primitive_type(i16) -> true;
is_primitive_type(i8) -> true;
is_primitive_type(_) -> false.

