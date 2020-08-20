-module(ecompiler_type).

-export([checktype_ast/2, typeof_expr/2]).

-import(ecompiler_utils, [is_primitive_type/1, is_integer_type/1,
			  expr2str/1, flat_format/2]).

-include("./ecompiler_frame.hrl").

checktype_ast({FunMap, StructMap}, GlobalVarTypes) ->
    lists:map(fun (S) ->
		      checktype_struct(S, StructMap)
	      end, maps:values(StructMap)),
    lists:map(fun (F) ->
		      checktype_function(F, FunMap, StructMap, GlobalVarTypes)
	      end, maps:values(FunMap)),
    ok.

checktype_function(#function{var_types=VarTypes, exprs=Exprs, type=Fntype},
		   Functions, Structs, GlobalVarTypes) ->
    %% TODO: check param default values (only const expressions are allowed)
    lists:map(fun (T) -> checktype_type(T, Structs) end, maps:values(VarTypes)),
    checktype_type(Fntype#fun_type.ret, Structs),
    CurrentVars = maps:merge(GlobalVarTypes, VarTypes),
    Ctx = {CurrentVars, Functions, Structs, Fntype#fun_type.ret},
    typeof_exprs(Exprs, Ctx).

checktype_struct(#struct{field_types=FieldTypes, name=Name}, Structs) ->
    %% TODO: check init code (only const expressions are allowed
    lists:map(fun (T) -> checktype_type(T, Structs) end,
	      maps:values(FieldTypes)),
    Conflicts = lists:filter(fun ({_, #basic_type{type={Tname, 0}}}) ->
				     Name =:= Tname;
				 (_) ->
				     false
			     end, maps:to_list(FieldTypes)),
    case Conflicts of
	[{_, #basic_type{line=Line}} | _] ->
	    throw({Line, "recursive definition is invalid"});
	_ ->
	    ok
    end.

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
	    throw({Line, flat_format("type mismatch in \"~s = ~s\"",
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
    EInfo = flat_format("type mismatch in \"~s ~s ~s\"",
			[fmt_type(TypeofOp1), Operator, fmt_type(TypeofOp2)]),
    %% pointer + integer is valid
    case compare_type(TypeofOp1, TypeofOp2) of
	false ->
	    if Operator =:= '+' ->
		   case is_pointer_and_int(TypeofOp1, TypeofOp2) of
		       {true, Ptype} ->
			   Ptype;
		       false ->
			   throw({Line, EInfo})
		   end;
	       true ->
		   throw({Line, EInfo})
	    end;
	true ->
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
typeof_expr(#varref{name=Name, line=Line},
	    {VarTypes, Functions, Structs, _}) ->
    Type = case maps:find(Name, VarTypes) of
	       error ->
		   case maps:find(Name, Functions) of
		       error -> throw({Line, flat_format("~s is undefined",
							 [Name])});
		       {ok, T} ->
			   T#function.type
		   end;
	       {ok, T} ->
		   T
	   end,
    checktype_type(Type, Structs),
    Type;
typeof_expr({float, Line, _}, _) ->
    #basic_type{type={f64, 0}, line=Line};
typeof_expr({integer, Line, _}, _) ->
    #basic_type{type={i64, 0}, line=Line};
typeof_expr({string, Line, _}, _) ->
    #basic_type{type={i8, 1}, line=Line}.

incr_pdepth(#basic_type{type={Tname, Pdepth}} = Type) ->
    Type#basic_type{type={Tname, Pdepth + 1}};
incr_pdepth(#array_type{elemtype=#basic_type{type={Type, N}, line=Line}}) ->
    #basic_type{type={Type, N + 1}, line=Line}.

decr_pdepth(#basic_type{type={Tname, Pdepth}} = Type) ->
    Type#basic_type{type={Tname, Pdepth - 1}};
decr_pdepth(#array_type{line=Line} = Type) ->
    throw({Line, flat_format("pointer - on array type ~s is invalid",
			     [fmt_type(Type)])}).

typeof_structfield(#basic_type{type={StructName, 0}}, #varref{name=FieldName},
		   Structs, Line) ->
    case maps:find(StructName, Structs) of
	{ok, S} ->
	    case maps:find(FieldName, S#struct.field_types) of
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
compare_type(#array_type{elemtype=E1, size=S1},
	     #array_type{elemtype=E2, size=S2}) ->
    compare_type(E1, E2) and (S1 =:= S2);
compare_type(#basic_type{type={T1, 0}}, #basic_type{type={T2, 0}}) ->
    (T1 =:= T2) orelse (is_integer_type(T1) and is_integer_type(T2));
compare_type(#basic_type{type=T1}, #basic_type{type=T2}) ->
    T1 =:= T2;
compare_type(_, _) ->
    false.

is_pointer_and_int(#basic_type{type={_, N}} = O, #basic_type{type={T, 0}})
  when N > 0 ->
    {is_integer_type(T), O};
is_pointer_and_int(#basic_type{type={T, 0}}, #basic_type{type={_, N}} = O)
  when N > 0 ->
    {is_integer_type(T), O};
is_pointer_and_int(_, _) ->
    false.

%% check type, ensure that all struct used by type exists.
checktype_type(#fun_type{params=Params, ret=Rettype}, Structs) ->
    lists:map(fun (P) ->
		      checktype_type(P, Structs)
	      end, Params),
    checktype_type(Rettype, Structs);
checktype_type(#array_type{elemtype=Elemtype}, Structs) ->
    case Elemtype of
	#array_type{line=Line} ->
	    throw({Line, "nested array is not supported"});
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
fmt_type(#array_type{elemtype=Type, size=N}) ->
    io_lib:format("<~s, ~w>", [Type, N]);
fmt_type(#basic_type{type={Type, Depth}}) when Depth > 0 ->
    io_lib:format("(~s~s)", [Type, lists:duplicate(Depth, "^")]);
fmt_type(#basic_type{type={Type, 0}}) ->
    atom_to_list(Type).

