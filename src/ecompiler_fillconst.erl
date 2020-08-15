-module(ecompiler_fillconst).

-export([parse_and_remove_const/1]).

-import(ecompiler_utils, [flat_format/2]).

-include("./ecompiler_frame.hrl").

%% find all consts in AST, calculate it and replace all const references.
parse_and_remove_const(Ast) ->
    {Constants, NewAst} = fetch_constants(Ast),
    replace_constants(NewAst, Constants).

%% fetch constants
fetch_constants(Ast) -> fetch_constants(Ast, [], #{}).

fetch_constants([#const{name=Name, val=Expr} | Rest], Statements, Constants) ->
    fetch_constants(Rest, Statements,
		    Constants#{Name => eval_constexpr(Expr, Constants)});
fetch_constants([Any | Rest], Statements, Constants) ->
    fetch_constants(Rest, [Any | Statements], Constants);
fetch_constants([], Statements, Constants) ->
    {Constants, lists:reverse(Statements)}.

eval_constexpr(#op2{operator='+', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) + eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='-', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) - eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='*', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) * eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='/', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) / eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='rem', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) rem eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='band', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) band eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='bor', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) bor eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='bxor', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) bxor eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='bsr', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) bsr eval_constexpr(Op2, Constants);
eval_constexpr(#op2{operator='bsl', op1=Op1, op2=Op2}, Constants) ->
    eval_constexpr(Op1, Constants) bsl eval_constexpr(Op2, Constants);
eval_constexpr(#varref{name=Name, line=Line}, Constants) ->
    try maps:get(Name, Constants)
    catch
	error:{badkey, _} ->
	    throw({Line, flat_format("undefined constant ~s", [Name])})
    end;
eval_constexpr({ImmiType, _, Val}, _) when ImmiType =:= integer;
					   ImmiType =:= float ->
    Val;
eval_constexpr(Num, _) when is_integer(Num); is_float(Num) ->
    Num;
eval_constexpr({Any, Line, Val}, _) ->
    E = flat_format("invalid const expression: ~s, ~p", [Any, Val]),
    throw({Line, E}).

%% replace constants in AST
replace_constants([#function{params=Params, exprs=Exprs} = Fn | Rest],
		  Constants) ->
    [Fn#function{params=replace_constants_inexprs(Params, Constants),
		 exprs=replace_constants_inexprs(Exprs, Constants)}
     | replace_constants(Rest, Constants)];
replace_constants([#struct{fields=Fields} = S | Rest], Constants) ->
    [S#struct{fields=replace_constants_inexprs(Fields, Constants)}
     | replace_constants(Rest, Constants)];
replace_constants([#vardef{initval=Initval} = V | Rest], Constants) ->
    [V#vardef{initval=replace_constants_inexpr(Initval, Constants)}
     | replace_constants(Rest, Constants)];
replace_constants([], _) ->
    [].

replace_constants_inexprs([Expr | Rest], Constants) ->
    [replace_constants_inexpr(Expr, Constants) |
     replace_constants_inexprs(Rest, Constants)];
replace_constants_inexprs([], _) ->
    [].

replace_constants_inexpr(#if_expr{condition=Condition, then=Then,
				  else=Else} = Expr,
			 Constants) ->
    Expr#if_expr{condition=replace_constants_inexpr(Condition, Constants),
		 then=replace_constants_inexprs(Then, Constants),
		 else=replace_constants_inexprs(Else, Constants)};
replace_constants_inexpr(#while_expr{condition=Condition, exprs=Exprs} = Expr,
			 Constants) ->
    Expr#while_expr{condition=replace_constants_inexpr(Condition, Constants),
		    exprs=replace_constants_inexprs(Exprs, Constants)};
replace_constants_inexpr(#op2{op1=Op1, op2=Op2} = Expr, Constants) ->
    Expr#op2{op1=replace_constants_inexpr(Op1, Constants),
	     op2=replace_constants_inexpr(Op2, Constants)};
replace_constants_inexpr(#op1{operand=Operand} = Expr, Constants) ->
    Expr#op1{operand=replace_constants_inexpr(Operand, Constants)};
replace_constants_inexpr(#call{args=Args} = Expr, Constants) ->
    Expr#call{args=replace_constants_inexprs(Args, Constants)};
replace_constants_inexpr(#return{expr=RetExpr} = Expr, Constants) ->
    Expr#return{expr=replace_constants_inexpr(RetExpr, Constants)};
replace_constants_inexpr(#vardef{name=Name, initval=Initval, type=Type,
				 line=Line} = Expr, Constants) ->
    case maps:find(Name, Constants) of
	{ok, _} ->
	    throw({Line, flat_format("name ~s is conflict with const",
				     [Name])});
	error ->
	    Expr#vardef{initval=replace_constants_inexpr(Initval, Constants),
			type=replace_constants_intype(Type, Constants)}
    end;
replace_constants_inexpr(#varref{name=Name, line=Line} = Expr, Constants) ->
    case maps:find(Name, Constants) of
	{ok, Val} ->
	    constnum_to_token(Val, Line);
	error ->
	    Expr
    end;
replace_constants_inexpr({constref, Line, Name}, Constants) ->
    case maps:find(Name, Constants) of
	{ok, Val} ->
	    constnum_to_token(Val, Line);
	error ->
	    throw({Line, flat_format("const ~s is not found", [Name])})
    end;
replace_constants_inexpr(Any, _) ->
    Any.

constnum_to_token(Num, Line) when is_float(Num) ->
    #float{val=Num, line=Line};
constnum_to_token(Num, Line) when is_integer(Num) ->
    #integer{val=Num, line=Line}.

replace_constants_intype(#box_type{elemtype=ElementType, size=Size} = T,
			 Constants) ->
    T#box_type{elemtype=replace_constants_intype(ElementType, Constants),
	       size=eval_constexpr(replace_constants_inexpr(Size, Constants),
				   Constants)};
replace_constants_intype(Any, _) ->
    Any.

