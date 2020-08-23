-module(ecompiler_expandinit).

-export([expand_initexpr_infun/2]).

-import(ecompiler_utils, [flat_format/2]).

-include("./ecompiler_frame.hrl").

expand_initexpr_infun([#function{exprs=Exprs} = F | Rest], StructMap) ->
    [F#function{exprs=expand_init(Exprs, [], {StructMap})} |
     expand_initexpr_infun(Rest, StructMap)];
expand_initexpr_infun([Any | Rest], StructMap) ->
    [Any | expand_initexpr_infun(Rest, StructMap)];
expand_initexpr_infun([], _) ->
    [].

-define(ASSIGN(Op1, Op2), #op2{operator=assign, op1=Op1, op2=Op2}).

expand_init([?ASSIGN(Op1, #struct_init{name=Name, field_values=FieldValues,
				       line=Line}) |
	     Rest], NewAst, {Structs} = Ctx) ->
    case maps:find(Name, Structs) of
	{ok, #struct{field_names=FieldNames,
		     field_defaults=FieldDefaults}} ->
	    FieldValueMap = maps:merge(FieldDefaults, FieldValues),
	    C = structinit_to_op(Op1, FieldNames, FieldValueMap, []),
	    expand_init(Rest, C ++ NewAst, Ctx);
	error ->
	    throw({Line, flat_format("struct ~s is not found", [Name])})
    end;
expand_init([?ASSIGN(Op1, #array_init{elements=Elements, line=Line}) | Rest],
	    NewAst, Ctx) ->
    C = arrayinit_to_op(Op1, Elements, 0, Line, []),
    expand_init(Rest, C ++ NewAst, Ctx);
expand_init([Any | Rest], NewAst, Ctx) ->
    expand_init(Rest, [Any | NewAst], Ctx);
expand_init([], NewAst, _) ->
    lists:reverse(NewAst).

structinit_to_op(Var, [#varref{line=Line, name=Fname} = Field | Rest],
		 FieldInitMap, Newcode) ->
    InitCode = case maps:find(Fname, FieldInitMap) of
		   {ok, InitOp} ->
		       InitOp;
		   error ->
		       0
	       end,
    Op = #op2{operator=assign, op2=InitCode, line=Line,
	      op1=#op2{operator='.', op1=Var, op2=Field, line=Line}},
    structinit_to_op(Var, Rest, FieldInitMap, [Op | Newcode]);
structinit_to_op(_, [], _, Newcode) ->
    Newcode.

arrayinit_to_op(Var, [E | Rest], Cnt, Line, Newcode) ->
    Op = #op2{operator=assign, op2=E, line=Line,
	      op1=#op1{operator='^', line=Line,
		       operand=#op2{operator='+', op2=Cnt, line=Line,
				    op1=#op1{operator='@', operand=Var,
					     line=Line}}}},
    arrayinit_to_op(Var, Rest, Cnt + 1, Line, [Op | Newcode]);
arrayinit_to_op(_, [], _, _, Newcode) ->
    Newcode.

