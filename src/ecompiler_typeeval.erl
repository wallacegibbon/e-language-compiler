%%% type checking should have been done before calling functions in this module
-module(ecompiler_typeeval).

-export([typeof_expr/3]).

-import(ecompiler_utils, [flat_format/2]).

-include("./ecompiler_frame.hrl").

typeof_expr(#op1{operator='^', operand=Operand, line=Line}, Vars, Structs) ->
    case typeof_expr(Operand, Vars, Structs) of
	#basic_type{type={_, PDepth}} = T when PDepth > 0 ->
	    decr_pdepth(T);
	_ ->
	    throw({Line, flat_format("invalid operator \"^\" on operand ~p",
				     [Operand])})
    end;
typeof_expr(#op1{operator='@', operand=Operand, line=Line}, Vars, Structs) ->
    case Operand of
	#varref{name=_} ->
	    incr_pdepth(typeof_expr(Operand, Vars, Structs));
	#op2{operator='.', op1=Op1, op2=Op2} ->
	    typeof_structfield(typeof_expr(Op1, Vars, Structs), Op2, Structs);
	_ ->
	    throw({Line, flat_format("invalid operator \"@\" on operand ~p",
				     [Operand])})
    end;
typeof_expr(#op2{operator=assign, op2=Op2}, Vars, Structs) ->
    typeof_expr(Op2, Vars, Structs);
typeof_expr(#op2{op1=Op1}, Vars, Structs) ->
    typeof_expr(Op1, Vars, Structs);
typeof_expr(#varref{name=Name}, Vars, _) ->
    maps:get(Name, Vars).

incr_pdepth(#basic_type{type={Tname, Pdepth}} = Type) ->
    Type#basic_type{type={Tname, Pdepth + 1}}.

decr_pdepth(#basic_type{type={Tname, Pdepth}} = Type) ->
    Type#basic_type{type={Tname, Pdepth - 1}}.

typeof_structfield(#basic_type{type=StructName}, FieldName, Structs)
  when is_atom(StructName) ->
    todo;
typeof_structfield(AnyType, _, _) ->
    throw({element(2, AnyType), "operator \".\" is for struct only"}).

