-module(e_compile_context).
-export_type([context/0]).
-include("e_record_definition.hrl").

-type context() :: #{
                     fn_map := #{atom() => #e_fn_type{}},
                     struct_map := #{atom() => #e_struct{}},
                     vars := #e_vars{},
                     wordsize := non_neg_integer()
                    }.
