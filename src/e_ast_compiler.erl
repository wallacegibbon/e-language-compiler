-module(e_ast_compiler).
-export([compile_from_raw_ast/2]).
-export_type([ast_compile_result/0]).
-include("e_record_definition.hrl").

-type ast_compile_result() :: {{e_ast(), e_ast()}, #e_vars{}}.

-spec compile_from_raw_ast(e_ast_raw(), e_compile_option:option()) -> ast_compile_result().
compile_from_raw_ast(AST, #{wordsize := WordSize, entry_function := Entry}) ->
  {GlobalVars00, AST00, InitCode00} = e_var:fetch_vars(AST),
  {FnTypeMap00, StructMap00} = e_util:make_function_and_struct_map_from_ast(AST00),
  ensure_function_exist(Entry, FnTypeMap00),

  %% Once the AST got constructed, we check the recursive definition problem of struct.
  e_struct:check_struct_recursion_in_map(StructMap00),

  Ctx00 = #{vars => GlobalVars00, fn_map => FnTypeMap00,
            struct_map => StructMap00, wordsize => WordSize},

  %% type checking
  e_type:check_types_in_ast(AST00, Ctx00),
  e_type:check_type_in_stmts(InitCode00, Ctx00),

  %% transform all `a[b]` into `(a + b * sizeof(a^))^`
  AST02 = e_array:transform_aref_in_ast(AST00, Ctx00),
  InitCode02 = e_array:transform_aref_in_stmts(InitCode00, Ctx00),
  {_, StructMap02} = e_util:make_function_and_struct_map_from_ast(AST02),
  Ctx02 = Ctx00#{struct_map := StructMap02},

  %% Fill offset of variables and struct fields.
  AST10 = e_size:fill_offsets_in_ast(AST02, Ctx02),
  %% Struct info is updated, so StructMap needs to be updated, too.
  {_, StructMap10} = e_util:make_function_and_struct_map_from_ast(AST10),
  Ctx10 = Ctx02#{struct_map := StructMap10},

  GlobalVars10 = e_size:fill_offsets_in_vars(GlobalVars00, Ctx10),
  GlobalVars20 = e_var:shift_offset_middle(GlobalVars10),
  Ctx11 = Ctx10#{vars := GlobalVars20},

  %% Expand expressions like `sizeof` and `alignof`.
  AST20 = e_size:expand_kw_in_ast(AST10, Ctx11),
  %% Initializing code for global variables are not in main ast, do not forget them.
  InitCode20 = e_size:expand_kw_in_stmts(InitCode02, Ctx11),
  %% sizeof expressions are expanded, so StructMap needs to be updated
  {_, StructMap20} = e_util:make_function_and_struct_map_from_ast(AST20),
  Ctx20 = Ctx11#{struct_map := StructMap20},

  %% expand init exprs like A{a = 1} and {1, 2, 3}
  AST30 = e_init_expr:expand_in_ast(AST20, Ctx20),
  InitCode30 = e_init_expr:expand_in_stmts(InitCode20, Ctx20),

  %% fix the pointer size. (Which was filled with `0`)
  AST40 = e_pointer:fix_pointer_size_in_ast(AST30, Ctx20),
  InitCode40 = e_pointer:fix_pointer_size_in_stmts(InitCode30, Ctx20),

  %% convert `.` into `@`, `+` and `^`
  AST70 = e_struct:eliminate_dot_in_ast(AST40, Ctx20),
  InitCode70 = e_struct:eliminate_dot_in_stmts(InitCode40, Ctx20),

  AST80 = e_varref:varref_to_offset_in_ast(AST70, Ctx20),
  InitCode80 = e_varref:varref_to_offset_in_stmts(InitCode70, Ctx20),

  #{vars := GlobalVars} = Ctx20,
  {{InitCode80, AST80}, GlobalVars}.

ensure_function_exist(FnName, FnTypeMap) ->
  case maps:find(FnName, FnTypeMap) of
    {ok, _} ->
      ok;
    _ ->
      e_util:ethrow({"", 0, 0}, "entry function \"~s\" is not defined", [FnName])
  end.
