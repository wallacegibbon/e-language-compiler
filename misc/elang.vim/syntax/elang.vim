syn region e_string		start=/"/ end=/"/ contains=e_char_modifier
syn region e_char		start=/'/ end=/'/ contains=e_char_modifier
syn match e_char_modifier	"\\(n|r|t|)"
syn match e_label		"^\s*@\s*@\s*[A-Za-z0-9_]\+\>"
syn match e_integer		"\<0x[0-9A-Fa-f_]\+\>"
syn match e_integer		"\<0o[0-7_]\+\>"
syn match e_integer		"\<0b[01_]\+\>"
syn match e_float		"\<\d\+\.\d*\([Ee][-+]\?\d\+\)\?\>"
syn match e_float		"\<\.\d\+\([Ee][-+]\?\d\+\)\?\>"
syn match e_float		"\<\d\+\([Ee]\d\+\)\?\>"
syn match e_float		"\<\d\+[Ee][-+]\?\d\+\>"
syn match e_comment		"%.*$"
syn match e_identifier		"[A-Za-z_][A-Za-z0-9_]*"
syn match e_function		/[A-Za-z0-9_]*\s*(/me=e-1
syn match e_struct_field	/\.[A-Za-z0-9_]\+/
syn match e_macro_def		/#define\s\+[A-Za-z0-9_]\+\s\+/
syn match e_macro_ref		/?[A-Za-z0-9_]\+/

syn keyword e_keyword		fn struct end if then else elif while do goto sizeof alignof return interrupt
syn keyword e_operator		band bor bxor bnot and or not rem bsl bsr
syn match e_operator		"==\|!=\|>=\|<=\|>\|<\|@\|\^\|+\|-\|*\|/\|=\|\[\|\]\|{\|}"
syn keyword e_type		byte word float any void

syn match e_type_complex	/:\s*[A-Za-z_][A-Za-z0-9_]*[\^]*\s*[;)=}]/ms=s+1,me=e-1
syn match e_array_type		/:\s*{[^}]*,\s*\d\+}/ms=s+1
syn match e_typecast		/\<as\s\+(\s*[A-Za-z_][A-Za-z0-9_]*[\^]*\s*)/

hi def link e_identifier	Identifier
hi def link e_struct_field	Number
hi def link e_comment		Comment
hi def link e_label		Label
hi def link e_string		String
hi def link e_char		Character
hi def link e_struct		Structure
hi def link e_function		Function
hi def link e_macro_def		PreProc
hi def link e_macro_ref		PreProc
hi def link e_type		Type
hi def link e_type_complex	Type
hi def link e_array_type	Type
hi def link e_typecast		Type
hi def link e_keyword		Keyword
hi def link e_operator		Operator
hi def link e_integer		Ingeger
hi def link e_float		Float
hi def link Ingeger		Number
hi def link Float		Number

hi def link e_keyword		Statement
hi def link e_operator		Operator
hi def link e_integer		Ingeger
hi def link e_float		Float
hi def link Ingeger		Number
hi def link Float		Number

