syn region e_string		start=/"/ end=/"/ contains=e_char_modifier
syn region e_char		start=/'/ end=/'/ contains=e_char_modifier
syn match e_char_modifier	"\\(n|r|t|)"
syn match e_integer		"\<\d\+\([Ee]\d\+\)\?\>"
syn match e_integer		"\<0x\x\+\>"
syn match e_float		"\<\d\+\.\d*\([Ee][-+]\d\+\)\?\>"
syn match e_float		"\<\.\d\+\([Ee][-+]\d\+\)\?\>"
syn match e_float		"\<\d\+[Ee][-+]\d\+\>"
syn match e_label		"^\s*@\s*@\s*[A-Za-z_]\+\>"
syn match e_comment		"%.*$"

syn region e_pre		start="^\s*\zs\%(%:\|#\)\s*\%(if\|ifdef\|ifndef\|else\|elif\|endif\|define\|undef\|error\|warning\|include\)\>" skip="\\$" end="$" keepend contains=e_comment,e_string,e_char,e_integer,e_float
syn match e_macro_ref		"?[A-Za-z_]\+\>"

syn keyword e_keyword		struct fn end if then else elif while do goto sizeof alignof typeof return as
syn keyword e_type		byte word float any void
syn keyword e_operator		band bor bxor bnot and or not rem
syn match e_operator		"==\|!=\|>=\|<=\|>\|<\|@\|\^\|+\|-\|*\|/\|="

hi def link e_comment		Comment
hi def link e_label		Label
hi def link e_string		String
hi def link e_char		Character
hi def link e_struct		Structure
hi def link e_function		Function
hi def link e_pre		PreProc
hi def link e_macro_ref		PreProc
hi def link e_type		Type
hi def link e_keyword		Statement
hi def link e_operator		Operator
hi def link e_integer		Ingeger
hi def link e_float		Float
hi def link Ingeger		Number
hi def link Float		Number

