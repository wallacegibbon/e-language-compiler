syn case match

syn match e_comment		"%.*$"
syn region e_string		start=/"/ end=/"/ contains=e_string_modifier
syn match e_string_modifier	"\\(n|r|t|)"
syn match e_integer		"\<\d\+\([Ee]\d\+\)\?\>"
syn match e_integer		"\<0x\x\+\>"
syn match e_float		"\<\d\+\.\d*\([Ee][-+]\d\+\)\?\>"
syn match e_float		"\<\.\d\+\([Ee][-+]\d\+\)\?\>"
syn match e_float		"\<\d\+[Ee][-+]\d\+\>"
syn match e_label		"^@@.*:"

syn keyword	e_keyword fun end if then else elif while do goto sizeof typeof struct return as
syn keyword	e_type usize isize u8 i8 u16 i16 u32 i32 u64 i64 float64 float32 any
syn keyword	e_operator band bor bxor bnot and or not rem
syn match	e_operator "==\|!=\|>=\|<=\|>\|<\|@\|\^\|+\|-\|*\|/\|="

hi def link e_comment		Comment
hi def link e_string		String

hi def link e_type		Type
hi def link e_keyword		Statement
hi def link e_operator		Operator
hi def link e_integer		Ingeger
hi def link e_float		Float
hi def link Ingeger		Number
hi def link Float		Number
hi def link e_label		Label
