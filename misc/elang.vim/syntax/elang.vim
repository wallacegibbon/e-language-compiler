syn case match

syn match eComment		"%.*$"
syn region eString		start=/"/ end=/"/ contains=eStringModifier
syn match eStringModifier	"\\(n|r|t|)"
syn match eInteger		"\<\d\+\([Ee]\d\+\)\?\>"
syn match eInteger		"\<0x\x\+\>"
syn match eFloat		"\<\d\+\.\d*\([Ee][-+]\d\+\)\?\>"
syn match eFloat		"\<\.\d\+\([Ee][-+]\d\+\)\?\>"
syn match eFloat		"\<\d\+[Ee][-+]\d\+\>"
syn match eLabel		"^@@.*:"

syn keyword eKeyword fun end if else elif while goto sizeof typeof struct
syn keyword eType usize isize u8 i8 u16 i16 u32 i32 u64 i64 float64 float32 any
syn match eOperator "==\|!=\|>=\|<=\|>\|<\|@\|\^\|+\|-\|*\|/\|="
syn keyword eOperator band bor bxor bnot and or not rem

hi def link eComment Comment
hi def link eString String

hi def link eType Type
hi def link eKeyword Statement
hi def link eOperator Operator
hi def link eInteger Ingeger
hi def link eFloat Float
hi def link Ingeger Number
hi def link Float Number
hi def link eLabel Label
