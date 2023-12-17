module AST

// Type used for translation
type TranslationUnit = {word: string; translate : bool; rhyme: bool}

// AST for Stochastic Parody
type Grammar =
| Sentiment of string
| Keywords of string List
| Line of TranslationUnit List
| Section of string * (Grammar List)
| Section_Instance of string

