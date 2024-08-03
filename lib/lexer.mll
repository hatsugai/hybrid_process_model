{
open Parser
}

let space = [' ' '\t' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z']
let special = ['_']

rule token = parse
| space+
	{ token lexbuf }
| "//"
	{ line_comment lexbuf; token lexbuf }
| "/*"
    { block_comment lexbuf; token lexbuf }
| '\n'
	{ Lexing.new_line lexbuf; token lexbuf }
| "?"			{ QUESTION }
| "!"			{ EXCLAMATION }
| "&"           { AMPERSAND }
| "|"			{ VBAR }
| ";"			{ SEMICOLON }
| ":"			{ COLON }
| ","			{ COMMA }
| "."			{ PERIOD }
| "("			{ LPAR }
| ")"			{ RPAR }
| "{"    		{ LCUR }
| "}"			{ RCUR }
| "["			{ LBRA }
| "]"			{ RBRA }
| "+"			{ PLUS }
| "-"			{ MINUS }
| "*"			{ ASTERISK }
| "div"			{ DIV }
| "mod"			{ MOD }
| "not"			{ NOT }
| "&&"			{ AND }
| "||"			{ OR }
| "="			{ EQ }
| "!="			{ NE }
| "<"			{ LT }
| "<="			{ LE }
| ">"			{ GT }
| ">="			{ GE }
| "->"			{ ARROW }
| "=>"			{ DARROW }
| "++"          { RECORD_UPDATE }
| "let"         { LET }
| "fun"			{ FUN }
| "call"		{ CALL }
| "false"		{ FALSE }
| "true"		{ TRUE }
| "skip"		{ SKIP }
| ":="			{ BECOMES }
| "if"			{ IF }
| "then"		{ THEN }
| "else"		{ ELSE }
| "while"		{ WHILE }
| "do"			{ DO }
| "return"		{ RETURN }
| "break"		{ BREAK }
| "continue"	{ CONTINUE }
| "select"		{ SELECT }
| "end"			{ END }
| "const"       { CONST }
| "var"         { VAR }
| digit+
	{ INT (int_of_string (Lexing.lexeme lexbuf)) }
| (alpha | special) (alpha | special | digit)*
    { ID (Id.make (Lexing.lexeme lexbuf)) }
| eof
	{ EOF }
| _
	{ failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }

and block_comment = parse
| "\n"
	{ Lexing.new_line lexbuf; block_comment lexbuf }
| "/*"
	{ block_comment lexbuf; block_comment lexbuf }
| "*/"
    { () }
| eof
	{ () }
| _
	{ block_comment lexbuf }

and line_comment = parse
| "\n"
	{ Lexing.new_line lexbuf; () }
| eof
	{ () }
| _
	{ line_comment lexbuf }
