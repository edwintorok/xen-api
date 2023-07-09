rule line = shortest
	| _* '\r''\n' { Lexing.lexeme_end lexbuf  }
	| _* eof { 0 }
