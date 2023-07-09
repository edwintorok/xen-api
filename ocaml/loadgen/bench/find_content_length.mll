let line = [^'\r']*"\r\n"
let digit = ['0'-'9']
let nonempty_line = ([^'\r'] line)
let ows = [' ''\t']*

let content_length = ['C''c']['O''o']['N''n']['T''t']['E''e']['N''n']['T''t']['-']['L''l']['E''e']['N''n']['G''g']['T''t']['H''h']

rule status_line = shortest
	| "HTTP/1.1 " (digit digit digit as status_code) " " line {
		(* this allocates, but the alternative is to use lexeme_start/lexeme_end, which only works when
			with_positions:true, which in turn would allocate
		 *)
		int_of_string status_code
	}

and content_length_header = shortest
	| nonempty_line* content_length ":" ows (digit+ as content_length_value) ows "\r\n" { int_of_string content_length_value }

and ignore_headers = shortest
	| _* "\r\n\r\n" eof { lexbuf.Lexing.lex_curr_pos }
