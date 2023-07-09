{
	let digit lexbuf pos =
		let c = Lexing.lexeme_char lexbuf (pos - lexbuf.Lexing.lex_start_pos) in
		let n = Char.code c - Char.code '0' in
		assert (n >= 0 && n < 10);
		n

	let rec number lexbuf ~finish ~start acc =
		if start < finish then
			number lexbuf ~finish ~start:(start+1) @@ (acc*10) + digit lexbuf start
		else acc
}


let line = [^'\r']*"\r\n"
let digit = ['0'-'9']
let nonempty_line = ([^'\r'] line)
let ows = [' ''\t']*

let content_length = ['C''c']['O''o']['N''n']['T''t']['E''e']['N''n']['T''t']['-']['L''l']['E''e']['N''n']['G''g']['T''t']['H''h']

rule status_line = shortest
	| "HTTP/1.1 " digit digit digit " " line {
		(* this allocates, but the alternative is to use lexeme_start/lexeme_end, which only works when
			with_positions:true, which in turn would allocate.
			so use the internal lexer positions
		 *)
		let pos = lexbuf.Lexing.lex_start_pos + 9 in
		number lexbuf ~finish:(pos + 3) ~start:pos 0
	}

and content_length_header = parse
	| nonempty_line* content_length ":" ows { content_length_value lexbuf.Lexing.lex_curr_pos lexbuf }

and content_length_value start = parse
	| digit+ {
	 let len = number lexbuf ~start ~finish:lexbuf.Lexing.lex_curr_pos 0 in
	 content_length_eol len lexbuf
}

and content_length_eol len = shortest
	| ows "\r\n" { len }

and ignore_headers = shortest
	| _* "\r\n\r\n" eof { lexbuf.Lexing.lex_curr_pos }
