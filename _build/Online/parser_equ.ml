type token =
  | VAR of (int)
  | LPAREN
  | RPAREN
  | EQU
  | DIS
  | IMPLY
  | AND
  | OR
  | NOT
  | EOF

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* EQU *);
  261 (* DIS *);
  262 (* IMPLY *);
  263 (* AND *);
  264 (* OR *);
  265 (* NOT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\003\000\002\000\003\000\003\000\003\000\
\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\011\000\000\000\
\003\000\000\000\000\000\000\000\005\000\000\000\000\000\000\000\
\001\000\009\000\010\000\004\000\000\000\007\000\000\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000"

let yysindex = "\255\255\
\022\000\000\000\012\255\006\255\006\255\000\000\000\000\001\000\
\000\000\008\255\010\255\017\255\000\000\006\255\006\255\006\255\
\000\000\000\000\000\000\000\000\254\254\000\000\011\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\000\000\000\010\000"

let yygindex = "\000\000\
\013\000\254\255\000\000"

let yytablesize = 287
let yytable = "\001\000\
\006\000\012\000\013\000\014\000\015\000\016\000\003\000\004\000\
\018\000\006\000\019\000\021\000\022\000\023\000\005\000\010\000\
\011\000\015\000\008\000\020\000\017\000\006\000\014\000\015\000\
\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\004\000\000\000\000\000\000\000\014\000\015\000\
\016\000\005\000\006\000\006\000\006\000\000\000\000\000\006\000\
\000\000\006\000\006\000\008\000\008\000\008\000\003\000\004\000\
\000\000\000\000\000\000\008\000\000\000\000\000\005\000"

let yycheck = "\001\000\
\000\000\004\000\005\000\006\001\007\001\008\001\001\001\002\001\
\001\001\000\000\001\001\014\000\015\000\016\000\009\001\004\001\
\005\001\007\001\000\000\003\001\008\000\000\000\006\001\007\001\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\255\255\255\255\255\255\006\001\007\001\
\008\001\009\001\001\001\002\001\003\001\255\255\255\255\006\001\
\255\255\008\001\009\001\001\001\002\001\003\001\001\001\002\001\
\255\255\255\255\255\255\009\001\255\255\255\255\009\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  EQU\000\
  DIS\000\
  IMPLY\000\
  AND\000\
  OR\000\
  NOT\000\
  EOF\000\
  "

let yynames_block = "\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : (int*int) Tseitin.formula) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (int*int) Tseitin.formula list) in
    Obj.repr(
# 33 "Online/parser_equ.mly"
                 ( _1::_2 )
# 162 "Online/parser_equ.ml"
               : (int*int) Tseitin.formula list))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "Online/parser_equ.mly"
         ( [] )
# 168 "Online/parser_equ.ml"
               : (int*int) Tseitin.formula list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 39 "Online/parser_equ.mly"
          ( Lit (_1,0) )
# 175 "Online/parser_equ.ml"
               : (int*int) Tseitin.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : (int*int) Tseitin.formula) in
    Obj.repr(
# 40 "Online/parser_equ.mly"
                      ( _2 )
# 182 "Online/parser_equ.ml"
               : (int*int) Tseitin.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (int*int) Tseitin.formula) in
    Obj.repr(
# 41 "Online/parser_equ.mly"
             ( Not (_2,0) )
# 189 "Online/parser_equ.ml"
               : (int*int) Tseitin.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : (int*int) Tseitin.formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (int*int) Tseitin.formula) in
    Obj.repr(
# 42 "Online/parser_equ.mly"
                 ( Or (_1, _3, 0) )
# 197 "Online/parser_equ.ml"
               : (int*int) Tseitin.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : (int*int) Tseitin.formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (int*int) Tseitin.formula) in
    Obj.repr(
# 43 "Online/parser_equ.mly"
                  ( And (_1, _3, 0) )
# 205 "Online/parser_equ.ml"
               : (int*int) Tseitin.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : (int*int) Tseitin.formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (int*int) Tseitin.formula) in
    Obj.repr(
# 44 "Online/parser_equ.mly"
                   ( Or (Not (_1,0), _3, 0) )
# 213 "Online/parser_equ.ml"
               : (int*int) Tseitin.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 48 "Online/parser_equ.mly"
                ( _1, _3 )
# 221 "Online/parser_equ.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 49 "Online/parser_equ.mly"
                ( -_1 - 1, -_3 - 1 )
# 229 "Online/parser_equ.ml"
               : 'atom))
(* Entry formula *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let formula (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (int*int) Tseitin.formula list)
