(**************************************************************************)
(*                                                                        *)
(* OCaml template Copyright (C) 2004-2010                                 *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(* Adaption to .dpt file parsing by Jonathan Kimmitt                      *)
(*  Copyright 2017 University of Cambridge                                *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

{
  open Lexing
  open Program

  let verbose = ref false
  let lincnt = ref 0

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k) Keywords.keywordlst;
    fun s -> Hashtbl.find h s

let lex_init () =
    lincnt := 0

let tok arg = arg

let discard token lexbuf = token lexbuf

let rec brackid cnt s =
    let len = String.length s in
        if len > 1 && s.[len-1] = '[' then brackid (cnt+1) (String.sub s 0 (len-1))
	else (cnt,s)

}

let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '-' '@' '[']*
let fltnum = ['0'-'9']+ '.' ['0'-'9']+ ['e']* ['+' '-']* ['0'-'9']* ['_']* ['0'-'9']+
let number = ['0'-'9']+
let unumber = '_' ['0'-'9']+
let space = [' ']+
let dashes = ['-']['-']['-']+
let newline = ['\n']
let qstring = '\''[^'\'']*'\''
let dblqstring = '"'[^'"']*'"'
let ampident = '&'[^' ']*
let lpident = '('['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* ' '
let identlp = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* '('
let identeq = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* '='
let identcln = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* ':' ' '
let comment = '!'[^'\n']*
let dotted = '.' ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* '.'
let ellipsis = '.''.'['.']*
let range = ['A'-'Z'] '-' ['A'-'Z'] ':'
let qq = '\'' '\'' '\'' '\''

rule token = parse
  | comment
      { discard token lexbuf }
  | space
      { discard token lexbuf }
  | newline
      { incr lincnt; discard token lexbuf }
  | range as r
      { tok ( RANGE r ) }
  | "(U- "
      { tok ( LPUMINUS ) }
  | "(U+ "
      { tok ( LPUPLUS ) }
  | fltnum as f
      { tok ( FLTNUM f ) }
  | number as n
      { tok ( INTNUM n ) }
  | unumber as s
      { tok ( UNUMBER s ) }
  | "_F."
      { tok ( FDOT ) }
  | "(// "
      { tok ( LPCONCAT ) }
  | ident as s
      { match brackid 0 s with
      	  | (0,_) -> tok ( try keyword s with Not_found -> NAME s )
	  | (1,s') -> tok ( BRACKID s' )
	  | (2,s') -> tok ( BRACKID2 s' )
	  | (_,_) -> failwith s }
  | dotted as s
      { tok ( try keyword s with Not_found -> DOTTED s ) }
  | ampident as s
      { tok ( try keyword s with Not_found -> HOLLERITH s ) }
  | lpident as s'
      { let s = String.sub s' 0 (String.length s' - 1) in
        let s' = String.sub s' 1 (String.length s' - 2) in
      	tok ( try keyword s with Not_found -> LPIDENT s' ) }
  | identlp as s
      { tok ( try keyword s with Not_found -> IDENTLP (String.sub s 0 (String.length s - 1))) }
  | identeq as s
      { tok ( try keyword s with Not_found -> IDENTEQ (String.sub s 0 (String.length s - 1))) }
  | identcln as s'
      { let s = String.sub s' 0 (String.length s' - 1) in 
      	tok ( try keyword s with Not_found -> IDENTCLN s ) }
  | qstring as s
      { tok ( HOLLERITH (String.sub s 1 (String.length s - 2))) }
  | dblqstring as s
      { tok ( HOLLERITH (String.sub s 1 (String.length s - 2))) }
  | qq as s
      { tok ( HOLLERITH (String.sub s 1 (String.length s - 2))) }
  | dashes
      { discard token lexbuf }
  | eof
      { tok ( EOF_TOKEN ) }
| "(**"
      { tok ( LPPOWER ) }
| "(/="
      { tok ( LPNOTEQUAL ) }
| '!'
{ tok ( PLING ) }

| '"'
{ tok ( DOUBLEQUOTE ) }

| '#'
{ tok ( HASH ) }

| '$'
{ tok ( DOLLAR ) }

| '%'
{ tok ( PERCENT ) }

| '&'
{ tok ( AMPERSAND ) }

| '''
{ tok ( QUOTE ) }

| '('
{ tok ( LPAR ) }

| '['
{ tok ( LBRACK ) }

| '{'
{ tok ( LBRACE ) }

| "(< "
{ tok ( LPLESS ) }

| ')'
{ tok ( RPAR ) }

| ']'
{ tok ( RBRACK ) }

| '}'
{ tok ( RBRACE ) }

| "(> "
{ tok ( LPGREATER ) }

| "(>= "
{ tok ( LPGTEQUAL ) }

| "(<= "
{ tok ( LPLTEQUAL ) }

| "(* "
{ tok ( LPSTAR ) }

| "(+ "
{ tok ( LPPLUS ) }

| "+"
{ tok ( PLUS ) }

| ','
{ tok ( COMMA ) }

| "(- "
{ tok ( LPMINUS ) }

| "-"
{ tok ( MINUS ) }

| '.'
{ tok ( DOT ) }

| "(/ "
{ tok ( LPSLASH ) }

| "(@ "
{ tok ( LPARRAY ) }

| "(% "
{ tok ( LPPERCENT ) }

| "@)"
{ tok ( ARRAYRP ) }

| '\\'
{ tok ( SLASHD ) }

| ':'
{ tok ( COLON ) }
(*
| ';'
{ tok ( SEMICOLON ) }
*)
| "(= "
{ tok ( LPEQUALS ) }

| '='
{ tok ( EQUALS ) }

| '?'
{ tok ( QUERY ) }

| '@'
{ tok ( AT ) }

| '/'
{ tok ( SLASH ) }

| '*'
{ tok ( STAR ) }

| '^'
{ tok ( CARET ) }

| '_'
{ tok ( UNDERSCORE ) }

| '`'
{ tok ( BACKQUOTE ) }

| '|'
{ tok ( VBAR ) }

| '~'
{ tok ( TILDE ) }

| _ as oth
{ tok ( failwith ("lex_file_lex: "^String.make 1 oth) ) }
