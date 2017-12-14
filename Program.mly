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

%{
  open Parsing
  let l l = TLIST (List.rev l)
  let merge l = if List.length l == 1 then List.hd l else TLIST (List.rev l)
  let unumcnv s k = try Int64.of_string ("-"^s) with _ -> print_endline (s^":"^k); Int64.one
%}

%token  ACCEPT
%token  AMPERSAND
%token <string> ASSOC
%token  AT
%token  BACKQUOTE
%token  BACKSLASH
%token <string> BRACKID
%token <string> BRACKID2
%token  CARET
%token  CASE
%token  DASHES
%token  DISCARD
%token  DEFAULT
%token  DEFERRED
%token  DOLLAR
%token  DOT
%token  DOUBLEQUOTE
%token  ELLIPSIS
%token  EMPTY_TOKEN
%token  EOF_TOKEN
%token  ERR
%token  ERROR
%token  ERROR_TOKEN
%token  FDOT
%token  FLUSH
%token  HASH
%token  INDENT
%token  LINEFEED
%token  PERCENT
%token  PPC
%token  PLING
%token  QUERY
%token  QUOTE
%token <string> DOTTED
%token <string> RANGE
%token  RBRACE
%token  RBRACK
%token  ARITHIF
%token  ASGOTO
%token  AUTOMATIC
%token  BACKSPACE
%token  BITCON
%token  BYTE
%token  COLON
%token <string*string> CONCAT
%token  COMMA
%token <string list> COMMENT
%token  COMMON
%token  COMPGOTO
%token  COMPLEX
%token  CONTINUE
%token  CURRENCY
%token  DATA
%token  DATASTAR
%token  DCOMPLEX
%token <float> DCON
%token  DOUBLE
%token  ELSEIF
%token  ENDDO
%token  ENDFILE
%token  EOS
%token  EQ
%token  EQUALS
%token  EQUIV
%token  FALSE
%token  FIELD
%token  FILE
%token  FORM
%token  FORMAT
%token  FORMATTED
%token  GE
%token  GO
%token  GOTO
%token  GT
%token  HEXCON
%token <string> HOLLERITH
%token <string> FLTNUM
%token <string> INTNUM
%token <string> IDENTLP
%token <string> IDENTEQ
%token <string> IDENTCLN
%token <int*string> INT
%token <int64*string> INT64
%token <float*string> FLT
%token <int> POP
%token <string> LABNUM
%token <(int, int) Hashtbl.t> LABHASH
%token <int> SPACE
%token  IMPLICIT
%token  IN_COMMON
%token  INCLUDE
%token  INQUIRE
%token  IOSTAT
%token  LE
%token  LEN
%token  LET
%token  LBRACE
%token  LBRACK
%token <string> LPIDENT
%token  LPAND
%token  LPAR
%token  LPARG
%token  LPARRAY
%token  LPBACK
%token  LPCHARACTER
%token  LPCOMPLEX
%token  LPCONCAT
%token  LPDERIVED
%token  LPDIM
%token  LPEL
%token  LPEQUALS
%token  LPEQV
%token  LPGREATER
%token  LPGTEQUAL
%token  LPINTEGER
%token  LPKEY
%token  LPKIND
%token  LPLABEL
%token  LPLESS
%token  LPLOGICAL
%token  LPLTEQUAL
%token  LPMASK
%token  LPMINUS
%token  LPMODULE
%token  LPNAMELIST
%token  LPNEQV
%token  LPNOT
%token  LPNOTEQUAL
%token  LPOR
%token  LPPADDING
%token  LPPARAMETER
%token  LPPARENS
%token  LPPERCENT
%token  LPPLUS
%token  LPPOS
%token  LPPOWER
%token  LPPROCEDURE
%token  LPPROGRAM
%token  LPREAL
%token  LPSLASH
%token  LPSTAR
%token  LPTHANDLE
%token  LPTRIM
%token  LPUMINUS
%token  LPUNKNOWN
%token  LPUPLUS
%token  LPVARIABLE
%token  LT
%token  MINUS
%token <string> NAME
%token <string> UNUMBER
%token  NE
%token  NONE
%token  OCTCON
%token  OPEN
%token  OPTIONAL
%token  PAUSE
%token  POWER
%token  PLUS
%token  PRINT
%token  PUBLIC
%token  PUNCH
%token <float> RCON
%token  READ
%token  READ_
%token  REC
%token  RECL
%token  RECURSIVE
%token  REWIND
%token  RPAR
%token  SAVE
%token  SELECT
%token  SCALE
%token  SLASH
%token  ARRAYRP
%token  SLASHRP
%token  SLASHD
%token  STAR
%token  STATIC
%token  STATUS
%token  STOP
%token  THEN
%token  TO
%token  TRUE
%token  UNDEFINED
%token  WHILE
%token  TILDE
%token <token array> TARRAY
%token <token list> TLIST
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9
%token <token*token*token*token*token*token*token*token*token*token> TUPLE10
%token <token*token*token*token*token*token*token*token*token*token*token> TUPLE11
%token <token*token*token*token*token*token*token*token*token*token*token*token> TUPLE12
%token <token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE13
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE14
%token UNDERSCORE
%token UNINDENT
%token VBAR
%token A
%token ABSTRACT
%token ACCESS
%token ACTION
%token ADVANCE
%token All
%token ALLOCATABLE
%token ALLOCATE
%token Allocating
%token Ambiguous
%token Array_Spec_
%token ARTIFICIAL
%token AS_ASSUMED_SHAPE
%token AS_ASSUMED_SIZE
%token AS_DEFERRED
%token AS_EXPLICIT
%token ASSOCIATE
%token ASSIGN
%token ASYNCHRONOUS
%token Attributes_
%token A_Z
%token Basisfn
%token Beginning
%token BINDLP
%token BLANK
%token BLOCK
%token C
%token CALL
%token C_funptr
%token CHARACTER
%token C_INTEROP
%token CLOSE
%token Code_
%token Common_
%token Components_
%token CONTAINS
%token CONVERT
%token C_ptr
%token CYCLE
%token DEALLOCATE
%token DECIMAL
%token DELIM
%token DIRECT
%token Di
%token DIMENSION
%token DO
%token DT_END
%token DUMMY
%token DUMMYLP
%token ELEMENTAL
%token ELSE
%token ENCODING
%token END
%token END_
%token ENDIF
%token Equivalence_
%token EXIST
%token EXIT
%token Exiting
%token EXPLICIT_SAVE
%token EXTERNAL
%token EXTERNAL_PROC
%token FMT
%token FMTASCII
%token FMTFLTE
%token FMTFLTF
%token FMTINT
%token From_Namespace_
%token Formal_Arglist_
%token FORALL
%token FULL
%token FUNCTION
%token Generic_Interfaces_
%token Hash_
%token HF
%token Highest
%token ID
%token IF
%token IMPLICIT_SAVE
%token IMPLICIT_TYPE
%token IN
%token IN_NAMELIST
%token INOUT
%token INTEGER
%token INTERNAL_PROC
%token INTRINSIC
%token INTRINSIC_PROC
%token IOMSG
%token ISO_C
%token LABEL
%token Label_
%token LOGICAL
%token MODULE_PROC
%token Namespace_
%token ARG_NOT_PRESENT
%token NAME_
%token NAMED
%token NEXTREC
%token NUMBER
%token NML
%token NULL
%token OPENED
%token Operator_Bindings_
%token Operator_Interfaces_For
%token OUT
%token PAD
%token Perturbation
%token PENDING
%token Please
%token POINTER
%token POSITION
%token PRIVATE
%token Procedure_Name_
%token Procedure_Bindings_
%token Processor
%token PURE
%token REAL
%token READWRITE
%token Restarting
%token Result_
%token RETURN
%token RESULT
%token ROUND
%token SEQUENCE
%token SEQUENTIAL
%token Setting
%token Shift
%token SIGN
%token SIZE
%token STAT
%token STATEMENT_PROC
%token SUBROUTINE
%token Symbol_
%token Symmetry
%token Sympairprod
%token Symtree_
%token TARGET
%token This
%token Timer
%token TRANSFER
%token Type_Spec_
%token UNIT
%token USE_ASSOCLP
%token User_operators_
%token UNFORMATTED
%token Value_
%token VALUE
%token VOLATILE
%token WHERE
%token WRITE
%token WRITE_
%type <token> ml_start
%type <token list> idxlst
%start ml_start
%%

ml_start: namespace procedure EOF_TOKEN { TUPLE2($1,$2) }

itemlst: { [] }
	| itemlst item { $2 :: $1 }

item: namespace { $1 }
	| procedure { $1 }
	
namespace: Namespace_ kindlst { TUPLE2(Namespace_, l $2) }

kindlst: { [] }
	| kindlst RANGE typekind RPAR { TUPLE3(RANGE $2, $3, RPAR) :: $1 }
	| kindlst IDENTCLN typekind RPAR { TUPLE3(RANGE ($2^"-"^$2), $3, RPAR) :: $1 }

procedure: Procedure_Name_ NAME INDENT level1lst maycontain UNINDENT
	   { TUPLE6(Procedure_Name_,NAME $2,INDENT, l $4,$5,UNINDENT) }

maycontain: { TLIST [] }
	| CONTAINS itemlst { l $2 }
	
level1lst: { [] }
	| level1lst level1item { $2 :: $1 }

level1item: symtree { $1 }
	| operintf { $1 }
	| useroper { $1 }
	| equivalence { $1 }
	| code { $1 }
        | common { $1 }

common: Common_ SLASH namcommon SLASH equivlst { TUPLE5(Common_,SLASH,$3,SLASH, l $5) }

namcommon: NAME { NAME $1 }

hollerith: HOLLERITH { $1 }
	| hollerith HOLLERITH { $1 ^ $2 }

symtree: Symtree_ hollerith sym_opt VBAR VBAR Symbol_ hollerith from_opt level2_opt {
	   TUPLE9(Symtree_,HOLLERITH $2,$3,VBAR,VBAR,Symbol_,HOLLERITH $7,$8,$9) }

sym_opt: { EMPTY_TOKEN }
	 | Ambiguous {  Ambiguous }

level2_opt: { EMPTY_TOKEN }
	  | INDENT level2lst UNINDENT { TUPLE3(INDENT, l $2,UNINDENT) }

level2lst: { [] }
	|  level2lst level2item { $2 :: $1 }

from_opt: { EMPTY_TOKEN }
	  | From_Namespace_ hollerith { TUPLE2(From_Namespace_,HOLLERITH $2) }

operintf: Operator_Interfaces_For operintf2 COLON namlst { TUPLE4(Operator_Interfaces_For,$2,COLON, l $4) }

operintf2: EQ { EQ }
	   | NE { NE }
	   | GT { GT }
	   | LT { LT }
	   | EQUALS { EQUALS }

equivalence: Equivalence_ equivlst { TUPLE2(Equivalence_, l $2) }

equivlst: lval { $1 :: [] }
	| equivlst COMMA lval { $3 :: $1 }

code: Code_ stmtlst { TUPLE2(Code_, l $2) }

assoclst: { [] }
	 | assoclst NAME EQUALS lval { TUPLE3(NAME $2,EQUALS,$4) :: $1 }
	 | assoclst NAME EQUALS dyadic expr expr RPAR { TUPLE6(NAME $2,EQUALS,$4,$5,$6,RPAR) :: $1 }

stmtlst: { [] }
	 | stmtlst stmt { $2 :: $1 }
	 | stmtlst Label_ INTNUM stmt { TUPLE2(LABNUM $3,$4) :: $1 }

ifstmtlst: { EMPTY_TOKEN }
	| INDENT stmtlst elseif_stmt_lst_un { TUPLE2(l $2, l $3) }
 
elseif_stmt_lst_end: ENDIF { ENDIF :: [] }
 	| ELSE ifstmtlst ENDIF { TUPLE3(ELSE, $2, ENDIF) :: [] }
 	| ELSEIF expr ifstmtlst elseif_stmt_lst_end { TUPLE3(ELSEIF,$2,$3) :: $4 }
 
elseif_stmt_lst_un: UNINDENT { UNINDENT :: [] }
 	| ELSE stmtlst UNINDENT { TUPLE3(ELSE, l $2, UNINDENT) :: [] }
 	| INDENT ELSE UNINDENT stmtlst UNINDENT { TUPLE3(ELSE, l $4, UNINDENT) :: [] }
 	| ELSEIF expr stmtlst elseif_stmt_lst_un { TUPLE3(ELSEIF, $2, l $3) :: $4 }
 	| INDENT ELSEIF expr UNINDENT stmtlst elseif_stmt_lst_un { TUPLE3(ELSEIF, $3, l $5) :: $6 }
 	| INDENT ELSEIF expr ELSE UNINDENT stmtlst elseif_stmt_lst_un { TUPLE4(ELSEIF, $3, ELSE, l $6) :: $6 }
	 
caselst: { [] }
	 | caselst LPAR cexpr cexpr RPAR { TUPLE4(LPAR, $3, $4, RPAR) :: $1 }
	 | caselst LPAR LPAR RPAR LPAR RPAR RPAR { TUPLE2(LPAR, RPAR) :: $1 }
	 | caselst LPAR RPAR { TUPLE2(LPAR, RPAR) :: $1 }

end_do:	 END DO { TUPLE2(END,DO) }
	| INDENT end_do UNINDENT { TUPLE3(INDENT, $2, UNINDENT) }

stmt:	  BLOCK INDENT level1lst stmtlst UNINDENT END BLOCK {
	       TUPLE7(BLOCK,INDENT, l $3, l $4,UNINDENT,END,BLOCK) }
	| ASSOCIATE assoclst INDENT level1lst stmtlst UNINDENT END ASSOCIATE {
	   	   TUPLE8(ASSOCIATE, l $2, INDENT, l $4, l $5,UNINDENT,END,ASSOCIATE) }
	| IF expr INTNUM COMMA INTNUM COMMA INTNUM { TUPLE5(ARITHIF,$2,INTNUM $3,INTNUM $5,INTNUM $7) }
	| IF expr ifstmtlst elseif_stmt_lst_end { TUPLE4(IF, $2, $3, l $4) }
	| DO WHILE expr stmtlst end_do { TUPLE6(DO,WHILE,$3, l $4, END, DO) }
	| DO asgnvar expr expr expr INDENT stmtlst UNINDENT END DO {
	       TUPLE11(DO,$2,EQUALS,$3,$4,$5,INDENT, l $7,UNINDENT,END,DO) }
	| DO INTNUM asgnvar expr expr expr INDENT stmtlst UNINDENT ENDDO INTNUM {
           TUPLE10(DO,INTNUM $2,$3,EQUALS,$4,$5,$6,INDENT,l $8,ENDDO) }
	| SELECT CASE expr { TUPLE3(SELECT,CASE,$3) }
	| caseitm { $1 }
	| INDENT caseitmlst UNINDENT { TUPLE2(CASE, l $2) }
	| END SELECT { TUPLE2(END,SELECT) }
	| INDENT END SELECT UNINDENT { TUPLE2(END,SELECT) }
        | LABEL ASSIGN lval INTNUM { TUPLE4(LABEL,ASSIGN,$3,INTNUM $4) }
        | ASSIGN lval expr { TUPLE3(LET,$2,$3) }
	| ASSIGN lval expr LBRACE initlst RBRACE { TUPLE6(LET,$2,$3,LBRACE, l $5, RBRACE) }
        | POINTER ASSIGN lval expr { TUPLE4(POINTER,ASSIGN,$3,$4) }
	| CALL callexpr paramlst RPAR { TUPLE5(CALL,$2,LPAR, l $3,RPAR) }
	| INQUIRE rdwrlst { TUPLE2(INQUIRE, l $2) }
	| OPEN rdwrlst { TUPLE2(OPEN, l $2) }
	| READ rdwrlst DT_END err_opt { TUPLE3(READ, l $2, DT_END) }
	| WRITE transfer_rdwrlst DT_END err_opt { TUPLE3(WRITE, l $2, DT_END) }
	| CLOSE rdwrlst { TUPLE2(CLOSE, l $2) }
	| REWIND rdwrlst { TUPLE2(REWIND, l $2) }
	| FLUSH UNIT INTNUM { TUPLE3(FLUSH, UNIT, INTNUM $3) }
	| DATA datalst SLASH starlst SLASH { TUPLE5(DATA, l $2,SLASH, l $4,SLASH) }
	| ALLOCATE alloclst lvallst { TUPLE3(ALLOCATE, l $2, l $3) }
	| DEALLOCATE alloclst lvallst { TUPLE3(DEALLOCATE, l $2, l $3) }
	| FORALL lval foralllst stmtlst END FORALL { TUPLE6(FORALL, $2, l $3, l $4, END, FORALL) }
	| FORALL lval foralllst stmtlst INDENT END FORALL UNINDENT { TUPLE6(FORALL, $2, l $3, l $4, END, FORALL) }
	| RETURN { RETURN }
	| CYCLE { CYCLE }
	| EXIT { EXIT }
	| STOP expr { TUPLE2(STOP,$2) }
	| GOTO INTNUM { TUPLE2(GOTO,INTNUM $2) }
	| GOTO lval COMMA LPAR lablst RPAR { TUPLE3(GOTO,$2, l $5) }
	| CONTINUE { CONTINUE }
	| WHERE expr stmtlst elsewhere_stmt_lst WHERE { TUPLE4(WHERE,$2, l $3, l $4) }
	
caseitmlst: caseitm { $1 :: [] }
	| caseitmlst caseitm { $2 :: $1 }

caseitm: CASE caselst { TUPLE2(CASE, l $2) }

elsewhere_stmt_lst: END { END :: [] }
 	| ELSE WHERE expr stmtlst elsewhere_stmt_lst { TUPLE4(ELSE, WHERE, $3, l $4) :: $5 }
 	| ELSE WHERE LPAR RPAR stmtlst elsewhere_stmt_lst { TUPLE5(ELSE, WHERE, LPAR, RPAR, l $5) :: $6 }

lablst: INTNUM { INTNUM $1 :: [] }
	| lablst COMMA INTNUM { INTNUM $3 :: $1 }

datalst: lval { $1 :: [] }
	| datalst COMMA lval { $3 :: $1 }

starlst: staritm { $1 :: [] }
	| starlst COMMA staritm { $3 :: $1 }

staritm: dataitm STAR dataitm { TUPLE3($1, STAR, $3) }

dataitm: INTNUM { INTNUM $1 }
       | hollerith { HOLLERITH $1 }
       | naturalnum { $1 }
       | FLTNUM { FLTNUM $1 }
       | TRUE { TRUE }
       | FALSE { FALSE }
	
foralllst: idxlst { l $1 :: [] }
	| foralllst idxlst { l $2 :: $1 }

callexpr: NAME LPAR { NAME $1 }
	| NAME TILDE NAME PERCENT IDENTLP { TUPLE5(NAME $1, TILDE, NAME $3, PERCENT, NAME $5) }

err_opt: { EMPTY_TOKEN }
	| ERR EQUALS INTNUM { TUPLE3(ERR,EQUALS,INTNUM $3) }
	| END_ INTNUM { TUPLE3(END,EQUALS,INTNUM $2) }

cexpr:	INTNUM { INTNUM $1 }
	| MINUS INTNUM { INTNUM ("-"^$2) }
	| HOLLERITH { HOLLERITH $1 }

stmt_opt: { EMPTY_TOKEN }
	| TRANSFER hollerith { TUPLE2(TRANSFER,HOLLERITH $2) }

lvallst: { [] }
	| lvallst lval { $2 :: $1 }

alloclst: { [] }
	| alloclst allocitm { $2 :: $1 }

allocitm:  STAT expr { TUPLE2(STAT,$2) }

paramlst: { [] }
	| paramlst LPAR expr RPAR { $3 :: $1 }
	| paramlst LPAR LPARG ARG_NOT_PRESENT RPAR { ARG_NOT_PRESENT :: $1 }
	| paramlst LPIDENT EQUALS expr RPAR { TUPLE3(NAME $2,EQUALS,$4) :: $1 }

transfer_rdwrlst: { [] }
	| transfer_rdwrlst transfer_rdwrt { $2 :: $1 }

rdwrlst: { [] }
	| rdwrlst rdwrt { $2 :: $1 }

rdwrt:	  ACCESS expr { TUPLE2(ACCESS,$2) }
	| ACTION expr { TUPLE2(ACTION,$2) }
	| ADVANCE expr { TUPLE2(ADVANCE,$2) }
	| ASYNCHRONOUS expr { TUPLE2(ASYNCHRONOUS,$2) }
	| BLANK expr { TUPLE2(BLANK,$2) }
	| CONVERT expr { TUPLE2(CONVERT,$2) }
	| DECIMAL expr { TUPLE2(DECIMAL,$2) }
	| DELIM expr { TUPLE2(DELIM,$2) }
	| DIRECT expr { TUPLE2(DIRECT,$2) }
	| ENCODING expr { TUPLE2(ENCODING,$2) }
	| END_ INTNUM { TUPLE2(END,INTNUM $2) }
	| ERR INTNUM { TUPLE2(ERR,INTNUM $2) }
	| EXIST expr { TUPLE2(EXIST,$2) }
	| FILE expr { TUPLE2(FILE,$2) }
	| FMT INTNUM fmts { TUPLE3(FMT,INTNUM $2,$3) }
	| FMT expr { TUPLE2(FMT,$2) }
	| FORM expr { TUPLE2(FORM,$2) }
	| FORMATTED expr { TUPLE2(FORMATTED,$2) }
	| ID expr { TUPLE2(ID,$2) }
	| IOMSG expr { TUPLE2(IOMSG,$2) }
	| IOSTAT expr { TUPLE2(IOSTAT,$2) }
	| NAME_ NAME { TUPLE2(NAME_,NAME $2) }
	| NAMED NAME { TUPLE2(NAMED,NAME $2) }
	| NEXTREC NAME { TUPLE2(NEXTREC,NAME $2) }
	| NML NAME { TUPLE2(NML,NAME $2) }
	| NUMBER NAME { TUPLE2(NUMBER,NAME $2) }
	| OPENED expr { TUPLE2(OPENED,$2) }
	| PAD expr { TUPLE2(PAD,$2) }
	| PENDING expr { TUPLE2(PENDING,$2) }
	| POSITION expr { TUPLE2(POSITION,$2) }
	| READ_ expr { TUPLE2(READ,$2) }
	| READWRITE expr { TUPLE2(READWRITE,$2) }
	| REC expr { TUPLE2(REC,$2) }
	| RECL expr { TUPLE2(RECL,$2) }
	| ROUND expr { TUPLE2(ROUND,$2) }
	| SEQUENTIAL expr { TUPLE2(SEQUENTIAL,$2) }
	| SIGN expr { TUPLE2(SIGN,$2) }
	| SIZE expr { TUPLE2(SIZE,$2) }
	| STAT expr { TUPLE2(STAT,$2) }
	| STATUS expr { TUPLE2(STATUS,$2) }
	| TRANSFER expr { TUPLE2(TRANSFER,$2) }
	| UNFORMATTED expr { TUPLE2(UNFORMATTED,$2) }
	| UNIT expr { TUPLE2(UNIT,$2) }
	| WRITE_ expr { TUPLE2(WRITE,$2) }

transfer_rdwrt: rdwrt { $1 }
	| DO asgnvar expr expr expr INDENT transfer_rdwrlst UNINDENT END DO stmt_opt {
	       TUPLE12(DO,$2,EQUALS,$3,$4,$5,INDENT, l $7,UNINDENT,END,DO,$11) }

asgnvar: NAME TILDE IDENTEQ { TUPLE4(NAME $1,TILDE,NAME $3, TLIST []) }

fmts:     skiplst { l $1 }
        | LBRACE fmts RBRACE { TUPLE3(LBRACE, $2, RBRACE) }
        | LPAR fmts RPAR { TUPLE3(LPAR, $2, RPAR) }

skiplst : skipitm { $1 :: [] }
	| skiplst COMMA skipitm { $3 :: $1 }

skipitm : DOT { DOT }
        | INTNUM { INTNUM $1 }
        | NAME { NAME $1 }
        | NAME DOT INTNUM { TUPLE3(NAME $1, DOT, INTNUM $3) }
        | INTNUM NAME { TUPLE2(INTNUM $1, NAME $2) }
        | PLUS { PLUS }
        | MINUS { MINUS }
        | COLON { COLON }
        | SLASH { SLASH }
        | HOLLERITH { HOLLERITH $1 }

useroper: User_operators_ useroperitmlst { TUPLE2(User_operators_, l $2) }

useroperitmlst: { [] }
	| useroperitmlst useroperitm { $2 :: $1 }
	
useroperitm: IDENTCLN useroperlst { TUPLE2(NAME $1, l $2) }

useroperlst: { [] }
	 | useroperlst NAME { NAME $2 :: $1 }

attrlst2: { [] }
	 | attrlst2 attr2 { $2 :: $1 }

lpattr: LPPROGRAM { LPPROGRAM }
	 | LPPROCEDURE { LPPROCEDURE }
         | LPDERIVED { LPDERIVED }
         | LPVARIABLE { LPVARIABLE }
         | LPMODULE { LPMODULE }
         | LPPARAMETER { LPPARAMETER }
         | LPLABEL { LPLABEL }
         | LPNAMELIST { LPNAMELIST }

level2item: Type_Spec_ typekind RPAR { TUPLE3(Type_Spec_,$2,RPAR) }
      | Attributes_ lpattr attrlst2 RPAR { TUPLE3(Attributes_, TLIST ($2 :: (List.rev $3)),RPAR) }
      | Array_Spec_ LPAR INTNUM LBRACK INTNUM RBRACK dkind RPAR { TUPLE8(Array_Spec_,LPAR,INTNUM $3,LBRACK,INTNUM $5,RBRACK,$7,RPAR) }
      | Result_ NAME { TUPLE2(Result_,NAME $2) }
      | Formal_Arglist_ arglst { TUPLE2(Formal_Arglist_, l $2) }
      | Components_ complst { TUPLE2(Components_, l $2) }
      | Hash_ INTNUM { TUPLE2(Hash_,INTNUM $2) }
      | Procedure_Bindings_ { Procedure_Bindings_ }
      | Operator_Bindings_ { Operator_Bindings_ }
      | Generic_Interfaces_ namlst { TUPLE2(Generic_Interfaces_, l $2) }
      | Value_ value { TUPLE2(Value_,$2) }
      | Common_ LPAR commonlst RPAR { TUPLE4(Common_,LPAR, l $3, RPAR) }

commonlst: hollerith { HOLLERITH $1 :: [] }
      | commonlst COMMA hollerith { HOLLERITH $3 :: $1 }

value: INTNUM { INTNUM $1 }
       | LPARRAY varglst ARRAYRP { TUPLE3(LPARRAY, l $2, ARRAYRP) }
       | NULL RPAR { TUPLE3(NULL,LPAR,RPAR) }
       | expr { $1 }

varglst: varg COLON INTNUM { let cnt = int_of_string $3 in if cnt > 1 then Array.to_list (Array.make cnt $1) else $1 :: [] }
	 | varglst COMMA varg COLON INTNUM { let cnt = int_of_string $5 in if cnt > 1 then Array.to_list (Array.make cnt $3) @ $1 else $3 :: $1 }

varg:	  NAME { NAME $1 }
	| naturalnum { $1 }
	| floatnum { $1 }
	| LPAR RPAR { TUPLE2(LPAR,RPAR) }
	| NULL RPAR { TUPLE3(NULL,LPAR,RPAR) }
	| TRUE { TRUE }
	| FALSE { FALSE }
	| DOTTED { DOTTED $1 }
	| hollerith { HOLLERITH $1 }
	| NAME LPAR varglst RPAR { TUPLE4(NAME $1, LPAR, l $3, RPAR) }
        | LPSLASH varglst SLASHRP { TUPLE3(LPSLASH, l $2, SLASHRP) }
        | LPARRAY varglst ARRAYRP { TUPLE3(LPARRAY, l $2, ARRAYRP) }

namlst:  { [] }
	| namlst NAME { NAME $2 :: $1 }

dkind: AS_DEFERRED attrlst3 { TUPLE2(AS_DEFERRED, l $2) }
	|	AS_EXPLICIT dimlst { TUPLE2(AS_EXPLICIT, l $2) }
	|	AS_ASSUMED_SHAPE dimlst { TUPLE2(AS_ASSUMED_SHAPE, l $2) }
	|	AS_ASSUMED_SIZE dimlst { TUPLE2(AS_ASSUMED_SIZE, l $2) }

dimlst:  { [] }
	| dimlst dim { $2 :: $1 }

dim:      LPAR RPAR { TUPLE2(LPAR,RPAR) }
        | expr { $1 }

idxlst: slice { $1 :: [] }
	| idxlst COMMA slice { $3 :: $1 }

slice: expr { $1 }
	| COLON { COLON }
	| COLON expr { TUPLE2(COLON,$2) }
	| expr COLON { TUPLE2($1,COLON) }
	| expr COLON LPAR RPAR { TUPLE4($1,COLON,LPAR,RPAR) }
	| LPAR RPAR COLON expr { TUPLE4(LPAR,RPAR,COLON,$4) }
	| NULL RPAR COLON expr { TUPLE4(NULL,RPAR,COLON,$4) }
	| expr COLON expr { TUPLE3($1,COLON,$3) }
	| expr COLON COLON expr { TUPLE4($1,COLON,COLON,$4) }
	| expr COLON expr COLON expr { TUPLE5($1,COLON,$3,COLON,$5) }

percent_lst: { [] }
	| percent_lst PERCENT NAME { TUPLE2(PERCENT,NAME $3) :: $1 }
	| percent_lst PERCENT IDENTLP FULL RPAR { TUPLE3(PERCENT,NAME $3, FULL) :: $1 }
	| percent_lst PERCENT IDENTLP RPAR { TUPLE4(PERCENT,NAME $3, LPAR, RPAR) :: $1 }
	| percent_lst PERCENT IDENTLP idxlst RPAR { TUPLE5(PERCENT,NAME $3, LPAR, l $4, RPAR) :: $1 }
	| percent_lst PERCENT brackid { TUPLE2(PERCENT,$3) :: $1 }

brackid: BRACKID LPAR brexprlst RPAR RBRACK { TUPLE6(NAME $1,LBRACK,LPAR, l $3,RPAR,RBRACK) }

lval:	  NAME TILDE NAME percent_lst { TUPLE4(NAME $1,TILDE,NAME $3, l $4) }
	| NAME TILDE IDENTLP FULL RPAR percent_lst { TUPLE5(NAME $1,TILDE,NAME $3, FULL, l $6) }
	| NAME TILDE IDENTLP idxlst RPAR percent_lst {
	   TUPLE7(NAME $1,TILDE,NAME $3, LPAR, l $4, RPAR, l $6) }
	| NAME percent_lst { TUPLE2(NAME $1, l $2) }
	| brackid { $1 }
	| BRACKID2 LPAR brexprlst RPAR RBRACK RBRACK { TUPLE8(NAME $1,LBRACK,LBRACK,LPAR, l $3,RPAR,RBRACK,RBRACK) }
	| FDOT BRACKID2 LPAR brexprlst RPAR RBRACK RBRACK {
	       TUPLE9(FDOT,NAME $2,LBRACK,LBRACK,LPAR, l $4,RPAR,RBRACK,RBRACK) }

brexprlst: { [] }
	| brexprlst LPAR LPARG ARG_NOT_PRESENT RPAR { ARG_NOT_PRESENT :: $1 }
	| brexprlst LPAR expr RPAR { TUPLE3(LPAR, $3, RPAR) :: $1 }
	| brexprlst LPIDENT percent_lst RPAR { TUPLE4(LPAR, NAME $2, l $3, RPAR) :: $1 }
	| brexprlst LPIDENT EQUALS expr RPAR { TUPLE5(LPAR,NAME $2, EQUALS, $4, RPAR) :: $1 }

expr: LPAR expr RPAR { TUPLE3(LPAR,$2,RPAR) }
	| lval { $1 }
        | hollerith { HOLLERITH $1 }
        | hollerith LPAR idxlst RPAR { HOLLERITH $1 }
	| naturalnum { $1 }
	| floatnum { $1 }
	| NULL RPAR { TUPLE3(NULL,LPAR,RPAR) }
	| TRUE { TRUE }
	| FALSE { FALSE }
	| LPCOMPLEX floatnum floatnum RPAR { TUPLE4(COMPLEX, $2, $3, RPAR) }
	| ARG_NOT_PRESENT { ARG_NOT_PRESENT }
        | monadic expr RPAR { TUPLE3($1, $2, RPAR) }
        | dyadic expr expr RPAR { TUPLE4($1,$2,$3,RPAR) }
	| IDENTLP idxlst RPAR { TUPLE4(NAME $1, LPAR, l $2, RPAR) }
	| LPARRAY idxlst ARRAYRP { TUPLE3(LPARRAY, l $2, ARRAYRP) }

floatnum: FLTNUM { FLT(float_of_string $1, "") }
	| MINUS FLTNUM { FLT(-. (float_of_string $2), "") }
	| FLTNUM UNUMBER { FLT(float_of_string $1,$2) }
/*
	| ICON { FLT(float_of_string $1, "") }
	| ICON UNUMBER { FLT(float_of_string $1, $2) }
*/

naturalnum:
       | INTNUM { INT(int_of_string $1, "") }
       | INTNUM UNUMBER { INT64(Int64.of_string $1, $2) }
       | MINUS INTNUM { INT(- (int_of_string $2), "") }
       | MINUS INTNUM UNUMBER { INT64(unumcnv $2 $3, $3) }

initlst: INTNUM { INTNUM $1 :: [] }
	| initlst COMMA INTNUM { INTNUM $3 :: $1 }

monadic: LPUMINUS { LPUMINUS }
	| LPUPLUS { LPUPLUS }
	| LPNOT { LPNOT }
	| LPPARENS { LPPARENS }
	| NAME { NAME $1 }

dyadic:
	| LPEQV { LPEQV }
	| LPNEQV { LPNEQV }
	| LPOR { LPOR }
	| LPAND { LPAND }
	| LPEQUALS { LPEQUALS }
	| LPLESS { LPLESS }
	| LPGREATER { LPGREATER }
	| LPGTEQUAL { LPGTEQUAL }
	| LPLTEQUAL { LPLTEQUAL }
	| LPPLUS { LPPLUS }
	| LPMINUS { LPMINUS }
	| LPSTAR { LPSTAR }
	| LPSLASH { LPSLASH }
	| LPPERCENT { LPPERCENT }
	| LPPOWER { LPPOWER }
	| LPCONCAT { LPCONCAT }
	| LPNOTEQUAL { LPNOTEQUAL }

complst:  { [] }
	| complst LPIDENT typekind RPAR attrlst3 RPAR { TUPLE6(LPAR,NAME $2,$3,RPAR, l $5,RPAR) :: $1 }

attrlst3: { [] }
	 | attrlst3 attr3 { $2 :: $1 }

attr3: LPAR RPAR { TUPLE2(LPAR,RPAR) }
       | ALLOCATABLE { ALLOCATABLE }
       | POINTER { POINTER }
       | PPC { PPC }
       | PUBLIC { PUBLIC }
       | PRIVATE { PRIVATE }
       | DIMENSION LPAR INTNUM LBRACK INTNUM RBRACK dkind RPAR { TUPLE8(DIMENSION,LPAR,INTNUM $3,LBRACK,INTNUM $5,RBRACK,$7,RPAR) }

arglst:  { [] }
	| arglst NAME { NAME $2 :: $1 }

typekind: LPINTEGER INTNUM typattrlst { TUPLE3(INTEGER, INTNUM $2, l $3) }
      | LPUNKNOWN INTNUM typattrlst { TUPLE3(LPUNKNOWN, INTNUM $2, l $3) }
      | LPLOGICAL INTNUM typattrlst { TUPLE3(LOGICAL, INTNUM $2, l $3) }
      | LPREAL INTNUM typattrlst { TUPLE3(REAL, INTNUM $2, l $3) }
      | LPCOMPLEX INTNUM typattrlst { TUPLE3(COMPLEX, INTNUM $2, l $3) }
      | LPCHARACTER cdim typattrlst { TUPLE3(CHARACTER, $2, l $3) }
      | LPDERIVED NAME typattrlst { TUPLE3(LPDERIVED, NAME $2, l $3) }

cdim: INTNUM { INTNUM $1 }
      | LPAR RPAR { TUPLE2(LPAR,RPAR) }
      | dyadic expr expr RPAR { TUPLE4($1,$2,$3,RPAR) }
      | lval { $1 }

typattrlst:  { [] }
	| typattrlst typattr { $2 :: $1 }

typattr: INTNUM { INTNUM $1 }
	| C_INTEROP { C_INTEROP }
        | ISO_C { ISO_C }
	| DEFERRED { DEFERRED }

attr2:
       | MODULE_PROC { MODULE_PROC }
       | DIMENSION { DIMENSION }
       | USE_ASSOCLP assoc RPAR { $2 }
       | FUNCTION { FUNCTION }
       | ELEMENTAL { ELEMENTAL }
       | PURE { PURE }
       | RECURSIVE { RECURSIVE }
       | ABSTRACT { ABSTRACT }
       | SUBROUTINE { SUBROUTINE }
       | VALUE { VALUE }
       | IN_COMMON { IN_COMMON }
       | IMPLICIT_SAVE { IMPLICIT_SAVE }
       | IMPLICIT_TYPE { IMPLICIT_TYPE }
       | IN_NAMELIST { IN_NAMELIST }
       | INTERNAL_PROC { INTERNAL_PROC }
       | STATEMENT_PROC { STATEMENT_PROC }
       | EXPLICIT_SAVE { EXPLICIT_SAVE }
       | BINDLP C RPAR { TUPLE3(BINDLP,C,RPAR) }
       | SEQUENCE { SEQUENCE }
       | INTRINSIC { INTRINSIC }
       | ALLOCATABLE { ALLOCATABLE }
       | POINTER { POINTER }
       | EXTERNAL { EXTERNAL }
       | EXTERNAL_PROC { EXTERNAL_PROC }
       | TARGET { TARGET }
       | INTRINSIC_PROC { INTRINSIC_PROC }
       | VOLATILE { VOLATILE }
       | DATA { DATA }
       | DUMMY { DUMMY }
       | DUMMYLP dir RPAR { TUPLE3(DUMMYLP,$2,RPAR) }
       | ARTIFICIAL { ARTIFICIAL }
       | PUBLIC { PUBLIC }
       | PRIVATE { PRIVATE }
       | RESULT { RESULT }
       | OPTIONAL { OPTIONAL }

dir: INOUT { INOUT }
     | IN { IN }
     | OUT { OUT }

assoc: NAME { ASSOC $1 }
       | LPAR NAME RPAR { ASSOC $2 }

unused: ACCEPT { ACCEPT }
    | A { A }
    | All { All }
    | Allocating { Allocating }
    | AMPERSAND { AMPERSAND }
    | ARITHIF { ARITHIF }
    | ASGOTO { ASGOTO }
    | AT { AT }
    | AUTOMATIC { AUTOMATIC }
    | A_Z { A_Z }
    | BACKQUOTE { BACKQUOTE }
    | BACKSLASH { BACKSLASH }
    | BACKSPACE { BACKSPACE }
    | Basisfn { Basisfn }
    | Beginning { Beginning }
    | BITCON { BITCON }
    | BYTE { BYTE }
    | CARET { CARET }
    | C_funptr { C_funptr }
    | CHARACTER { CHARACTER }
    | COMMENT { COMMENT }
    | COMMON { COMMON }
    | COMPGOTO { COMPGOTO }
    | COMPLEX { COMPLEX }
    | CONCAT { CONCAT }
    | C_ptr { C_ptr }
    | CURRENCY { CURRENCY }
    | DASHES { DASHES }
    | DATASTAR { DATASTAR }
    | DCOMPLEX { DCOMPLEX }
    | DCON { DCON }
    | DEFAULT { DEFAULT }
    | Di { Di }
    | DISCARD { DISCARD }
    | DOLLAR { DOLLAR }
    | DOUBLE { DOUBLE }
    | DOUBLEQUOTE { DOUBLEQUOTE }
    | ELLIPSIS { ELLIPSIS }
    | EMPTY_TOKEN { EMPTY_TOKEN }
    | ENDFILE { ENDFILE }
    | EOS { EOS }
    | EQUIV { EQUIV }
    | ERROR { ERROR }
    | ERROR_TOKEN { ERROR_TOKEN }
    | Exiting { Exiting }
    | FIELD { FIELD }
    | FLT { FLT $1 }
    | FMTASCII { FMTASCII }
    | FMTFLTE { FMTFLTE }
    | FMTFLTF { FMTFLTF }
    | FMTINT { FMTINT }
    | FORMAT { FORMAT }
    | GE { GE }
    | GO { GO }
    | HASH { HASH }
    | HEXCON { HEXCON }
    | HF { HF }
    | Highest { Highest }
    | IDENTCLN { IDENTCLN }
    | IMPLICIT { IMPLICIT }
    | INCLUDE { INCLUDE }
    | INT { INT $1 }
    | INTEGER { INTEGER }
    | LABHASH { LABHASH }
    | LABNUM { LABNUM }
    | LE { LE }
    | LEN { LEN }
    | LET { LET }
    | LINEFEED { LINEFEED }
    | LOGICAL { LOGICAL }
    | LPBACK { LPBACK }
    | LPDIM { LPDIM }
    | LPEL { LPEL }
    | LPKEY { LPKEY }
    | LPKIND { LPKIND }
    | LPMASK { LPMASK }
    | LPPADDING { LPPADDING }
    | LPPOS { LPPOS }
    | LPTHANDLE { LPTHANDLE }
    | LPTRIM { LPTRIM }
    | NONE { NONE }
    | OCTCON { OCTCON }
    | PAUSE { PAUSE }
    | Perturbation { Perturbation }
    | Please { Please }
    | PLING { PLING }
    | POP { POP }
    | POWER { POWER }
    | PRINT { PRINT }
    | Processor { Processor }
    | PUNCH { PUNCH }
    | QUERY { QUERY }
    | QUOTE { QUOTE }
    | RCON { RCON }
    | REAL { REAL }
    | Restarting { Restarting }
    | SAVE { SAVE }
    | SCALE { SCALE }
    | Setting { Setting }
    | Shift { Shift }
    | SLASHD { SLASHD }
    | SPACE { SPACE }
    | STATIC { STATIC }
    | Symmetry { Symmetry }
    | Sympairprod { Sympairprod }
    | TARRAY { TARRAY }
    | THEN { THEN }
    | This { This }
    | Timer { Timer }
    | TLIST { TLIST }
    | TO { TO }
    | TUPLE10 { TUPLE10 }
    | TUPLE11 { TUPLE11 }
    | TUPLE12 { TUPLE12 }
    | TUPLE13 { TUPLE13 }
    | TUPLE14 { TUPLE14 }
    | TUPLE2 { TUPLE2 }
    | TUPLE3 { TUPLE3 }
    | TUPLE4 { TUPLE4 }
    | TUPLE5 { TUPLE5 }
    | TUPLE6 { TUPLE6 }
    | TUPLE7 { TUPLE7 }
    | TUPLE8 { TUPLE8 }
    | TUPLE9 { TUPLE9 }
    | UNDEFINED { UNDEFINED }
    | UNDERSCORE { UNDERSCORE }
