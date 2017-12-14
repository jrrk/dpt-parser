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

open Program
open Program_lex

let parse_from_chan ch =
  lex_init();
  let lb = Lexing.from_channel ch in
  let output = try
      ml_start token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
(*
    | _ ->
      failwith (Printf.sprintf "Parser error at line %d" !Scope.lincnt)
*)
  in
  output
					    
let parse arg =
  let ch = open_in arg in
  try
  print_endline ("**** Parsing "^arg^" ****");
  let log = open_out (arg^".err") in
  Unix.dup2 (Unix.descr_of_out_channel log) Unix.stderr;
  print_string ("stderr redirected to "^arg^".err - ");
  let rslt = parse_from_chan ch in
  close_in ch;
  print_endline "completed.";
  rslt
  with  e ->
    print_endline ("** Error ** "^Printexc.to_string e);
    Printexc.print_backtrace stderr;
    close_in ch;
  EMPTY_TOKEN

let expand_paths dir =
    let dirlst = List.sort compare (Array.to_list (Sys.readdir dir)) in
    let filt itm = let len = String.length itm in len > 5 && String.sub itm (len-4) 4 = ".dpt" in
    let dir' = List.filter filt dirlst in
    Array.map parse (Array.map (fun itm -> dir^"/"^itm) (Array.of_list dir'))

let marshal_out dir dump =
    begin
    let rslt = expand_paths dir in
    let chan = open_out dump in							       
    Marshal.to_channel chan rslt [Marshal.No_sharing];
    close_out chan
    end

let _ = if Array.length Sys.argv > 2 then marshal_out Sys.argv.(1) Sys.argv.(2)
