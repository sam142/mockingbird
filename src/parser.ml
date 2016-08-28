open Opal
open Types

type ast =
  | AVar of string
  | AApp of ast * ast
  | AAbs of string * ast

let term input =
  let spaces1 = skip_many1 space in
  let parens = between (token "(") (token ")") in
  let iden = lexeme (many1 alpha_num => implode) in
  let var = iden => fun name -> AVar name in
  let rec abs input =
    begin
      token "\\" >>
      chainr1
        (iden => fun name body -> AAbs (name, body))
        (spaces1 => fun () outer inner body -> outer @@ inner body) >>= fun abs ->
      token "." >>
      term => fun body -> abs body
    end input
  and term input =
    chainl1
      (var <|> abs <|> parens term)
      (spaces1 => fun () rator rand -> AApp (rator, rand))
      input
  in term input

let term_of_ast term =
  let rec free_var_idx name idx = function
    | [] -> (idx, [name])
    | f::fs as vars ->
      if name = f then (idx, vars) else
        let (idx, fs) = free_var_idx name (idx + 1) fs
        in (idx, f::fs)
  in
  let rec var_idx name idx fs = function
    | [] -> free_var_idx name idx fs
    | b::bs ->  if name = b then (idx, fs) else var_idx name (idx + 1) fs bs
  in
  let rec term_of_ast fs bs = function
    | AVar name ->
      let (idx, fs) = var_idx name 0 fs bs
      in (evar (idx, name), fs)
    | AAbs (binder, body) ->
      let (body, fs) = term_of_ast fs (binder::bs) body
      in (eabs body, fs)
    | AApp (rator, rand) ->
      let (rator, fs) = term_of_ast fs bs rator in
      let (rand, fs) = term_of_ast fs bs rand
      in (eapp (rator, rand), fs)
  in term_of_ast [] [] term

let parse s =
  match LazyStream.of_string s |> parse term with
  | Some ast -> Some (term_of_ast ast)
  | None -> None
