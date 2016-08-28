open Tyxml
open Types

let default x = function None -> x | Some x -> x

let abs_var_name (Abs body) =
  let rec abs_var_name : type a. a term -> int -> string option = fun term idx ->
    match term with
    | Abs t -> abs_var_name t (idx + 1)
    | Var (binder, name) -> if binder = idx then Some name else None
    | App (rator, rand) ->
      match abs_var_name rator idx with
      | None -> abs_var_name rand idx
      | x -> x
  in abs_var_name body 0

let string_of_term ?(parens=false) term =
  let string_of_abs_var (Abs _ as t) = default "_" (abs_var_name t) in
  let rec string_of_term : type a. a term -> int -> string = fun term pctx ->
    let precs prec s = if prec < pctx || parens then s else "" in
    match term with
    | Abs b as t -> precs 0 "(" ^ "\\" ^ string_of_abs_var t ^ string_of_abs b ^ precs 0 ")"
    | Var (_, name) -> name
    | App (rator, rand) ->
      precs 1 "(" ^
      string_of_term rator 1 ^ " " ^ string_of_term rand 2 ^
      precs 1 ")"
  and string_of_abs : type a. a term -> string = fun term ->
    match term with
    | Abs b as t -> " " ^ string_of_abs_var t ^ string_of_abs b
    | x -> ". " ^ string_of_term x 0

  in string_of_term term 0

let string_of_eterm (ETerm t) = string_of_term t

let root_beta : type a. abs term -> a term -> eterm = fun (Abs body) rand ->
  let rec raise_free : type a. int -> int -> a term -> a term = fun idx n term ->
    match term with
    | Abs b -> Abs (raise_free (idx + 1) n b)
    | Var (binder, name) as var -> if binder >= idx then Var (binder + n, name) else var
    | App (rator, rand) -> App (raise_free idx n rator, raise_free idx n rand)
  in
  let rec substitute : type a. int -> a term -> eterm = fun idx term ->
    match term with
    | Abs b -> eabs (substitute (idx + 1) b)
    | Var (binder, name) ->
      if binder = idx then ETerm (raise_free 0 idx rand) else evar (binder, name)
    | App (rator, rand) -> eapp (substitute idx rator, substitute idx rand)
  in substitute 0 body

let (<$>) f v = match v with None -> None | Some x -> Some (f x)

let rec left_outer_beta : type a. a term -> eterm option = function
  | Abs t -> eabs <$> left_outer_beta t
  | Var _ -> None
  | App (Abs _ as rator, rand) -> Some (root_beta rator rand)
  | App (rator, rand) ->
    match left_outer_beta rator with
    | None -> (fun rand -> eapp (ETerm rator, rand)) <$> left_outer_beta rand
    | rator -> (fun rator -> eapp (rator, ETerm rand)) <$> rator

let left_outer_beta' term = default (ETerm term) @@ left_outer_beta term

type content = [ |Svg_types.svg_content] Tyxml.Svg.elt

let boxes scale term =
  let rec boxes : type a. int -> int -> a term -> (content list * int * int) = fun binder left term ->
    let open Svg in
    let x v = a_x (float_of_int v, None) in
    let y v = a_y (float_of_int v, None) in
    let width v = a_width (float_of_int v, None) in
    let height v = a_height (float_of_int v, None) in
    let color = `Color ("black", None) in
    match term with
    | App (rator, rand) ->
      let (svg_rator, right, h_rator) = boxes binder left rator in
      let (svg_rand, right, h_rand) = boxes binder (right + 1 * scale) rand in
      (svg_rator @ svg_rand, right, max h_rator h_rand)
    | Var (_, _) ->
      ([rect ~a:[
           x left; y @@ binder * scale;
           width scale; height scale;
           a_fill color] []
        ], left + 1 * scale, 1)
    | Abs body ->
      let (svg_body, right, h) = boxes (binder + 1) (left + 1 * scale) body in
      let w = right - left + 1 * scale in
      let svg_abs = rect ~a:[
          x left; y @@ binder * scale;
          width w; height @@ (h + 2) * scale;
          a_fill `None;
          a_stroke (`Color ("black", None))] [] in
      (svg_abs :: svg_body, left + w, h + 2)
  in boxes 0 0 term |> fun (c, _, _) -> c

(* -- *)

let boxes' (ETerm t) = boxes 100 t
let the = function None -> raise @@ Failure "none" | Some x -> x
let ex1 = Parser.parse "\\z. (\\y. y (\\x. x)) (\\x. z x)" |> the |> fst

let out = open_out "test.svg"
let test = Svg.pp () (Format.formatter_of_out_channel out) (Svg.svg @@ boxes' ex1)
let () = close_out out

let y = Parser.parse "\\x y. x" |> the |> fst
let z = Parser.parse "x (x x)" |> the |> fst

