type var = {a : 'a. 'a}
type app = {b : 'a. 'a}
type abs = {c : 'a. 'a}

type _ term =
  | Var : int * string -> var term
  | App : 'a term * 'b term -> app term
  | Abs : 'a term -> abs term

type eterm = ETerm : 'a term -> eterm

let evar (idx, name) = ETerm (Var (idx, name))
let eabs (ETerm t) = ETerm (Abs t)
let eapp (ETerm rator, ETerm rand) = ETerm (App (rator, rand))
