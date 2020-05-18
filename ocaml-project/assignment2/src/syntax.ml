type expr =
    | Var of string
    | Lam of string * expr
    | App of expr * expr

let string_of_expr x =
  let rec str : (expr -> string) = function
  | Var s -> s
  | Lam (s, x) -> "λ" ^ s ^ "." ^ (str x)
  | App (x, y) ->
      let x' =
        match x with
        |  Lam _ -> "(" ^ (str x) ^ ")"
        | _ -> str x
      in
      let y' =
        match y with
        | App _ -> "(" ^ (str y) ^ ")"
        | Lam _ -> "(" ^ (str y) ^ ")"
        | _ -> str y
      in
      x' ^ " " ^ y'
  in
  str x
