(* Utility *)

let listInit n f =
  let rec helper i acc =
    if i < 0
    then acc
    else helper (i - 1) ((f i)::acc)
  in helper (n - 1) []

let explode str =
  listInit
    (String.length str)
    (fun i -> String.get str i)

let rec iterate f l =
  match f l with
  | Some (result, rest) ->
     (match rest with
      | [] -> [result]
      | _  -> result::(iterate f rest))
  | None -> []

(* Lexer *)

type token =
  | LeftParen
  | RightParen
  | RightArrow
  | Number of int

let nextToken = function
  (* Check against terminal tokens *)
  | ('('::xs) -> Some (LeftParen, xs)
  | (')'::xs) -> Some (RightParen, xs)
  | ('-'::'>'::xs) -> Some (RightArrow, xs)
  | _ -> None

