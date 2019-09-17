open Result

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

let (>>=) = Result.bind


let rec iterate
          (f: ('a list -> ('b * ('a list), string) result))
          (l: 'a list)
        : ('b list, string) result = 
  match f l with
  | Ok (x, xs) ->
     (match xs with
      | [] -> Ok [xs]
      | _  -> map (List.cons x) (iterate f xs))
  | Error e -> Error e

(* Lexer *)

type token =
  | LeftParen
  | RightParen
  | RightArrow
  | Number of int

let nextToken = function
  (* Check against terminal tokens *)
  | ('('::xs) -> Ok (LeftParen, xs)
  | (')'::xs) -> Ok (RightParen, xs)
  | ('-'::'>'::xs) -> Ok (RightArrow, xs)
  | _ -> Error "Unrecognized char"

let lex = iterate nextToken
            
