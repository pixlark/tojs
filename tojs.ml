open Result

(* Utility *)

let compose f g = (fun x -> f (g x))
let (<<) = compose
       
let isDigit = function
  | '0' .. '9' -> true
  | _ -> false

let isSpace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let isSymbolic = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

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

let implode lst =
  String.init
    (List.length lst)
    (fun i -> List.nth lst i)
    
let (>>=) = Result.bind

let rec iterate
          (f : 'a list -> ('c * ('a list), string) result)
          (l : 'a list)
        : ('c list, string) result = 
  match f l with
  | Ok (got, remaining) ->
     (match remaining with
      | [] -> Ok [got]
      | _  -> map (List.cons got) (iterate f remaining))
  | Error e -> Error e

(* Lexer *)

type token =
  | LeftParen
  | RightParen
  | RightArrow
  | Number of int

type 'a continuation = 'a * char list

let rec pmap
          (f: 'a -> 'b)
          (c: 'a continuation)
        : 'b continuation =
  match c with
  | (a, rest) -> (f a, rest)
                                 
let rec readWhile f = function
  | (c::cs) -> if f c
               then pmap (fun str -> c::str) (readWhile f cs)
               else ([], (c::cs))
  | [] -> ([], [])

let rec nextToken = function
  (* Check against terminal tokens *)
  | ('('::cs) -> Ok (LeftParen, cs)
  | (')'::cs) -> Ok (RightParen, cs)
  | ('-'::'>'::cs) -> Ok (RightArrow, cs)
  (* Number *)
  | (c::cs) ->
     (* Whitespace *)
     if isSpace c
     then nextToken cs
     (* Numbers *)
     else if isDigit c
     then Ok (pmap
                (fun lst -> Number (int_of_string (implode lst)))
                (readWhile isDigit (c::cs)))
     else if isSymbolic c
     then Ok (pmap
                (fun lst -> Symbol (implode lst))
                (readWhile (fun c -> isDigit c || isSymbolic c) (c::cs)))
     else Error "Unrecognized char"
  | [] -> Error "End of file"

let lex = iterate nextToken << explode
