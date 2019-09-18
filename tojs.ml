open Result

(* Utility *)

let isDigit = function
  | '0' .. '9' -> true
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

let rec readWhile f = function
  | (c::cs) -> if f c
               then let (str, rest) = readWhile f cs
                    in (c::str, rest)
               else ([], (c::cs))
  | [] -> ([], [])
                
let nextToken = function
  (* Check against terminal tokens *)
  | ('('::cs) -> Ok (LeftParen, cs)
  | (')'::cs) -> Ok (RightParen, cs)
  | ('-'::'>'::cs) -> Ok (RightArrow, cs)
  (* Number *)
  | (c::cs) -> if   isDigit c
               then let (lst, rest) = (readWhile isDigit (c::cs))
                    in Ok (Number (int_of_string (implode lst)), rest)
               else Error "Unrecognized char"
  | [] -> Error "End of file"

let lex = iterate nextToken
