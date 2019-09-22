open Result

(* Utility *)

let compose f g = (fun x -> f (g x))
let (<.) = compose

let composeReverse f g = (fun x -> g (f x))
let (>.) = composeReverse
             
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
  | PlusSign
  | MinusSign
  | TimesSign
  | DivideSign
  | Number of int
  | Symbol of string

type ('a, 'b) continuation = 'a * 'b list

let rec pmap
          (f: 'a -> 'b)
          (c: ('a, 'n) continuation)
        : ('b, 'n) continuation =
  match c with
  | (a, rest) -> (f a, rest)
                                 
let rec readWhile f = function
  | (c::cs) -> if f c
               then pmap (fun str -> c::str) (readWhile f cs)
               else ([], (c::cs))
  | [] -> ([], [])

let rec nextToken = function
  (* Constant tokens *)
  | ('('::cs) -> Ok (LeftParen, cs)
  | (')'::cs) -> Ok (RightParen, cs)
  | ('+'::cs) -> Ok (PlusSign, cs)
  | ('-'::'>'::cs) -> Ok (RightArrow, cs)
  | ('-'::cs) -> Ok (MinusSign, cs)
  | ('*'::cs) -> Ok (TimesSign, cs)
  | ('/'::cs) -> Ok (DivideSign, cs)
  (* Variable tokens *)
  | (c::cs) ->
     (* Whitespace *)
     if isSpace c
     then nextToken cs
     (* Numbers *)
     else if isDigit c
     then Ok (pmap
                (fun lst -> Number (int_of_string (implode lst)))
                (readWhile isDigit (c::cs)))
     (* Symbols *)
     else if isSymbolic c
     then Ok (pmap
                (fun lst -> Symbol (implode lst))
                (readWhile (fun c -> isDigit c || isSymbolic c) (c::cs)))
     else Error (Printf.sprintf "Unrecognized char %c" c)
  | [] -> Error "End of file"

let lex = iterate nextToken <. explode

(* Parser *)
type ast =
  (* Atomic *)
  | Integer of int
  (* Math Operators *)
  | Plus   of { left: ast; right: ast; }
  | Minus  of { left: ast; right: ast; }
  | Times  of { left: ast; right: ast; }
  | Divide of { left: ast; right: ast; }
  (* Misc Operators *)
  | Arrow of { left: ast; right: ast; }
              
let rec parseAtom = function
  | (Number n)::rest -> Ok (Integer n, rest)
  | LeftParen::rest ->
     (parseTop rest) >>= (fun (expr, rest) ->
      match rest with
      | RightParen::rest -> Ok (expr, rest)
      | _ -> Error "Expected closing parentheses")
  | _ -> Error "Unexpected token"

and parseMultDiv toks =
  (parseAtom toks) >>= (fun (left, rest) ->
    let rec helper left toks =
      match toks with
      | TimesSign::rest -> (parseAtom rest) >>= (fun (right, rest) ->
          helper (Times { left = left; right = right; }) rest)
      | DivideSign::rest -> (parseAtom rest) >>= (fun (right, rest) ->
          helper (Divide { left = left; right = right; }) rest)
      | _ -> Ok(left, toks)
    in helper left rest)
               
and parsePlusMinus toks =
  (parseMultDiv toks) >>= (fun (left, rest) ->
    let rec helper left toks =
      match toks with
      | PlusSign::rest -> (parseMultDiv rest) >>= (fun (right, rest) ->
          helper (Plus { left = left; right = right; }) rest)
      | MinusSign::rest -> (parseMultDiv rest) >>= (fun (right, rest) ->
          helper (Minus { left = left; right = right; }) rest)
      | _ -> Ok(left, toks)
    in helper left rest)

and parseArrow toks =
  (parsePlusMinus toks) >>= (fun (left, rest) ->
    (match rest with
     | RightArrow::rest -> (parseArrow rest) >>= (fun (right, rest) ->
         Ok (Arrow { left = left; right = right; }, rest))
     | _ -> Ok (left, rest)))

and parseTop toks = parseArrow toks
                              
let parse = lex >. (fun res -> match res with
                               | Ok toks -> (iterate parseArrow toks)
                               | Error e -> Error e)

(* Compiler *)
let cOut = stdout

