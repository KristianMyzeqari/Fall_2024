(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* Q1 *)
(* TODO: Write a good set of tests for compress *)
let compress_tests = [
  ([],[]);
  ([A],[1,A]);
  ([A;A;A;A;T;T;T;T;G;G;G;G;C;C;C;C],[(4, A); (4, T); (4, G); (4, C)]);
]

(* TODO: Implement compress. *)
let compress (l : nucleobase list) : (int * nucleobase) list =
  let rec compress_r current list acc count =
    match list with
    |[] -> ((count, current) :: acc)
    |h::t -> if h = current then
          compress_r h t acc (count + 1)
        else 
          compress_r h t ((count, current) :: acc) 1
  in match l with
  |[] -> []
  |h::t ->  List.rev(compress_r h t [] 1)

(* TODO: Write a good set of tests for decompress *)
let decompress_tests = [
  ([],[]);
  ([(1,A)], [A]);
  ([(4, A)], [A;A;A;A])
]

(* TODO: Implement decompress. *)
let decompress (l : (int * nucleobase) list) : nucleobase list =
  let rec decompress_r cur_int list acc =
    match list with
    |[] -> (acc)
    |(c, n)::t -> 
        if cur_int = 0 then
          decompress_r 1 t acc
        else
          decompress_r (c-1) (((c-1),n)::t) (n::acc)
  in match l with
  |[] -> []
  |(c, n)::t -> List.rev(decompress_r c l [])

(* Q2 *)
(* TODO: Write a good set of tests for eval *)
let eval_tests = [
  (PLUS(FLOAT 2.0, FLOAT 2.0), 4.0);
  (MULT(FLOAT 2.0, FLOAT 2.0), 4.0);
  (COS(FLOAT 0.0), 1.0);
  (EXP(FLOAT 0.0), 1.0);
  (MINUS(PLUS(MULT(FLOAT 1.0, FLOAT 2.0), FLOAT 2.0),FLOAT 1.0), 3.0);
  (FLOAT 0.0, 0.0)
]

(* TODO: Implement eval. *)
let rec eval e =
  match e with
  |PLUS(x, y) -> (eval x) +. (eval y)
  |MINUS(x, y) -> (eval x) -. (eval y)
  |MULT(x, y) -> (eval x) *. (eval y)
  |DIV(x, y) -> (eval x) /. (eval y)
  |SIN(x) -> sin(eval x)
  |COS(x) -> cos(eval x)
  |EXP(x) -> exp(eval x)
  |FLOAT(x) -> x

(* TODO: Write a good set of tests for to_instr *)
let to_instr_tests = [
  (FLOAT 1.0, [Float 1.0]);
  (PLUS(FLOAT 1.0, FLOAT 1.0), [Float 1.0; Float 1.0; Plus]);
  (COS(FLOAT 0.0), [Float 0.0; Cos])
]

(* TODO: Implement to_instr. *)
let rec to_instr e = 
  match e with
  |FLOAT(x) -> [Float x]
  |PLUS(x, y) -> to_instr x @ to_instr y @ [Plus]
  |MINUS(x, y) -> to_instr x @ to_instr y @ [Minus]
  |MULT(x, y) -> to_instr x @ to_instr y @ [Mult]
  |DIV(x, y) -> to_instr x @ to_instr y @ [Div]
  |SIN(x) -> to_instr x @ [Sin]
  |COS(x) -> to_instr x @ [Cos]
  |EXP(x) -> to_instr x @ [Exp]

(* TODO: Write a good set of tests for instr *)
let instr_tests = [
  ((Mult, [5.0; 5.5]), Some [27.5]);
  ((Cos, [0.0]), Some[1.0]);
  ((Float 1.0, [2.0]), Some[1.0; 2.0])
]


(* TODO: Implement to_instr. *)               
let instr (i: instruction) (s: stack) : stack option =
  match i with
  | Float f -> Some (f::s)  (* Push a float onto the stack *) 
  | Sin -> (match s with
      |h :: t -> Some(sin(h) :: t)
      |[] -> Some(s)) 
  | Cos -> (match s with
      |h :: t -> Some(cos(h) :: t)
      |[] -> Some(s)) 
  | Exp -> (match s with
      |h :: t -> Some(exp(h) :: t)
      |[] -> Some(s)) 
  | Plus -> 
      if List.length s < 2 then None
      else (match s with
          | x :: y :: t -> Some ((y +. x)::t)
          |_ -> Some (s)) 
  | Minus -> 
      if List.length s < 2 then None
      else (match s with
          | x :: y :: t -> Some ((y -. x)::t)
          |_ -> Some (s)) 
  | Mult -> 
      if List.length s < 2 then None
      else (match s with
          | x :: y :: t -> Some ((y *. x)::t)
          |_ -> Some (s)) 
  | Div -> 
      if List.length s < 2 then None
      else
        (* Handle division by zero *)
        let divide s =
          match s with
          | x1 :: x2 :: t -> if x1 = 0.0 then None
              else Some ((x2 /. x1) :: t)
          | _ -> None
        in
        divide s 

(* TODO: Write a good set of tests for prog *)
let prog_tests = [
  ([Float 2.0; Float 1.0; Mult], Some 2.0);
  ([Float 0.0; Cos; Float 2.0; Mult], Some 2.0); 
]

(* TODO: Implement prog. *)
let rec exec (insts: instruction list) (stk: stack) : stack option =
  match insts with
  | [] -> Some stk  (* If no instructions left, return the stack *) 
  | h :: t ->
      match instr h stk with
      | None -> None  (* If an instruction fails, return None *)
      | Some new_stack -> exec t new_stack

let prog (insts: instruction list) : float option =
  match exec insts [] with
  | Some [result] -> Some result
  | _ -> None
               
