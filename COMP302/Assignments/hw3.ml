(* TODO: Write some tests for tabulate. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let tabulate_tests: (((int -> int) * int) * int list) list = [
  (* Remember: your test cases should have this form:
     ((f, n), output)
     The following test case asserts that:
       tabulate (fun x -> x) (-1)
     should have the output []
  *)
  (((fun x -> x), -1), []);
  (((fun x -> x+1), 0), [1]);
  (((fun x -> 3*x), 2), [0; 3; 6])
] 

(* TODO: Implement dist_table: (int * int) -> int -> float list *)
let dist_table ((marblesTotal, marblesDrawn): (int * int)) (x: int) : float list =
  tabulate(fun n -> dist_black n x (marblesTotal, marblesDrawn)) marblesTotal

(* TODO: Write some test cases for is_empty. *)
let is_empty_tests: (float list list * bool) list = [
  ([[];[]], true);
  ([[]], true);
  ([[1.0];[0.3]], false)
]

(* TODO: Implement is_empty: 'a list list -> bool *)
let is_empty (matrix: 'a list list) : bool =
  List.for_all(fun x -> x = []) matrix
    

(* TODO: Implement dist_matrix: int * int -> int list -> float list list *)
let dist_matrix ((total, drawn): int * int) (resultList: int list) : float list list =
  List.map(fun n -> dist_table (total, drawn) n) resultList

(* TODO: Implement combined_dist_table: float list list -> float list *)
let rec combined_dist_table (matrix: float list list) =
  if is_empty matrix then
    []  (* Return an empty list if the distribution matrix is empty *)
  else
    List.fold_left (fun acc row ->
        List.map2 (fun a b -> a *. b) acc row
      ) (List.hd matrix) (List.tl matrix)

(* Once you have implemented all the above functions, you can
   use this function to compute the maximum likelihood.
   You can try it on the given example by running:
     max_likelihood (6, 3) [2; 0; 1]
*)
let max_likelihood (total, drawn) resultList =
  max_in_list
    (combined_dist_table
       (dist_matrix (total, drawn) resultList))


(* TODO: Implement all: (ingredients list -> bool) -> cake -> bool *)
let rec all (p: (ingredients list -> bool)) (c: cake) : bool = 
  match c with
  |Slice ingredients -> p ingredients
  |Cake (c1, c2) -> all p c1 && all p c2

(* TODO: Write some test cases for is_chocolate_cake. *)
let is_chocolate_cake_tests = [
  (Slice[Chocolate], true);
  (Slice[], false);
  (Cake(Slice[Chocolate], Slice[Almonds]), false)
]

(* TODO: Implement is_chocolate_cake: cake -> bool *)
let rec aux (ing: ingredients list) : bool = 
  match ing with
  |[] -> false
  |h::t -> if h == Chocolate then true
      else aux t
  
let is_chocolate_cake (c: cake) : bool = 
  match c with
  |Slice ingredients -> if ingredients == [] then false
      else aux ingredients
  |Cake (c1, c2) -> all(aux) c1 && all aux c2


(* TODO: Implement map: (ingredients list -> ingredients list) -> cake -> cake *)
let rec map (p: (ingredients list -> ingredients list)) (c: cake) = 
  match c with
  |Slice ingredients -> Slice(p ingredients)
  |Cake (c1, c2) -> Cake (map p c1, map p c2)
  

(* TODO: Write some test cases for add_ingredient. *)
let add_ingredient_tests = [
  ((Chocolate,Slice[]), Slice[Chocolate]);
  ((Chocolate, Cake(Slice[Chocolate], Slice[])), Cake(Slice[Chocolate], Slice[Chocolate]))
]

let ingredient_insert (ing: ingredients)(ingredients: ingredients list) =
  if ingredients = [] then ing::ingredients
  else if List.exists(fun w -> w == ing) ingredients then ingredients
  else ingredients @ [ing]
                     
(* TODO: Implement add_ingredient: ingredients -> cake -> cake *)
let add_ingredient (x: ingredients) (c: cake) : cake = 
  match c with
  |Slice ingredients -> Slice(ingredient_insert x ingredients)
  |Cake (c1,c2) -> Cake(map (ingredient_insert (x)) c1, map (ingredient_insert(x)) c2)
  

(* TODO: Implement fold_cake: (ingredients list -> 'a -> 'a) -> 'a -> cake -> 'a  *)
let rec fold_cake (f: (ingredients list -> 'a -> 'a)) (base: 'a) (c: cake) : 'a = 
  match c with
  |Slice ingredients -> f ingredients base
  |Cake(c1, c2) -> let newbase = fold_cake f base c1 
      in fold_cake f newbase c2


(* TODO: Implement get_all_ingredients: cake -> ingredients list *)
let get_all_ingredients (c: cake) : ingredients list = 
  raise NotImplemented