type expression =
	  Funktion of funktion
	| Apply of (expression*expression)

and funktion =
	  I
	| Dot of char
	| K1 of funktion
	| K
	| S2 of (funktion*funktion)
	| S1 of funktion
	| S
	| V
	| D1 of expression
	| D
	| Cont of (funktion->unit)
	| Dcont of (funktion->unit)
	| C
	| E
	| P
	| F
	| At
	| Ques of char
	| Pipe
;;

let current_ch = ref (None : char option);;

exception NoTopLevelReset;;

let meta_cont = ref [];;

let abort v = match !meta_cont with
    [] -> raise NoTopLevelReset
  | cont::rest -> (meta_cont := rest; cont v);;

let rec apply = function
	  I -> (fun t -> fun cont -> cont t)
	| Dot ch -> (fun t -> fun cont -> print_char ch; flush stdout; cont t)
	| K1 v -> (fun t -> fun cont -> cont v)
	| K -> (fun t -> fun cont -> cont (K1 t))
	| S2 (x,y) -> (fun z -> fun cont ->
			eval (Apply (Apply (Funktion x,Funktion z),
			      Apply (Funktion y,Funktion z))) cont)
	| S1 x -> (fun y -> fun cont -> cont (S2 (x,y)))
	| S -> (fun x -> fun cont -> cont (S1 x))
	| V -> (fun t -> fun cont -> cont V)
	| D1 e -> (fun t -> fun cont -> eval (Apply (e, Funktion t)) cont)
(*	| D -> raise your_expectations *)
	| Cont cnt -> (fun t -> fun cont -> cnt t)
	| C -> (fun t -> fun cont ->
			 eval (Apply (Funktion t, Funktion (Cont cont))) cont)
	| E -> (fun t -> fun cont -> exit 0)
	| P -> (fun t -> fun cont -> reset (Apply (Funktion t, Funktion I)) cont)
	| F -> (fun t -> fun cont ->
	  eval (Apply (Funktion t, Funktion (Dcont cont))) abort)
	| Dcont dcnt -> fun t -> fun cont ->
	  reset (Apply (Funktion (Cont dcnt), Funktion t)) cont
	| At -> (fun t -> fun cont ->
			  (current_ch := (try Some (input_char stdin)
;					  with End_of_file -> None);
				eval (Apply (Funktion t, Funktion ((function
			    None -> V
			  | Some _ -> I) (!current_ch)))) cont))
	| Ques ch -> (fun t -> fun cont ->
			  (eval (Apply (Funktion t, Funktion ((function
			    None -> V
			  | Some ch2 -> if ch=ch2 then I else V)
					(!current_ch)))) cont))
	| Pipe -> (fun t -> fun cont ->
			  (eval (Apply (Funktion t, Funktion ((function
			    None -> V
			  | Some ch -> Dot ch) (!current_ch)))) cont))
and eval = function
	  Funktion f -> fun cont -> cont f
	| Apply (rator, rand) -> fun cont -> eval rator (function D ->
		  cont (D1 rand)
		| erator -> eval rand (fun erand -> apply erator erand cont))
and reset expr cont =
	meta_cont := cont :: !meta_cont;
	(eval expr abort)
;;

exception ParseError;;

let rec parse = fun input ->
	( function
	     '#' -> let rec gobble () = ((function
		  '\n' -> parse input
		| _ -> gobble ())
			(input_char input))
		in gobble ()
	   | ' ' -> parse input
	   | '\n' -> parse input
	   | '\t' -> parse input
	   | '\r' -> parse input
	   | '`' -> Apply (let e=(parse input) in (e,(parse input)))
	   | 'i' -> Funktion I
	   | 'I' -> Funktion I
	   | 'k' -> Funktion K
	   | 'K' -> Funktion K
	   | 's' -> Funktion S
	   | 'S' -> Funktion S
	   | 'v' -> Funktion V
	   | 'V' -> Funktion V
	   | 'd' -> Funktion D
	   | 'D' -> Funktion D
	   | 'c' -> Funktion C
	   | 'C' -> Funktion C
	   | 'e' -> Funktion E
	   | 'E' -> Funktion E
	   | 'p' -> Funktion P
	   | 'P' -> Funktion P
	   | 'f' -> Funktion F
	   | 'F' -> Funktion F
	   | '.' -> Funktion (Dot (input_char input))
	   | 'r' -> Funktion (Dot '\n')
	   | '@' -> Funktion At
	   | '?' -> Funktion (Ques (input_char input))
	   | '|' -> Funktion Pipe
	   | _ -> raise ParseError
	) (input_char input)
;;

let program = match Array.length Sys.argv with
  | 1 -> parse stdin
  | 2 -> let f = open_in Sys.argv.(1) in
         let e = parse f in
         close_in f; e
  | _ -> raise (Failure "Expected zero or one argument")
;;

eval program (fun x -> ())
;;
