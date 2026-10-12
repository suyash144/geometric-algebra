



module MultiVector = struct
  type t = {
    s : float;
    e1 : float ;
    e2 : float ;
    e12 : float ;
  }

  type grade = G0 | G1 | G2

  let make s e1 e2 e12 = {s ; e1; e2; e12}

  let ( * ) a b = {
    s = a.s *. b.s 
    +. a.e1 *. b.e1 
    +. a.e2 *. b.e2 
    -. a.e12 *. b.e12;

    e1 = a.s *. b.e1 
    +. a.e1 *. b.s 
    -. a.e2 *. b.e12 
    +. a.e12 *. b.e2;

    e2 = a.s *. b.e2 
    +. a.e1 *. b.e12 
    +. b.s *. a.e2 
    -. a.e12 *. b.e1;

    e12 = a.s *. b.e12
    +. a.e1 *. b.e2
    -. a.e2 *. b.e1 
    +. a.e12 *. b.s;
  }

  let ( + ) a b = {
    s = a.s +. b.s;
    e1 = a.e1 +. b.e1;
    e2 = a.e2 +. b.e2;
    e12 = a.e12 +. b.e12;
  }

  let scale a scalar = {
    s = a.s *. scalar;
    e1 = a.e1 *. scalar;
    e2 = a.e2 *. scalar;
    e12 = a.e12 *. scalar;
  }

  let neg a = scale a (-1.)

  let ( - ) a b = a + neg b

  let reverse a = {
    s = a.s;
    e1 = a.e1;
    e2 = a.e2;
    e12 = -. a.e12;
  }

  let zero = make 0. 0. 0. 0.

  let grade_project a = function
  | G0 -> {a with e1 = 0.; e2 = 0.; e12 = 0.}
  | G1 -> {a with s = 0.; e12 = 0.}
  | G2 -> {a with e1 = 0.; e2 = 0.; s = 0.}

  let wedge a b = {
    s = a.s *. b.s;
    e1 = a.s *. b.e1 +. a.e1 *. b.s;
    e2 = a.s *. b.e2 +. a.e2 *. b.s;
    e12 = a.s *. b.e12 +. a.e1 *. b.e2 
    -. a.e2 *. b.e1 +. a.e12 *. b.s;
  }

  let scalar_dot a b = grade_project (a * b) G0

  let norm a = sqrt(((reverse a) * a).s)

  let grade_invol a = {
    s = a.s;
    e1 = -. a.e1;
    e2 = -. a.e2;
    e12 = a.e12;
  }

  let clifford_conj a = grade_invol (reverse a)

  let inverse a =
    let conj = clifford_conj a in 
    scale conj (1. /. (a * conj).s)

  let int_of_grade = function 
    | G0 -> 0
    | G1 -> 1
    | G2 -> 2

  let grade_of_int = function
    | 0 -> G0
    | 1 -> G1
    | 2 -> G2
    | n -> invalid_arg (Printf.sprintf "grade_of_int: %d" n)

  let grades = [G0; G1; G2]

  let left_contract a b = 
    List.concat_map (fun r -> List.map (fun s -> (r, s)) grades) grades
    |> List.filter (fun (r, s) -> int_of_grade r <= int_of_grade s)
    |> List.map (fun (r, s) ->
      let out = grade_of_int Stdlib.(int_of_grade s - int_of_grade r) in 
      grade_project (grade_project a r * grade_project b s) out)
    |> List.fold_left (+) zero

  let to_string a =
    let terms =
      [ (a.s, ""); (a.e1, "e1"); (a.e2, "e2"); (a.e12, "e12") ]
      |> List.filter (fun (c, _) -> c <> 0.)
    in
    match terms with
    | [] -> "0"
    | (c0, n0) :: rest ->
      let term c n = Printf.sprintf "%g%s" (Float.abs c) n in
      let first = (if c0 < 0. then "-" else "") ^ term c0 n0 in
      let others =
        List.map (fun (c, n) -> (if c < 0. then " - " else " + ") ^ term c n) rest
      in
      String.concat "" (first :: others)

  let pp fmt a = Format.pp_print_string fmt (to_string a)

end

