



module MultiVector = struct
  type t = {
    s : float;
    e1 : float ;
    e2 : float ;
    e12 : float ;
  }
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

  let reverse a = {
    s = a.s;
    e1 = a.e1;
    e2 = a.e2;
    e12 = -1. *. a.e12;
  }

  type basis = S | E1 | E2 | E12

  let grade_project a = function
  | S -> {a with e1 = 0.; e2 = 0.; e12 = 0.}
  | E1 -> {a with s = 0.; e2 = 0.; e12 = 0.}
  | E2 -> {a with e1 = 0.; s = 0.; e12 = 0.}
  | E12 -> {a with e1 = 0.; e2 = 0.; s = 0.}


end

