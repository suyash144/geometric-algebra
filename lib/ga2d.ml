

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

end

