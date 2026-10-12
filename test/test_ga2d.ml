open Geoalg.Ga2d.MultiVector

let one = make 1. 0. 0. 0.
let e1 = make 0. 1. 0. 0.
let e2 = make 0. 0. 1. 0.
let e12 = make 0. 0. 0. 1.
let u = make 1. 2. 3. 4.
let v = make 4. 3. 2. 1.
let w = make (-5.) 3. 2. 1.

let () =
  assert (e1 * e1 = one);
  assert (e2 * e2 = one);
  assert (e12 * e12 = make (-1.) 0. 0. 0.);
  assert (e1 * e2 = e12);
  assert (e2 * e1 = scale e12 (-1.));
  assert (e1 * e12 = e2);
  assert (e12 * e2 = e1);
  assert (one * e12 = e12);
  assert (e1 + e2 = make 0. 1. 1. 0.);
  assert (scale (make 1. 2. 3. 4.) 2. = make 2. 4. 6. 8.);
  assert (u * (v * w) = (u * v) * w);
  assert (u * (v + w) = u * v + u * w);
  assert (inverse e1 = e1);
  assert (inverse e12 = scale e12 (-1.));
  assert (inverse (make 2. 0. 0. 0.) = make 0.5 0. 0. 0.)

let () =
  assert (to_string (make 1. 2. 0. 4.) = "1 + 2e1 + 4e12");
  assert (to_string (make 0. (-1.) 3. 0.) = "-1e1 + 3e2");
  assert (to_string (make 0. 0. 0. 0.) = "0")
