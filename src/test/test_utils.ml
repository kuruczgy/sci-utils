open Base

let () =
  let open Utils.Units in
  let open Option.Monad_infix in
  let v = Qty.{ v = 5.0; u = { s = -1; m = 1; kg = 0 } } in
  let t = Qty.{ v = 10.0; u = { s = 1; m = 0; kg = 0 } } in
  let s2 = Qty.{ v = 15.0; u = { s = 0; m = 1; kg = 0 } } in
  let res = Some Qty.(v * t) >>= fun s -> Qty.(s + s2) in
  Option.iter res ~f:(fun q -> Stdlib.print_endline Qty.(to_string q))

let () =
  let open Utils.Units in
  let qty = Qty.of_string "0.1g" in
  let qty = Option.map qty ~f:Qty.to_string in
  Stdlib.print_endline (Option.value qty ~default:"[error]")

let () =
  let open Utils.Bom in
  let m1 =
    QuantityOfMaterial.of_alist 5.0
      [ (Prop.Carbohydrate, 0.0); (Prop.Energy, 1.0); (Prop.Fat, 4.0) ]
  in
  let m2 =
    QuantityOfMaterial.of_alist 15.0
      [ (Prop.Carbohydrate, 1.0); (Prop.Energy, 2.0) ]
  in
  let m = QuantityOfMaterial.combine m2 m1 in
  QuantityOfMaterial.show m |> Stdlib.print_endline