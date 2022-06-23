open Base

let map_inverse_exn cmp m =
  Map.of_alist_exn cmp (List.map (Map.to_alist m) ~f:(fun (a, b) -> (b, a)))

module DerivedUnit = struct
  module T = struct
    type t = { s : int; m : int; kg : int } [@@deriving compare, sexp_of]
  end

  module Comp = struct
    include T
    include Comparator.Make (T)
  end

  include Comp

  let comp_to_string k v =
    match v with 0 -> "" | 1 -> k | _ -> k ^ "^" ^ Int.to_string v

  let to_string u =
    comp_to_string "s" u.s ^ comp_to_string "m" u.m ^ comp_to_string "kg" u.kg

  let combine f a b = { s = f a.s b.s; m = f a.m b.m; kg = f a.kg b.kg }
  let apply f a = { s = f a.s; m = f a.m; kg = f a.kg }

  let abbrev_map =
    Map.of_alist_exn
      (module String)
      [
        ("s", ({ s = 1; m = 0; kg = 0 }, 0));
        ("m", ({ s = 0; m = 1; kg = 0 }, 0));
        ("g", ({ s = 0; m = 0; kg = 1 }, -3));
        ("l", ({ s = 0; m = 3; kg = 0 }, -3));
        ("Pa", ({ s = -2; m = -1; kg = 1 }, 0));
        ("N", ({ s = -2; m = 1; kg = 1 }, 0));
        ("J", ({ s = -2; m = 2; kg = 1 }, 0));
      ]

  let of_abbrev s = Map.find abbrev_map s

  let to_abbrev =
    Map.to_alist abbrev_map
    |> List.map ~f:(fun (a, (u, e)) -> (u, (a, e)))
    |> Map.of_alist_exn (module Comp)
    |> Map.find
end

module Units = struct
  open DerivedUnit

  let kg = { s = 0; m = 0; kg = 1 }
  let m3 = { s = 0; m = 3; kg = 0 }
  let _J = { s = -2; m = 2; kg = 1 }
end

module Qty = struct
  type group_op = Add | Sub
  type field_op_mul = Mul | Div
  type field_op = Add | Sub | M of field_op_mul

  let f_of_field_op op =
    match op with
    | Add -> ( +. )
    | Sub -> ( -. )
    | M Mul -> ( *. )
    | M Div -> ( /. )

  let f_of_log_field_op (op : field_op_mul) =
    match op with Mul -> ( + ) | Div -> ( - )

  type t = { v : float; u : DerivedUnit.t }

  let op_add op a b =
    if Poly.(a.u = b.u) then Some { v = (f_of_field_op op) a.v b.v; u = a.u }
    else None

  let op_mul op a b =
    {
      v = (f_of_field_op (M op)) a.v b.v;
      u = DerivedUnit.combine (f_of_log_field_op op) a.u b.u;
    }

  let ( + ) = op_add Add
  let ( - ) = op_add Sub
  let ( * ) = op_mul Mul
  let ( / ) = op_mul Div

  let ( ** ) q e =
    { v = q.v **. Float.of_int e; u = DerivedUnit.apply (Int.( * ) e) q.u }

  let prefix_map =
    Map.of_alist_exn
      (module String)
      [
        ("Y", 24);
        ("Z", 21);
        ("E", 18);
        ("P", 15);
        ("T", 12);
        ("G", 9);
        ("M", 6);
        ("k", 3);
        ("", 0);
        ("m", -3);
        ("u", -6);
        ("n", -9);
        ("p", -12);
        ("f", -15);
        ("a", -18);
        ("z", -21);
        ("y", -24);
      ]

  let exp_of_prefix p = Map.find_exn prefix_map p

  let prefix_of_exp =
    let prefix_map_inv = map_inverse_exn (module Int) prefix_map in
    fun e -> Map.find_exn prefix_map_inv e

  let si_exp v ex =
    Float.(
      v |> abs |> log10
      |> Float.( + ) (Float.of_int ex)
      |> clamp_exn ~min:(-24.) ~max:24.
      |> (fun v -> v /. 3.)
      |> round_down |> to_int |> Int.( * ) 3)

  let to_string { v; u } =
    let name, unit_ex =
      Option.value (DerivedUnit.to_abbrev u)
        ~default:(DerivedUnit.to_string u, 0)
    in
    let si_ex = si_exp v (-unit_ex) in
    let v = v /. (10. **. Float.of_int Int.(si_ex + unit_ex)) in
    Printf.sprintf "%.2f%s%s" v (prefix_of_exp si_ex) name

  let re_qty = Re.Pcre.regexp {|^([\d\.]+)\s*([YZEPTGMkmunpfazy]|)_?(\w+)$|}

  let of_string s =
    Option.bind (Re.exec_opt re_qty s) ~f:(fun g ->
        let get = Re.Group.get g in
        let v = Float.of_string (get 1) in
        let prefix_exp = exp_of_prefix (get 2) in
        Option.map
          (DerivedUnit.of_abbrev (get 3))
          ~f:(fun (u, unit_exp) ->
            let e = Int.(prefix_exp + unit_exp) in
            let v = v *. Float.(10. ** of_int e) in
            { v; u }))

  let grab q u = if Poly.(q.u = u) then Some q.v else None
end
