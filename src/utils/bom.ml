open Base

(*

Use cases:
- Describe a recipe with a mix of masses, volumes, and pieces.
- Scale such a recipe to a target total mass/volume.
- Sum properties of the ingredients of the recipe (energy, nutrition info, price).
- Be able to derive missing measures for materials.
- Be able to search for a match for a generic ingredient from a database.

Material properties:
- inv_density (m^3/kg)
- energy/mass (J/kg)
- price (1/kg)
- glycemic load (1/kg)
- carbohydrates (kg/kg)
- fat (kg/kg)
- fiber (kg/kg)
- protein (kg/kg)
*)

module Prop = struct
  module T = struct
    type t =
      | Inv_density (* m^3/kg *)
      | Energy (* J/kg *)
      | Price (* 1/kg *)
      | GL (* 1/kg *)
      | Carbohydrate (* kg/kg *)
      | Fat (* kg/kg *)
      | Fiber (* kg/kg *)
      | Protein (* kg/kg *)
    [@@deriving compare, sexp_of, show]
  end

  include T
  include Comparator.Make (T)
end

module QuantityOfMaterial = struct
  let pp_prop_float = [%derive.show: (Prop.t * float) list]

  type t = {
    qty : float; (* kg *)
    props : (Prop.t, float, Prop.comparator_witness) Map.t;
        [@printer
          fun fmt v -> fprintf fmt "%s" (pp_prop_float (Map.to_alist v))]
  }
  [@@deriving show]

  let combine a b =
    let qty = a.qty +. b.qty in
    {
      qty;
      props =
        a.props |> Map.to_alist
        |> List.filter_map ~f:(fun (k, va) ->
               Option.map (Map.find b.props k) ~f:(fun vb ->
                   (k, ((va *. a.qty) +. (vb *. b.qty)) /. qty)))
        |> Map.of_alist_exn (module Prop);
    }

  let of_alist qty l = { qty; props = Map.of_alist_exn (module Prop) l }

  let sexp_of_t t =
    Sexp.(
      List
        [
          List [ Atom "qty"; Atom (Float.to_string t.qty) ];
          List
            [
              Atom "props";
              List
                (Map.to_alist t.props
                |> List.map ~f:(fun (k, v) ->
                       List [ Prop.sexp_of_t k; Float.sexp_of_t v ]));
            ];
        ])
end