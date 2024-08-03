module IdMap =
  struct
    include Map.Make (Id)

    let hash_fold_t f s m =
      fold (fun k v s -> f (Id.hash_fold_t s k) v)
        m s

    let pp pp_v fmt m =
      iter (fun k v ->
          Format.pp_print_char fmt '(';
          Id.pp fmt k;
          Format.pp_print_char fmt ',';
          pp_v fmt v;
          Format.pp_print_char fmt ')')
        m

    let show show_v m =
      let buf = Buffer.create 0 in
      let fmt = Format.formatter_of_buffer buf in
      pp (fun fmt v -> Format.pp_print_string fmt (show_v v)) fmt m;
      Format.pp_print_flush fmt ();
      Buffer.contents buf
  end

module IdHash =
  Hashtbl.Make (
      struct
        type t = Id.t
        let equal = Id.equal
        let hash = Id.hash
      end)
