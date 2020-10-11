module String_Table =
  Hashtbl.Make
    (struct
      type t = string
      let equal = String.equal
      let hash = Hashtbl.hash
    end)

module String_Set = Set.Make(String)