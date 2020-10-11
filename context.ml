open Util

module type S = sig
  val is_typename : string -> bool
  val initialize_typename_table : string list -> unit
  val enter_scope : unit -> unit
  val leave_scope : unit -> unit
  val register_typename : string -> unit
  val assume_typename : string -> unit
  val all_typenames : unit -> String_Set.t
end

module Make () : S = struct
  let typename_stack : String_Set.t ref list ref = ref []

  let is_typename name =
    let rec lookup = function
      | [] -> false
      | set::stack -> String_Set.mem name !set || lookup stack
    in lookup !typename_stack

  let initialize_typename_table init_typenames =
    typename_stack := [ref (String_Set.of_list init_typenames)]

  let enter_scope () =
    typename_stack := ref (String_Set.empty) :: !typename_stack

  let leave_scope () =
    typename_stack := List.tl !typename_stack

  let register_typename name =
    Printf.eprintf "typedef name: %s\n" name;
    let set = List.hd !typename_stack in
    set := String_Set.add name !set

  let assume_typename name =
    Printf.eprintf "assumed typedef name: %s\n" name;
    let set = List.hd !typename_stack in
    set := String_Set.add name !set

  let all_typenames () =
    List.fold_left (fun acc set -> String_Set.union acc !set)
      String_Set.empty !typename_stack

end
