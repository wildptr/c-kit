open Util

module type S = sig
  val is_typename : string -> bool
  val initialize_typename_table : string list -> unit
  val enter_scope : unit -> unit
  val leave_scope : unit -> unit
  val register_typename : string -> unit
  val assume_typename : string -> unit
  val all_typenames : unit -> String_Set.t
  val register_variable_name : string -> unit
end

type name_kind = Type | Variable

module Make () : S = struct
  let name_stack : name_kind String_Map.t ref list ref = ref []

  let is_typename name =
    let rec lookup = function
      | [] -> false
      | map::stack ->
        begin match String_Map.find_opt name !map with
          | None -> lookup stack
          | Some Type -> true
          | Some Variable -> false
        end
    in lookup !name_stack

  let initialize_typename_table init_typenames =
    let seq =
      Seq.map (fun name -> (name, Type)) (List.to_seq init_typenames)
    in
    name_stack := [ref (String_Map.of_seq seq)]

  let enter_scope () =
    name_stack := ref (String_Map.empty) :: !name_stack

  let leave_scope () =
    name_stack := List.tl !name_stack

  let register_typename name =
    Printf.eprintf "typedef name: %s\n" name;
    let map = List.hd !name_stack in
    map := String_Map.add name Type !map

  let all_typenames () =
    List.fold_right begin fun map acc ->
      String_Map.fold begin fun name kind acc ->
        match kind with
        | Type -> String_Set.add name acc
        | Variable -> String_Set.remove name acc
      end !map acc
    end !name_stack String_Set.empty

  let assume_typename name =
(*  all_typenames () |> String_Set.iter begin fun name ->
      Printf.eprintf "  %s\n" name
    end;*)
    Printf.eprintf "assumed typedef name: %s\n" name;
    let map = List.hd !name_stack in
    map := String_Map.add name Type !map

  let register_variable_name name =
    Printf.eprintf "variable name: %s\n" name;
    let map = List.hd !name_stack in
    map := String_Map.add name Variable !map

end
