(* NOT suitable for use as a standalone C preprocessor *)
open Preproc

let clear_ws s =
  let n = String.length s in
  let buf = Buffer.create n in
  let rec loop i =
    try
      let newline_pos = String.index_from s i '\n' in
      Buffer.add_char buf '\n';
      loop (newline_pos + 1)
    with Not_found ->
      (* no more '\n' *)
      Buffer.add_substring buf s i (n-i)
  in
  loop 0;
  Buffer.contents buf

let () =
  for i=1 to Array.length Sys.argv - 1 do
    let arg = Sys.argv.(i) in
    if String.length arg = 0 || arg.[0] = '-' then () else begin
      let filepath = arg in
      let ic = open_in filepath in
      let st = init_state ic filepath in
      let p = make_preproc_parser st in
      let rec loop () =
        let t = getsym_expand st.macro_tab p in
        print_string t.ws;
        print_string t.text;
        if t.kind = EOF then () else loop ()
      in
      try loop (); close_in ic
      with Failure msg ->
        Format.eprintf "Failure ‘%s’ raised at %a@."
          msg pp_pos st.lexbuf.lex_curr_p;
        exit 1
    end
  done
