open Ppxlib

let split_by_newline =
  let pattern = Re.(
    char '\n'
  ) |> Re.compile in
  Re.split_delim pattern

let split_by_equal =
  let pattern = Re.(
    char '='
  ) |> Re.compile in
  Re.split_delim pattern

let dict =
  let dotenv_file = Sys.getcwd () ^  "/.env" in
  (* print_endline dotenv_file; *)
  if Sys.file_exists dotenv_file then Some (
    let dotenv = open_in dotenv_file in
    let dict = In_channel.input_all dotenv
      |> split_by_newline
      |> List.filter_map @@ fun str ->
        match split_by_equal str with
        | [a; b] -> Some (a, b)
        | [""]   -> None
        | _ -> failwith "wths" in
    close_in dotenv; dict
  ) else None

let enabled =
  match dict with
  | Some dict -> (
    match List.assoc_opt "KEXPECT_TEST" dict with
    | Some "y" -> true
    | Some _ | None -> false
  )
  | None ->
    match Sys.getenv_opt "KEXPECT_TEST" with
    | Some "y" -> true
    | Some _ | None -> false

let expand__expect_test ~ctxt =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let (module A) = Ast_builder.make loc in
  fun struct_ ->
  match struct_ with
  | [{ pstr_desc = Pstr_value (rec_, [{pvb_pat = { ppat_desc = Ppat_constant (Pconst_string (name, _, _)) ; _ }; pvb_expr = e; pvb_attributes = _; pvb_loc = _}]); pstr_loc = _ }] ->
    ignore rec_;
    A.pstr_eval (
      if enabled then
        [%expr
          try
            let mkdir path perm =
              let is_directory x =
                try Sys.is_directory x
                with Sys_error _ -> false in
              if is_directory path then ()
              else Sys.mkdir path perm
              in
            mkdir "_build" 0o777;
            mkdir "_build/kexpect" 0o777;
            let id = ref 0 in
            let ch = ref None in
            let it_end () =
              let id_ = !id in
              let ch_, _ = !ch |> Option.get in
              id := id_ + 1;
              close_out ch_;
              let path = "_build/kexpect/out-" ^ (Int.to_string id_) in
              let ch_ = open_out path in
              Format.set_formatter_out_channel ch_;
              ch := Some (ch_, path);
              in
            let path = "_build/kexpect/out-" ^ (Int.to_string !id) in
            let ch_ = open_out path in
            Format.set_formatter_out_channel ch_;
            ch := Some (ch_, path);
            let ret = (fun ch -> [%e e]) (object method testname = [%e A.estring name] method path = let _, x = !ch |> Option.get in x method it_end () = it_end () end) in
              let ch_, _ = !ch |> Option.get in
            Format.set_formatter_out_channel stdout; close_out ch_; ret
          with
          | Failure x -> Printf.eprintf "K-expect exception: Failure: %s" x; exit 112
          | x -> Printf.eprintf "K-expect exception: %s\n" (Printexc.to_string x); exit 112
        ]
      else [%expr ()]
    )
    []
  | _ -> failwith "fuck"

let expand__expect ~ctxt =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let ff = Expansion_context.Extension.code_path ctxt |> Code_path.file_path in
  (* Printf.eprintf "ff: '%s'\n" ff; *)
  let (module A) = Ast_builder.make loc in
  fun expect_str loc _x ->
  let expect_str = String.trim expect_str in
  if enabled then
    [%expr
      Format.print_flush ();
      let result_file = open_in ch#path in
      let result = In_channel.input_all result_file in
      close_in result_file;
      Format.set_formatter_out_channel stderr;
      let expect_str = [%e A.estring expect_str] in
      if not (String.equal result expect_str) then begin
        let open Ppx_k_expect__kernel in
        let open Text in
        let ff = [%e A.estring ff] in
        let text = In_channel.with_open_text ff In_channel.input_all in
        let text' = text |> String.split_on_char '\n' in
        let loc =
          { loc_start =
            { pos_lnum  = [%e A.eint loc.loc_start.pos_lnum]
            ; pos_cnum  = [%e A.eint loc.loc_start.pos_cnum]
            ; pos_bol   = [%e A.eint loc.loc_start.pos_bol]
            }
          ; loc_end =
            { pos_lnum  = [%e A.eint loc.loc_end.pos_lnum]
            ; pos_cnum  = [%e A.eint loc.loc_end.pos_cnum]
            ; pos_bol   = [%e A.eint loc.loc_end.pos_bol]
            }
          } in
        let sec = Text.replace loc ~subst:(" " ^ result ^ " ") text' in
        match Diff.(
          let prev = { Diff_input.name = "prev"; text }
          and next = { Diff_input.name = "next"; text = sec |> String.concat "\n" } in
          Compare_core.diff_strings Configuration.(override ~line_big_enough:5 default) ~prev ~next
        ) with
        | `Same -> failwith "??"
        | `Different msg ->
        Format.eprintf "In file '%s' test '%s':\n%s\nPromote? [y/n] " ff ch#testname msg;
        Format.print_flush ();
        match In_channel.input_line stdin with
        | Some "n" ->
          raise (Failure (Printf.sprintf "test '%s' did not pass. in file '%s'. at line %d. expecting \"%s\". got \"%s\"\n" ch#testname ff [%e A.eint loc.loc_start.pos_lnum] expect_str result))
        | Some "y" ->
          Out_channel.with_open_text ff (fun f -> Out_channel.output_string f (sec |> String.concat "\n"));
        | Some x -> failwith (Printf.sprintf "unknown response '%s'" x)
        | None   -> failwith ("unknown response '\n'")
      end;
      ch#it_end ();
      ()
    ]
  else [%expr ()]

let () =
  let expect_test =
    Extension.V3.declare
      "expect_test"
      Extension.Context.structure_item
      Ast_pattern.(pstr __)
      expand__expect_test
    |> Context_free.Rule.extension
  and expect =
    Extension.V3.declare
      "expect"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ __ __)))
      expand__expect
    |> Context_free.Rule.extension in
  Driver.register_transformation
    ~rules:[expect_test; expect] "ppx-k-expect"

