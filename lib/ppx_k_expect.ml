let ( %> ) f1 f2 = fun s -> (f1 s) |> f2

open Ppxlib

module type SYS = module type of Sys
module type IN_CHAN  = module type of In_channel

let expand__expect_test ~enabled ~ctxt =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let (module A) = Ast_builder.make loc in
  function
  | [ { pstr_desc = Pstr_value ( rec_, [ { pvb_pat = { ppat_desc = Ppat_constant ( Pconst_string (name, _, _)) ; _ } ; pvb_expr = e ; pvb_attributes = _ ; pvb_loc = _ } ]) ; pstr_loc  = _ } ] ->
  ignore rec_;
  if not enabled then A.pstr_eval [%expr ()] [] else
  [%expr try
    Ppx_k_expect__kernel.mkdir_laxed "_build" 0o777;
    Ppx_k_expect__kernel.mkdir_laxed "_build/kexpect" 0o777;
    let id = ref 0 in
    let ch = ref None in
    let it_end () =
      let id_ = !id in
      let ch_, _ = !ch |> Option.get in
      id := id_ + 1;
      Out_channel.close ch_;
      let path = "_build/kexpect/out-" ^ (Int.to_string id_) in
      let ch_ = Out_channel.open_text path in
      Format.set_formatter_out_channel ch_;
      ch := Some (ch_, path);
      in
    let path = "_build/kexpect/out-" ^ (Int.to_string !id) in
    let ch_ = Out_channel.open_text path in
    Format.set_formatter_out_channel ch_;
    ch := Some (ch_, path);
    ((fun ch -> [%e e]) (object
      val dict = Ppx_k_expect__kernel.dict (module Sys) (module In_channel) ()
      method testname = [%e A.estring name]
      method path = let _, x = !ch |> Option.get in x
      method runtime_dict = dict
      method it_end () = it_end ()
    end)) |> fun ret ->
    let ch_, _ = !ch |> Option.get in
    Format.set_formatter_out_channel stdout;
    Out_channel.close ch_; ret
    (* ... *)
    with
    | Failure x -> Format.eprintf "K-expect exception: Failure: %s\n" x; exit 112
    | x -> Format.eprintf "K-expect exception: %s\n" (Printexc.to_string x); exit 112
  ] |> fun e ->
    A.pstr_eval e []
  | _ -> failwith "fuck"

let expand__expect module__Sys dict =
  let path__minimize = Ppx_k_expect__kernel.path__minimize module__Sys dict in
  fun ~enabled ~ctxt ->
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let srcfile = Expansion_context.Extension.code_path ctxt |> Code_path.file_path in
  let (module A) = Ast_builder.make loc in
  fun expect_str loc _x ->
  let min_padding = if String.for_all (fun x -> not (x = '\n')) expect_str then 0 else
    expect_str
    |> String.split_on_char '\n'
    |> List.fold_left (fun acc x ->
      if String.for_all (function ' ' -> true | _ -> false) x then acc else
      let n = Ppx_k_expect__kernel.get_padding x |> String.length in
      if n < acc then n else acc
    ) Int.max_int in
  let expect_str = Ppx_k_expect__kernel.undo_relative_indentation ~min_padding expect_str in
  let expect_str, (pad_left, pad_right) = Ppx_k_expect__kernel.pretty_trim expect_str in
  let pad_left, pad_right = if expect_str |> String.for_all ((=) ' ') then (" ", " ") else pad_left, pad_right in
  if not enabled then [%expr ()] else
  [%expr (* val ch : < .. > *)
    let module K = Ppx_k_expect__kernel in
    Format.print_flush ();
    let result = In_channel.with_open_text ch#path In_channel.input_all in
    Format.set_formatter_out_channel stderr;
    let expect_str = [%e A.estring expect_str] in
    if not (String.equal result expect_str) then begin
      let open Ppx_k_expect__kernel.Text in
      let srcfile = [%e A.estring srcfile] in
      let srcfile__minimized = [%e A.estring (srcfile |> path__minimize)] in
      let text = In_channel.with_open_text srcfile In_channel.input_all in
      let text' = text |> String.split_on_char '\n' in
      let loc = { loc_start = { pos_lnum = [%e A.eint loc.loc_start.pos_lnum] ; pos_cnum = [%e A.eint loc.loc_start.pos_cnum] ; pos_bol = [%e A.eint loc.loc_start.pos_bol] } ; loc_end = { pos_lnum  = [%e A.eint loc.loc_end.pos_lnum] ; pos_cnum = [%e A.eint loc.loc_end.pos_cnum] ; pos_bol = [%e A.eint loc.loc_end.pos_bol] } } in
      let result' = K.redo_relative_indentation ~min_padding:[%e A.eint min_padding] result in
      let text_new = K.Text.replace loc ~subst:([%e A.estring pad_left] ^ result' ^ [%e A.estring pad_right]) text'
        |> String.concat "\n" in
      match Patdiff.(
        let prev = { Diff_input.name = "prev" ; text }
        and next = { Diff_input.name = "next" ; text = text_new } in
        Compare_core.diff_strings Configuration.default ~prev ~next
      ) with
      | `Same -> failwith "??"
      | `Different msg ->
      let msg =
        K.dict__getenv__int (module Sys) "TEXTWIDTH" ch#runtime_dict
        |> Option.fold ~none:msg ~some:(fun maxwidth -> K.clip_text ~maxwidth msg) in
      Format.eprintf "In file '%s' test '%s':\n%s\nPromote? [\027[1;34my\027[0m/\027[1;34m\027[4;34mn\027[0m] " srcfile__minimized ch#testname msg;
      Format.print_flush ();
      match K.getch (module Unix) (module Format) (module In_channel) () with
      | Some 'y' ->
        let out_channel__output_string text_new f = Out_channel.output_string f text_new in
        Out_channel.with_open_text srcfile @@ out_channel__output_string text_new;
      | Some _ | None ->
        let a, b = if String.exists ((=) '\n') expect_str then ("\n", "\n") else (" '", "' ") in
        let c, d = if String.exists ((=) '\n') result then ("\n", "\n") else (" '", "'\n") in
        let msg = Format.sprintf "in file '%s' test '%s' at line %d expecting%s%s%sgot%s%s%s" srcfile ch#testname [%e A.eint loc.loc_start.pos_lnum] a expect_str b c result d in
        raise (Failure msg)
    end;
    ch#it_end ();
  ]

let () =
  let env = Ppx_k_expect__kernel.dict (module Sys) (module In_channel) () in
  let enabled = Ppx_k_expect__kernel.dict__getenv__bool (module Sys) "KEXPECT_TEST" env in
  let expect_test =
    Extension.V3.declare
      "expect_test"
      Extension.Context.structure_item
      Ast_pattern.(pstr __)
      (expand__expect_test ~enabled)
    |> Context_free.Rule.extension
  and expect =
    Extension.V3.declare
      "expect"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ __ __)))
      (expand__expect (module Sys) env ~enabled)
    |> Context_free.Rule.extension in
  Driver.register_transformation
    ~rules:[expect_test; expect] "ppx-k-expect"

