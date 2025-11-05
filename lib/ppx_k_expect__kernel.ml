let ( %> ) f1 f2 = fun s -> (f1 s) |> f2
let __re__group__get i groups = Re.Group.get groups i

let option__bindre f x =
  match x with
  | None -> f ()
  | Some x -> Some x

let option__bind f x = Option.bind x f

module type UNIX = module type of Unix
module type SYS = module type of Sys
module type FORMAT = module type of Format
module type OUT_CHAN = module type of Out_channel
module type IN_CHAN  = module type of In_channel
module type PRINTEXC = module type of Printexc

let undo_relative_indentation ~min_padding expect_str = if min_padding <= 0 then expect_str else begin
  expect_str
  |> String.split_on_char '\n'
  |> List.fold_left (fun acc x ->
    if String.for_all (function ' ' -> true | _ -> false) x then x :: acc else
    String.sub x min_padding (String.length x - min_padding) :: acc
  ) []
  |> List.rev
  |> String.concat "\n"
end

let redo_relative_indentation ~min_padding result = if min_padding <= 0 then result else begin
  result
  |> String.split_on_char '\n'
  |> List.fold_left (fun acc x ->
    if String.for_all (function ' ' -> true | _ -> false) x then x :: acc else
    ((String.init min_padding (fun _ -> ' ')) ^ x) :: acc
  ) []
  |> List.rev
  |> String.concat "\n"
end

let clip_text =
  let get_decochars =
    let pattern = Re.(
      seq [ char '\027'; char '['; group (any |> rep |> shortest); char 'm' ]
    ) |> Re.compile in
    Re.all pattern
    %> List.map Re.Group.all
    %> List.map (fun groups -> ((3 + String.length groups.(1)), ())) in
  fun ~maxwidth ->
  String.split_on_char '\n'
  %> List.map (fun x ->
    let offset = get_decochars x |> List.fold_left (fun acc (len, ()) -> len + acc) 0 in
    String.sub x 0 (Int.min (String.length x) (maxwidth + offset))
  )
  %> String.concat "\n"

let mkdir_laxed path perm =
  let is_directory x =
    try Sys.is_directory x
    with Sys_error _ -> false in
  if is_directory path then ()
  else Sys.mkdir path perm

module Text = struct
  type pos =
    { pos_lnum : int
    ; pos_cnum : int
    ; pos_bol  : int
    }

  type loc =
    { loc_start : pos
    ; loc_end   : pos
    }

  module Loc = struct
    let colnum loc = loc.pos_cnum - loc.pos_bol
  end

  let extract
  pos text =
    if pos.loc_start.pos_lnum = pos.loc_end.pos_lnum then begin
      let lnum = pos.loc_start.pos_lnum
      and cnum_start = Loc.colnum pos.loc_start
      and cnum_end   = Loc.colnum pos.loc_end in
      [List.nth text (lnum - 1) |> fun s -> String.sub s cnum_start (cnum_end - cnum_start)]
    end else begin
    ( [List.nth text (pos.loc_start.pos_lnum - 1) |> fun s -> String.sub s (Loc.colnum pos.loc_start) (String.length s - (Loc.colnum pos.loc_start))]
    @ List.init (pos.loc_end.pos_lnum - pos.loc_start.pos_lnum - 1) (fun i ->
        let lnum = i + pos.loc_start.pos_lnum in
        List.nth text lnum
      )
    @ [List.nth text (pos.loc_end.pos_lnum - 1) |> fun s -> String.sub s 0 (Loc.colnum pos.loc_end)]
    )
    end

  let replace
  delpos ~subst text =
    if delpos.loc_start.pos_lnum = delpos.loc_end.pos_lnum then begin
      List.mapi (fun i x -> match i with
      | i when i = (delpos.loc_start.pos_lnum - 1) ->
        (String.sub x 0 (Loc.colnum delpos.loc_start)) ^ subst ^ (String.sub x (Loc.colnum delpos.loc_end) (String.length x - (Loc.colnum delpos.loc_end)))
      | _ -> x) text
    end else begin
    text
    |> List.mapi (fun i x -> (i, x))
    |> List.fold_left (fun acc (i, x) ->
      match i with
      | i when i < (delpos.loc_start.pos_lnum - 1) ->
        x :: acc
      | i when i = (delpos.loc_start.pos_lnum - 1) ->
        let x = String.sub x 0 (Loc.colnum delpos.loc_start) ^ subst in
        x :: acc
      | i when i > (delpos.loc_start.pos_lnum - 1) && i < (delpos.loc_end.pos_lnum - 1) ->
        acc
      | i when i = (delpos.loc_end.pos_lnum - 1) ->
        let x = String.sub x (Loc.colnum delpos.loc_end) (String.length x - (Loc.colnum delpos.loc_end)) in
        ( match acc with 
        | []               -> [x]
        | [lastline]       -> [lastline ^ x]
        | lastline :: rest -> (lastline ^ x) :: rest
        )
      | i when i > (delpos.loc_end.pos_lnum - 1) ->
        x :: acc
      | _ -> failwith "wdsf"
    ) []
    |> List.rev
    end

end

(** @reference https://www.reddit.com/r/C_Programming/comments/v5k3z1/reading_char_for_char_from_stdin_without_waiting/ *)
let getch
(module Unix : UNIX) (module Format : FORMAT) (module In_channel : IN_CHAN) () =
  let oldattr = Unix.tcgetattr (Unix.descr_of_in_channel stdin) in
  let newattr = { oldattr with c_icanon = false; c_echo = true; c_vmin = 1; c_vtime = 0 } in
  Unix.tcsetattr (Unix.descr_of_in_channel stdin) Unix.TCSANOW newattr;
  let v = In_channel.input_char stdin in
  Unix.tcsetattr (Unix.descr_of_in_channel stdin) Unix.TCSANOW oldattr;
  Format.eprintf "\n";
  v

let dict
(module Sys : SYS) (module In_channel : IN_CHAN) () =
  let dotenv_file = Sys.getcwd () ^  "/.env" in
  if Sys.file_exists dotenv_file then Some (
    let dotenv = In_channel.open_text dotenv_file in
    let dict = In_channel.input_all dotenv
      |> String.split_on_char '\n'
      |> List.filter_map @@ fun str ->
        match String.split_on_char '=' str with
        | [a; b] -> Some (a, b)
        | [""]   -> None
        | _ -> failwith "wths" in
    In_channel.close dotenv; dict
  ) else None

let dict__getenv__string
(module Sys : SYS) varname =
  option__bind (List.assoc_opt varname)
  %> option__bindre (fun () -> Sys.getenv_opt varname)

let dict__getenv__bool
(module Sys : SYS) varname =
  option__bind (List.assoc_opt varname)
  %> option__bindre (fun () -> Sys.getenv_opt varname)
  %> function
    | Some "y" -> true
    | Some _ | None -> false

let dict__getenv__int
(module Sys : SYS) varname =
  option__bind (List.assoc_opt varname)
  %> option__bindre (fun () -> Sys.getenv_opt varname)
  %> Option.map int_of_string

let dict__getcwd
(module Sys : SYS) =
  option__bind (List.assoc_opt "CWD")
  %> option__bindre (Sys.getcwd %> Option.some)

let pretty_trim =
  let pattern = Re.(
    seq [bos; group (space |> rep |> greedy); group (any |> rep |> shortest); group (space |> rep |> greedy); eos]
  ) |> Re.compile in
  fun str ->
  let groups = Re.exec pattern str in
  let pad_left = Re.Group.get groups 1 in
  let content = Re.Group.get groups 2 in
  let pad_right = Re.Group.get groups 3 in
  content, (pad_left, pad_right)

let get_padding =
  let pattern =
    let open Re in
    (seq [bos; group (space |> rep |> greedy); notnl |> rep |> shortest; eos])
    |> compile in
  Re.exec pattern
  %> __re__group__get 1

let path__minimize (module Sys : SYS) dict =
  let home__username = dict__getcwd (module Sys : SYS) dict in
  match home__username with None -> Fun.id | Some home__username ->
  fun s ->
  if String.starts_with ~prefix:home__username s
  then String.sub s (String.length home__username + 1) (String.length s - (String.length home__username) - 1)
  else s

