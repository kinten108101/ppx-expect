module Diff = Patdiff

module Text = struct
  module type SYS = module type of Sys
  module type OUT_CHAN = module type of Out_channel
  module type IN_CHAN  = module type of In_channel

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

