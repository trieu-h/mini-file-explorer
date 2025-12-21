open Unix;;
open Printf;;
open Stdlib;;

let list_dirs path: string list =
  print_string path;
  flush stdout;
  let dh = opendir path in
  let rec list_dirs_rec dirs =
    match readdir dh with
    | exception End_of_file -> closedir dh; List.rev dirs
    | "." -> list_dirs_rec dirs
    | dir -> list_dirs_rec (dir :: dirs)
  in
  list_dirs_rec [];;

type color = { r: int; g: int; b: int };;

let green = {r = 152; g = 195; b = 121};;
let yellow = {r = 209; g = 154; b = 102};;
let white = {r = 255; g = 255; b = 255};;

let print_dirs list_dirs cur_dir focus_idx =
  let print_dir dir_index dir = 
    let { st_kind = kind } = Unix.stat (cur_dir ^ "/" ^ dir) in
    let focused = focus_idx == dir_index in
    let fg, bg = match kind, focused with 
    | S_DIR, true -> (white, Some green)
    | S_DIR, false -> (green, None)
    | S_REG, true -> (white, Some yellow)
    | S_REG, false -> (yellow, None)
    | _, _ -> failwith "Unreachable"
    in
    let text_with_color fg_color bg_color text =
      match fg_color, bg_color with
       | { r = fg_r; g = fg_g; b = fg_b}, None ->
           sprintf "\x1b[38;2;%d;%d;%dm%s\x1b[0m\n" fg_r fg_g fg_b text
       | { r = fg_r; g = fg_g; b = fg_b}, Some { r = bg_r; g = bg_g; b = bg_b } ->
           sprintf "\x1b[38;2;%d;%d;%d;48;2;%d;%d;%dm%s\x1b[0m\n" fg_r fg_g fg_b bg_r bg_g bg_b text
    in
    text_with_color fg bg dir |> print_string
  in
    List.iteri print_dir list_dirs;;

let set_raw_mode () =
  (* Raw mode must be enabled to handle character by character input *)
  let term = tcgetattr Unix.stdin in
  let raw_term = { term with c_icanon = false; c_echo = false } in
  raw_term.c_vmin <- 1;
  raw_term.c_vtime <- 1;
  tcsetattr Unix.stdin TCSANOW raw_term;;

let restore_terminal old_term =
  tcsetattr Unix.stdin TCSANOW old_term;;

let () = 
  Sys.catch_break true;
  let loop = ref true in
  let focus_idx = ref 0 in
  let old_term = tcgetattr Unix.stdin in
  let cur_dir = ref (getcwd ()) in
  let dirs = ref (list_dirs !cur_dir) in
  let restore_term_state () =
    print_string "\x1b[?25h"; (* Show cursor *)
    restore_terminal old_term; in
  set_raw_mode ();
  print_string "\x1b[?25l"; (* Hide cursor *)
  try 
    while !loop = true do
      printf "\x1b[2J\x1b[H";  (* Clear screen *)
      flush stdout;
      print_dirs !dirs !cur_dir !focus_idx;
      flush stdout;
      let buf = Bytes.create 3 in
      let bytes_read = read Unix.stdin buf 0 3 in
      if bytes_read > 1 then
        let len = List.length !dirs in
        match Bytes.sub_string buf 0 3 with
          (* Arrow down *)
          | "\x1b[B" -> if !focus_idx < len - 1
                        then focus_idx := !focus_idx + 1
                        else focus_idx := 0
          (* Arrow up *)
          | "\x1b[A" -> if !focus_idx > 0
                        then focus_idx := !focus_idx - 1
                        else focus_idx := len - 1;
          | _ -> ();
      else
        match Bytes.get buf 0 with
          | 'q' -> loop := false; restore_term_state ();
          | '\n' -> let selected_dir = List.nth !dirs !focus_idx in
                    cur_dir := !cur_dir ^ "/" ^ selected_dir;
                    dirs := list_dirs !cur_dir;
                    focus_idx := 0;
          | _ -> ();
      sleepf (1. /. 60.)
    done;
  with
  | Sys.Break -> restore_term_state ();

