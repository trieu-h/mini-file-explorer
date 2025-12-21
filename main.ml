open Unix;;
open Printf;;
open Stdlib;;

let list_dirs () : string list =
  let cwd = getcwd () in
  let dh = opendir cwd in
  let rec list_dirs_rec dirs =
    match readdir dh with
    | exception End_of_file -> closedir dh; List.rev dirs
    | "." -> list_dirs_rec dirs
    | dir -> list_dirs_rec (dir :: dirs)
  in
  list_dirs_rec [];;

let print_dirs list_dirs =
  let print_dir = printf "%s\n" in
  List.iter print_dir list_dirs;;

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
  let old_term = tcgetattr Unix.stdin in
  let dirs = list_dirs () in
  set_raw_mode ();
  try 
    while !loop = true do
      printf "\x1b[2J\x1b[H";  (* Clear screen *)
      flush stdout;
      print_dirs dirs;
      flush stdout;
      let buf = Bytes.create 1 in
      let bytes = read Unix.stdin buf 0 1 in
      match Bytes.get buf 0 with
        | 'q' -> loop := false; restore_terminal old_term;
        | _ -> ();
      sleepf (1. /. 60.) 
    done;
  with
  | Sys.Break -> restore_terminal old_term;

