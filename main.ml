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

let show_cursor () = print_string "\x1b[?25h";;
let hide_cursor () = print_string "\x1b[?25l";;
let clear_screen () = printf "\x1b[2J\x1b[H";;
let set_cursor_pos ~x ~y = sprintf "\x1b[%d;%dH" y x |> print_string;;
    
type delete_kind =
  | Dir
  | File;;

type mode =
  | Navigation
  | Delete of delete_kind
  | Create

let () = 
  Sys.catch_break true;
  let loop = ref true in
  let focus_idx = ref 0 in
  let term_h = Unix.open_process_in "tput lines" |> input_line |> int_of_string in
  let term_w = Unix.open_process_in "tput cols"  |> input_line |> int_of_string in
  let old_term = tcgetattr Unix.stdin in
  let cur_dir = ref (getcwd ()) in
  let dirs = ref (list_dirs !cur_dir) in
  let mode = ref Navigation in
  let user_input = ref "" in
  let user_input_view = ref "" in
  let user_input_max_w = 50 in
  let restore_term_state () =
    clear_screen ();
    show_cursor ();
    restore_terminal old_term;
  in
  set_raw_mode ();
  hide_cursor ();
  try 
    while !loop = true do
      clear_screen ();
      flush stdout;

      print_dirs !dirs !cur_dir !focus_idx;
      flush stdout;

      let () = match !mode with
        | Delete delete_kind -> 
          begin
            let m_h = term_h / 2 in
            let text = if delete_kind = Dir then "directory" else "file" in
            let confirm_msg = sprintf "Do you want to delete this %s? (y/n)" text in
            let confirm_msg_len = String.length confirm_msg in
            let start_pos = (term_w - confirm_msg_len) / 2 in

            let x = start_pos and y = m_h - 1 in set_cursor_pos ~x ~y;
            for i = 1 to confirm_msg_len do print_string "─"; done;

            let x = start_pos - 1 and y = m_h - 1 in set_cursor_pos ~x ~y;
            print_string "╭";

            let x = start_pos - 1 and y = m_h in set_cursor_pos ~x ~y;
            print_string "│";

            let x = start_pos - 1 and y = m_h + 1 in set_cursor_pos ~x ~y;
            print_string "╰";

            let x = start_pos and y = m_h in set_cursor_pos ~x ~y;
            print_string confirm_msg;

            let x = start_pos + confirm_msg_len and y = m_h - 1 in set_cursor_pos ~x ~y;
            print_string "╮";

            let x = start_pos + confirm_msg_len and y = m_h in set_cursor_pos ~x ~y;
            print_string "│";

            let x = start_pos + confirm_msg_len and y = m_h + 1 in set_cursor_pos ~x ~y;
            print_string "╯";

            let x = start_pos and y = m_h + 1 in set_cursor_pos ~x ~y;
            for i = 1 to confirm_msg_len do print_string "─"; done;

            flush stdout;
          end;
        | Create ->
          begin
            let m_h = term_h / 2 in
            let start_pos = (term_w - user_input_max_w) / 2 in

            let x = start_pos and y = m_h - 1 in set_cursor_pos ~x ~y;
            for i = 1 to user_input_max_w do print_string "─"; done;

            let x = start_pos - 1 and y = m_h - 1 in set_cursor_pos ~x ~y;
            print_string "╭";

            let x = start_pos - 1 and y = m_h in set_cursor_pos ~x ~y;
            print_string "│";

            let x = start_pos - 1 and y = m_h + 1 in set_cursor_pos ~x ~y;
            print_string "╰";

            let x = start_pos + user_input_max_w and y = m_h - 1 in set_cursor_pos ~x ~y;
            print_string "╮";

            let x = start_pos + user_input_max_w and y = m_h in set_cursor_pos ~x ~y;
            print_string "│";

            let x = start_pos + user_input_max_w and y = m_h + 1 in set_cursor_pos ~x ~y;
            print_string "╯";

            let x = start_pos and y = m_h + 1 in set_cursor_pos ~x ~y;
            for i = 1 to user_input_max_w do print_string "─"; done;

            let x = start_pos and y = m_h in set_cursor_pos ~x ~y;
            print_string !user_input_view;

            show_cursor ();

            flush stdout;
          end;
        | _ -> ();
      in
      let buf = Bytes.create 3 in
      let bytes_read = read Unix.stdin buf 0 3 in
      if bytes_read > 1 then
        let len = List.length !dirs in
        let usr_input = Bytes.sub_string buf 0 3 in
        match usr_input, !mode with
          | "\x1b[B", Navigation -> (* Arrow down *)
              if !focus_idx < len - 1
              then focus_idx := !focus_idx + 1
              else focus_idx := 0;
          | "\x1b[A", Navigation -> (* Arrow up *)
              if !focus_idx > 0
              then focus_idx := !focus_idx - 1
              else focus_idx := len - 1;
          | _, _ -> ();
      else
        let usr_input = Bytes.get buf 0 in
        match usr_input, !mode with
          | 'q', Navigation -> 
              loop := false;
              restore_term_state ();
          | '\n', Navigation ->
              let selected_dir = List.nth !dirs !focus_idx in 
              cur_dir := !cur_dir ^ "/" ^ selected_dir;
              dirs := list_dirs !cur_dir;
              focus_idx := 0;
          | '-', Navigation ->
              cur_dir := !cur_dir ^ "/" ^ "..";
              dirs := list_dirs !cur_dir;
              focus_idx := 0;
          | 'd', Navigation ->
              let selected_entry = List.nth !dirs !focus_idx in
              let full_path = !cur_dir ^ "/" ^ selected_entry in
              let { st_kind = kind } = Unix.stat full_path in
              mode := begin match kind with
                | S_REG -> Delete File
                | S_DIR -> Delete Dir
                | _ -> failwith "Not handled"
              end
          | 'n', Navigation ->
              mode := Create;
          | 'y', Delete delete_kind ->
              let selected_entry = List.nth !dirs !focus_idx in
              let full_path = !cur_dir ^ "/" ^ selected_entry in
              begin
                match delete_kind with
                | Dir -> sprintf "rm -rf %s" full_path |> Sys.command |> ignore;
                | File -> unlink full_path;
              end;
              focus_idx := !focus_idx - 1;
              dirs := list_dirs !cur_dir;
              mode := Navigation;
          | 'q', Delete _ | 'n', Delete _ -> 
              mode := Navigation;
          | '\n', Create ->
              sprintf "mkdir %s" !user_input |> Sys.command |> ignore;
              user_input := "";
              user_input_view := "";
              hide_cursor();
              dirs := list_dirs !cur_dir;
              mode := Navigation;
          | c, Create ->
              let user_input_len = String.length !user_input in
              begin
                user_input := match c with
                  | '' when user_input_len > 0 -> 
                      String.sub !user_input 0 (user_input_len - 1)
                  | '' when user_input_len = 0 -> 
                      ""
                  | _  -> 
                      !user_input ^ String.make 1 c
              end;
              user_input_view := 
                if user_input_len > user_input_max_w then 
                  String.sub !user_input (user_input_len - user_input_max_w - 1) (user_input_max_w - 1)
                else
                  !user_input
          | _, _-> ();
      (* sleepf (1. /. 60.) *)
    done;
  with
  | Sys.Break -> restore_term_state ();

