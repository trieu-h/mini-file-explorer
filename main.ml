open Unix;;
open Printf;;
open Stdlib;;

let str_compare s1 s2 =
  let s1 = s1 |> String.to_seq in
  let s2 = s2 |> String.to_seq in
  let cmp_fn c1 c2 =
    if c1 < c2 then -1
    else if c1 > c2 then 1
    else 0
  in
  Seq.compare cmp_fn s1 s2;;

let list_dirs path: string list =
  let dh = opendir path in
  let rec list_dirs_rec dirs =
    match readdir dh with
    | exception End_of_file -> closedir dh; List.sort str_compare dirs
    | "." -> list_dirs_rec dirs
    | dir -> list_dirs_rec (dir :: dirs)
  in
  list_dirs_rec [];;

type color = {r: int; g: int; b: int};;
let green = {r = 152; g = 195; b = 121};;
let yellow = {r = 255; g = 209; b = 115};;
let white = {r = 255; g = 255; b = 255};;
let gray = {r = 92; g = 97; b = 101};;
let black = {r = 0; g = 0; b = 0};;

let text_with_color fg bg text =
  sprintf "\x1b[38;2;%d;%d;%d;48;2;%d;%d;%dm%s\x1b[0m" fg.r fg.g fg.b bg.r bg.g bg.b text;;

let print_dirs list_dirs cur_dir focus_idx =
  let print_dir dir_index dir = 
    let { st_kind = kind } = Unix.stat (cur_dir ^ "/" ^ dir) in
    let focused = focus_idx == dir_index in
    let fg, bg = match kind, focused with 
    | S_DIR, true -> (yellow, gray)
    | S_DIR, false -> (yellow, black)
    | _, true -> (white, gray)
    | _, false -> (white, black)
    in
    text_with_color fg bg dir ^ "\n" |> print_string
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
let clear_screen () = print_string "\x1b[48;2;0;0;0m\x1b[2J\x1b[H";;
let set_cursor_pos ~x ~y = sprintf "\x1b[%d;%dH" y x |> print_string;;
let draw_border x y w h =
    set_cursor_pos ~x ~y;
    "╭" |> text_with_color green black |> print_string;

    set_cursor_pos ~x:(x+1) ~y;
    for i = 0 to w-1 do 
      "─" |> text_with_color green black |> print_string;
    done;

    set_cursor_pos ~x:(x+w+1) ~y;
    "╮" |> text_with_color green black|> print_string;

    for i = 0 to h do 
      set_cursor_pos ~x ~y:(y+h);
      "│" |> text_with_color green black |> print_string;
    done;

    set_cursor_pos ~x ~y:(y+h+1);
    "╰" |> text_with_color green black |> print_string;

    for i = 0 to h do 
      set_cursor_pos ~x:(x+w+1) ~y:(y+h);
      "│" |> text_with_color green black |> print_string;
    done;

    set_cursor_pos ~x:(x+w+1) ~y:(y+h+1);
    "╯" |> text_with_color green black |> print_string;

    set_cursor_pos ~x:(x+1) ~y:(y+h+1);
    for i = 0 to w-1 do 
      "─" |> text_with_color green black |> print_string;
    done;

type key = [
  |`Arrow_down
  |`Arrow_up
  |`Escape
  |`Enter
  |`Key of char
];;

let parse_input () =
  let buf = Bytes.create 3 in
  let bytes_read = read Unix.stdin buf 0 3 in
  if bytes_read > 1 then 
    let input = Bytes.sub_string buf 0 bytes_read in
    match input with
     | "\x1b[B" -> `Arrow_down
     | "\x1b[A" -> `Arrow_up
  else
    let input = Bytes.get buf 0 in
    match input with
    | '\n' -> `Enter
    | _ ->
      let input_bytes = Bytes.make 1 input in
      if input_bytes = Bytes.of_string "\x1b" then `Escape else `Key input;;
    
type entry_kind =
  | Dir
  | File;;

type mode =
  | Navigation
  | Delete of entry_kind
  | Create of entry_kind;;

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
      print_dirs !dirs !cur_dir !focus_idx;

      let () = match !mode with
        | Delete entry_kind -> 
          begin
            let kind = if entry_kind = Dir then "directory" else "file" in
            let confirm_msg = sprintf "Do you want to delete this %s? (y/n)" kind in
            let confirm_msg_len = String.length confirm_msg in
            let confirm_msg_x = (term_w - confirm_msg_len) / 2 in
            let confirm_msg_y = term_h / 2 in
            draw_border (confirm_msg_x-1) (confirm_msg_y-1) confirm_msg_len 1; 
            set_cursor_pos ~x:confirm_msg_x ~y:confirm_msg_y;
            print_string confirm_msg;
          end;
        | Create entry_kind ->
          begin
            let input_x = (term_w - user_input_max_w) / 2 in
            let input_y = term_h / 2 in

            draw_border (input_x-1) (input_y-1) user_input_max_w 1;

            set_cursor_pos ~x:(input_x+1) ~y:(input_y-1);
            let label = match entry_kind with 
              | Dir -> "Create directory"
              | File -> "Create file"
            in
            label |> text_with_color green black |> print_string;

            set_cursor_pos ~x:input_x ~y:input_y;
            print_string !user_input_view;

            show_cursor ();
          end;
        | _ -> ();
      in
      flush stdout;
      let key = parse_input () in
      let len = List.length !dirs in
      match !mode with
        | Navigation -> (
            match key with
            | `Arrow_down ->
                if !focus_idx < len - 1
                then focus_idx := !focus_idx + 1
                else focus_idx := 0
            | `Arrow_up ->
                if !focus_idx > 0
                then focus_idx := !focus_idx - 1
                else focus_idx := len - 1
            | `Key 'q' ->
                loop := false;
                restore_term_state ()
            | `Enter ->
                let selected_dir = List.nth !dirs !focus_idx in 
                cur_dir := !cur_dir ^ "/" ^ selected_dir;
                dirs := list_dirs !cur_dir;
                focus_idx := 0
            | `Key '-' ->
                cur_dir := !cur_dir ^ "/" ^ "..";
                dirs := list_dirs !cur_dir;
                focus_idx := 0
            | `Key 'd' ->
                let selected_entry = List.nth !dirs !focus_idx in
                let full_path = !cur_dir ^ "/" ^ selected_entry in
                let { st_kind = kind } = Unix.stat full_path in
                mode := begin match kind with
                  | S_REG -> Delete File
                  | _ -> Delete Dir
                end
            | `Key 'n' ->
                mode := Create File
            | `Key 'N' ->
                mode := Create Dir
            | _ -> ()
        )
        | Delete kind -> (
            match key with
            | `Enter | `Key 'y' ->
                let selected_entry = List.nth !dirs !focus_idx in
                let full_path = !cur_dir ^ "/" ^ selected_entry in
                begin
                  match kind with
                  | Dir -> sprintf "rm -rf %s" full_path |> Sys.command |> ignore;
                  | File -> unlink full_path;
                end;
                focus_idx := !focus_idx - 1;
                dirs := list_dirs !cur_dir;
                mode := Navigation
            | `Key 'q' | `Key 'n' | `Escape -> 
                mode := Navigation
            | _ -> ()
        )
        | Create kind ->
          match key with
            | `Enter ->
                if kind = Dir then (
                  sprintf "mkdir %s" !user_input |> Sys.command |> ignore;
                ) else (
                  sprintf "touch %s" !user_input |> Sys.command |> ignore;
                );
                user_input := "";
                user_input_view := "";
                hide_cursor();
                dirs := list_dirs !cur_dir;
                mode := Navigation
            | `Key key ->
                let user_input_len = String.length !user_input in
                begin
                  user_input := match key with
                    | '' when user_input_len > 0 -> 
                        String.sub !user_input 0 (user_input_len - 1)
                    | '' when user_input_len = 0 -> 
                        ""
                    | _  -> 
                        !user_input ^ String.make 1 key
                end;

                let user_input_len = String.length !user_input in
                user_input_view :=
                  if user_input_len >= user_input_max_w then
                    String.sub !user_input (user_input_len - user_input_max_w + 1) (user_input_max_w - 1)
                  else
                  !user_input
            | `Escape | `Key 'q' ->
                mode := Navigation
            | _ -> ()
    done;
  with
  | Sys.Break -> restore_term_state ();

