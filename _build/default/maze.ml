(* Affiche la liste des commandes disponibles pour l'utilisateur *)
let usage () =
  print_endline "Maze Generator & Solver Usage:";
  print_endline "  ./maze.exe print <file.laby>           : Display a maze";
  print_endline "  ./maze.exe solve <file.laby>           : Solve and display the path";
  print_endline "  ./maze.exe random <width> <height>     : Generate a random maze";
  print_endline "  ./maze.exe test                        : Run unit tests";
  print_endline "  ./maze.exe help                        : Display this help message"


(* Tente de charger un labyrinthe, le valide et l'affiche si tout est correct *)
let run_print filename =
  try
    let g = Grid.from_file filename in
    if Grid.check g then begin
      let (h, w) = Grid.get_dimensions g in
      Printf.printf "Valid maze of size %d x %d:\n" w h;
      Grid.print g
    end else
      print_endline "Error: The maze file is invalid (wrong shape or missing S/E)."
  with Sys_error msg ->
    print_endline ("System error: " ^ msg)
  | _ ->
    print_endline "Error reading the file."


(* Tente de charger un labyrinthe, le valide et essaie de le résoudre *)
let run_solve filename =
  try
    let g = Grid.from_file filename in
    if Grid.check g then begin
      if Grid.solve g then
        Grid.print g
      else
        print_endline "No solution found."
    end else
        print_endline "Error: The maze file is invalid."
  with Sys_error msg ->
    print_endline ("System error: " ^ msg)
  | _ ->
    print_endline "Error during resolution."

let main () =
  let args = Array.to_list Sys.argv in
  match args with
  | _ :: "print" :: filename :: [] ->
      run_print filename
  | _ :: "solve" :: filename :: [] ->
      run_solve filename
  | _ :: "random" :: w_str :: h_str :: seed_str :: [] ->
      let w = int_of_string w_str in
      let h = int_of_string h_str in
      let seed = int_of_string seed_str in
      Random.init seed;
      let g = Grid.generate w h in
      Grid.print g
  | _ :: "test" :: [] ->
      Test.run_all ()

  | _ :: "help" :: _ | _ :: "--help" :: _ ->
      usage ()
  | _ ->
      usage ()

let () = main ()