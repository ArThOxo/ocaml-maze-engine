(* Affiche "OK" si le test réussit, ou "FAIL" s'il échoue *)
let assert_true name condition =
  if condition then
    Printf.printf "[OK] Test '%s' passé.\n" name
  else
    Printf.printf "[FAIL] Test '%s' raté !\n" name

(* Vérifie que le labyrinthe généré a la bonne taille physique *)
let test_generation_dimensions () =
  let w, h = 10, 5 in
  let g = Grid.generate w h in
  let (rows, cols) = Grid.get_dimensions g in
  let expected_rows = 2 * h + 1 in
  let expected_cols = 2 * w + 1 in
  assert_true "Dimensions" (rows = expected_rows && cols = expected_cols)


(* Vérifie que la génération place bien un seul Départ (S) et une seule Arrivée (E) *)
let test_presence_depart_arrivee () =
  let g = Grid.generate 5 5 in
  let check = Grid.check g in
  assert_true "Presence S et E (Grid.check)" check

(* Vérifie que tous les labyrinthes générés aléatoirement ont bien une solution *)
let test_always_solvable () =
  Random.self_init ();
  let g = Grid.generate 10 10 in
  assert_true "Solvabilité garantie (Generate -> Solve)" (Grid.solve g)

(* Vérifie que la fonction 'solve' laisse bien des traces (points) sur le chemin *)
let test_marquage () =
  let g = Grid.generate 5 5 in
  let _ = Grid.solve g in 
  let found_dot = ref false in
  let (h, w) = Grid.get_dimensions g in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      if Grid.get g i j = '.' then found_dot := true
    done;
  done;
  assert_true "Marquage du chemin (.)" !found_dot
let run_all () =
  print_endline "Demarrage des tests ----------";
  test_generation_dimensions ();
  test_presence_depart_arrivee ();
  test_always_solvable ();
  test_marquage ();
  print_endline "Fin des tests ----------"