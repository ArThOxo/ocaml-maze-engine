type t = {
  cases : char array array;
  nb_lignes : int;
  nb_colonnes : int;
}

(* Convertit une chaîne de caractères en tableau de caractères pour pouvoir la modifier *)
let string_to_array s =
  let n = String.length s in
  let tab = Array.make n ' ' in
  for i = 0 to n - 1 do
    tab.(i) <- s.[i]
  done;
  tab


(* Ouvre un fichier .laby  lit toutes les lignes et construit la grille en mémoire *)
let from_file nom_fichier =
  let canal = open_in nom_fichier in
  let rec lire_lignes acc =
    try
      let ligne = input_line canal in
      let ligne_tab = string_to_array ligne in
      lire_lignes (ligne_tab :: acc)
    with End_of_file ->
      close_in canal;
      List.rev acc
  in
  let liste_lignes = lire_lignes [] in
  let matrice = Array.of_list liste_lignes in
  let h = Array.length matrice in
  let w = if h > 0 then Array.length matrice.(0) else 0 in
  { cases = matrice; nb_lignes = h; nb_colonnes = w }
let get_dimensions g = 
  (g.nb_lignes, g.nb_colonnes)




(* Affichage texte brut (pour sauvegarder dans un fichier propre) *)
let print g =
  for i = 0 to g.nb_lignes - 1 do
    let ligne = g.cases.(i) in
    for j = 0 to g.nb_colonnes - 1 do
      print_char ligne.(j)
    done;
    print_newline ()
  done

(* Affichage avec couleurs ANSI (pour le style dans le terminal) *)
let print_color g =
  for i = 0 to g.nb_lignes - 1 do
    let ligne = g.cases.(i) in
    for j = 0 to g.nb_colonnes - 1 do
      match ligne.(j) with
      | 'S' | 'E' -> Printf.printf "\027[1;31m%c\027[0m" ligne.(j)
      | '.' -> Printf.printf "\027[1;32m%c\027[0m" ligne.(j)
      | '+' | '-' | '|' -> Printf.printf "\027[36m%c\027[0m" ligne.(j)
      | c -> print_char c
    done;
    print_newline ()
  done

(* Cherche la case de départ 'S' dans toute la grille et renvoie ses coordonnées *)
let trouver_depart g =
  let result = ref None in
  for i = 0 to g.nb_lignes - 1 do
    for j = 0 to g.nb_colonnes - 1 do
      if g.cases.(i).(j) = 'S' then
        result := Some (i, j)
    done;
  done;
  !result


(* Cherche un chemin du départ 'S' vers la sortie 'E' et laisse des points '.' sur sa route *)
let solve g =
  let rec parcours i j =
    if i < 0 || i >= g.nb_lignes || j < 0 || j >= g.nb_colonnes then
      false
    else if g.cases.(i).(j) = 'E' then
      true
    else if g.cases.(i).(j) <> ' ' && g.cases.(i).(j) <> 'S' then
      false
    else
      begin
        if g.cases.(i).(j) <> 'S' then g.cases.(i).(j) <- '.';
        if parcours (i + 1) j || parcours (i - 1) j || 
           parcours i (j + 1) || parcours i (j - 1) then
          true
        else
          begin
            if g.cases.(i).(j) <> 'S' then g.cases.(i).(j) <- ' ';
            false
          end
      end
  in
  match trouver_depart g with
  | Some (x, y) -> parcours x y
  | None -> false


let melanger_liste liste =
  let etiquettes = List.map (fun x -> (Random.bits (), x)) liste in
  let trie = List.sort compare etiquettes in
  List.map snd trie


(* Crée un tout nouveau labyrinthe de la taille demandée en creusant des murs au hasard *)
let generate w h =
  let nb_lignes = 2 * h + 1 in
  let nb_colonnes = 2 * w + 1 in
  let cases = Array.make_matrix nb_lignes nb_colonnes ' ' in
  
  for i = 0 to nb_lignes - 1 do
    for j = 0 to nb_colonnes - 1 do
      if i mod 2 = 0 && j mod 2 = 0 then cases.(i).(j) <- '+'
      else if i mod 2 = 0 then cases.(i).(j) <- '-'
      else if j mod 2 = 0 then cases.(i).(j) <- '|'
    done;
  done;

  cases.(1).(1) <- 'S';
  cases.(nb_lignes - 2).(nb_colonnes - 2) <- 'E';

  let visite = Array.make_matrix h w false in

  let rec creuser i j =
    visite.(i).(j) <- true;
    
    let directions = [(0, -1); (0, 1); (-1, 0); (1, 0)] in
    let directions_hasard = melanger_liste directions in

    List.iter (fun (di, dj) ->
      let ni = i + di in
      let nj = j + dj in

      if ni >= 0 && ni < h && nj >= 0 && nj < w && not visite.(ni).(nj) then begin
        
        
        let y_curr = 2 * i + 1 in
        let x_curr = 2 * j + 1 in
        let y_next = 2 * ni + 1 in
        let x_next = 2 * nj + 1 in
        
        let y_mur = (y_curr + y_next) / 2 in
        let x_mur = (x_curr + x_next) / 2 in
        
        cases.(y_mur).(x_mur) <- ' ';

        creuser ni nj
      end
    ) directions_hasard
  in

  creuser 0 0;

  { cases; nb_lignes; nb_colonnes }


(* Vérifie que le labyrinthe est bien rectangulaire et possède exactement un S et un E *)
let check g =
  let s_count = ref 0 in
  let e_count = ref 0 in
  let dimensions_ok = ref true in

  for i = 0 to g.nb_lignes - 1 do
    if Array.length g.cases.(i) <> g.nb_colonnes then
      dimensions_ok := false;

    for j = 0 to g.nb_colonnes - 1 do
      if g.cases.(i).(j) = 'S' then s_count := !s_count + 1;
      if g.cases.(i).(j) = 'E' then e_count := !e_count + 1;
    done;
  done;
  !dimensions_ok && !s_count = 1 && !e_count = 1

(* Permet de lire le contenu d'une case précise de la grille *)
let get g i j =
    g.cases.(i).(j)




(* Cherche un chemin avec une animation visuelle en direct dans le terminal *)
let solve_animated g =
  print_color g; (* Affiche la grille une première fois *)
  let rec parcours i j =
    if i < 0 || i >= g.nb_lignes || j < 0 || j >= g.nb_colonnes then
      false
    else if g.cases.(i).(j) = 'E' then
      true
    else if g.cases.(i).(j) <> ' ' && g.cases.(i).(j) <> 'S' then
      false
    else begin
      if g.cases.(i).(j) <> 'S' then g.cases.(i).(j) <- '.';
      
      (* --- ANIMATION AVANCÉE --- *)
      Printf.printf "\027[%dA" g.nb_lignes; (* Remonte le curseur *)
      print_color g;
      flush stdout;
      Unix.sleepf 0.01; (* Pause de 20 millisecondes *)
      
      if parcours (i + 1) j || parcours (i - 1) j || 
         parcours i (j + 1) || parcours i (j - 1) then
        true
      else begin
        if g.cases.(i).(j) <> 'S' then g.cases.(i).(j) <- ' ';
        
        (* --- ANIMATION RETOUR (BACKTRACKING) --- *)
        Printf.printf "\027[%dA" g.nb_lignes;
        print_color g;
        flush stdout;
        Unix.sleepf 0.01;
        
        false
      end
    end
  in
  match trouver_depart g with
  | Some (x, y) -> parcours x y
  | None -> false