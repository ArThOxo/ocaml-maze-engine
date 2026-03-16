#!/bin/bash

clear

echo "========================================="
echo "      🚀 OCAML MAZE ENGINE DEMO 🚀     "
echo "========================================="
sleep 1

echo -e "\n🔨 [1/2] Building the project with Dune..."
dune build
sleep 1

echo -e "\n🎬 [2/2] Generating and solving a 20x10 maze in real-time...\n"
sleep 1

# Génère le labyrinthe dans le fichier
./maze.exe random 20 10 $RANDOM > demo_maze.laby

# Lance l'animation en direct (le labyrinthe va s'animer tout seul !)
./maze.exe animate demo_maze.laby

# Nettoie le fichier temporaire
rm demo_maze.laby

echo -e "\n✅ Demo complete! The source code is ready."