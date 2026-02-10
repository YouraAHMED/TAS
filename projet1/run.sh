#!/bin/bash

echo "===================================="
echo "   Compilation du projet TAS...     "
echo "===================================="

# Fichiers sources à compiler (dans l'ordre correct)
FILES="ast.ml pretty.ml utils.ml semantics.ml types.ml typing.ml main.ml"

# Compilation
ocamlc -o main $FILES

if [ $? -ne 0 ]; then
    echo "❌ Erreur : la compilation a échoué."
    exit 1
fi

echo "✅ Compilation réussie."

echo "===================================="
echo "      Exécution du programme        "
echo "===================================="

./main

echo ""
echo "===================================="
echo " Nettoyage : suppression fichiers   "
echo "   .cmi / .cmo et binaire main      "
echo "===================================="

rm -f *.cmi
rm -f *.cmo
rm -f main

echo "🧹 Nettoyage terminé."

echo "===================================="
echo "        ✓ Script terminé ✓          "
echo "===================================="
