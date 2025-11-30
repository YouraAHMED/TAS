README — Projet TAS (Langage Lambda Étendu)

Présentation du projet
----------------------
Ce projet implémente un langage fonctionnel complet basé sur le λ-calcul,
avec entiers, listes, opérateurs arithmétiques, conditionnels, let,
point fixe, références mémoire et un système de types avec unification.

Le langage inclut :
- entiers natifs
- listes
- if zero / if empty
- let-binding
- fixpoint
- références (ref, !, :=)
- typage via génération d’équations + unification
- sémantique LTR-CBV (Left-to-Right Call-by-Value)

Architecture des fichiers
-------------------------
ast.ml           : Syntaxe des termes
pretty.ml        : Pretty-printer
utils.ml         : Mémoire + utilitaires
semantics.ml     : Sémantique LTR-CBV
types.ml         : Syntaxe des types
typing.ml        : Génération d’équations + unification
main.ml          : Tests automatiques
resultats.txt    : Résultats enregistrés
run.sh           : Script de compilation & exécution

Exécution du projet
-------------------
1) Rendre le script exécutable (si besoin) :
   chmod +x run.sh

2) Lancer le projet :
   ./run.sh

Le script effectue :
- Compilation
- Exécution
- Enregistrement des résultats
- Nettoyage des fichiers .cmi/.cmo et du binaire

Résultats produits
------------------
Le terminal affiche :
- Pretty-printer
- Évaluation simple
- Tests sur listes
- Fact(5) = 120
- Références mémoire
- Typage

Le fichier resultats.txt contient les mêmes informations.

Comprendre les tests
--------------------
Pretty-printer : affiche (λx. x 5)
Évaluation simple : (λx. x + 3) 7 = 10
Listes : Head = 1, Tail = [2]
Point fixe : fact(5) = 120
Références : let x = ref 10 in x := 42
Typage : λx. x + 3 : N -> N

Conclusion
----------
Le projet met en œuvre l’AST, la sémantique CBV, la mémoire impérative,
le polymorphisme, l’unification, ainsi que des tests automatisés.