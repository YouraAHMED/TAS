# Projet d'analyseur statique du cours TAS - Rapport de projet

*À compléter.*


Q1: Quelle est la sémantique de l'instruction `rand(l,h)` dans un programme ? Quel est le résultat attendu de l'interprète ?

L’instruction rand(l,h) représente un choix non-déterministe d’une valeur entière comprise entre l et h inclus, contrairement à un générateur aléatoire classique, rand(l,h) ne choisit pas une seule valeur arbitrairement, mais introduit plusieurs exécutions possibles, une pour chaque valeur possible dans l’intervalle [l,h].


Q2: Sous quelles conditions l'exécution d'un programme s'arrête-t-elle ? Quel est alors le résultat de l'interprète ?

L’exécution d’un programme s’arrête lorsque :

le flot d’exécution atteint la fin du programme (ou la fin d’un bloc),

aucune instruction supplémentaire n’est à exécuter.

À la fin de l’exécution, l’interprète affiche :

soit un ensemble d’environnements finaux possibles,

soit un environnement vide si toutes les variables étaient locales au bloc.


Q3: Si le programme comporte une boucle infinie, est-il possible que l'interprète termine tout de même ? Dans quels cas ?

Oui, il est possible que l’interprète termine même si le programme comporte une boucle qui semble infinie.
L’analyse concrète manipule des ensembles d’états et explore toutes les exécutions possibles.

#######ASSERT########
L’instruction assert a été implémentée dans l’interprète concret.
Lorsqu’une assertion peut être violée, un message assertion failure est affiché.
L’analyse se poursuit ensuite en ne conservant que les états satisfaisant l’assertion,
afin d’éviter des erreurs redondantes lors des itérations de boucles.


# 3 Extension : entiers machine 32-bit signés (int32)
# 3.1 Objectif

Dans le projet de base, le type int correspond à des entiers mathématiques (dans ℤ), sans borne et sans dépassement.

Cette extension modifie la sémantique du langage pour interpréter int comme des entiers machine 32-bit signés (complément à deux), donc les valeurs appartiennent à :

[−2^31 ; 2^31 − 1]

Les opérations arithmétiques (+, -, *, / et -e) sont effectuées avec wrap-around modulo 
2^32, puis réinterprétées en signé.

Le but est :

de montrer des comportements différents entre ℤ et int32,

d’adapter les domaines abstraits pour analyser correctement cette nouvelle sémantique.

# 3.2 Activation du mode int32

Un nouveau mode a été ajouté à l’analyseur via l’option :

-int32

si -int32 est activée : toutes les opérations se font en int32 signés ;

sinon : on garde la sémantique initiale (ℤ).

Ce mode est contrôlé par une référence globale :

Int32_semantics.enabled : bool ref

# 3.3 Implantation technique
# 3.3.1 Module Int32_semantics

Un fichier src/utils/int32_semantics.ml est ajouté pour regrouper :

les bornes machine int32,

une fonction de normalisation (wrap-around),

des opérations arithmétiques int32.

Les bornes sont :

min_i32 = -2^31

max_i32 = 2^31 - 1

La fonction principale est :

normalize : Z.t -> Z.t

Elle transforme n’importe quel entier en une valeur équivalente int32 signée.

# 3.4 Modifications des domaines
# 3.4.1 Domaine concret

Dans concrete_domain.ml, l’exécution concrète est modifiée pour respecter la sémantique int32 :

toutes les opérations utilisent les fonctions de Int32_semantics,

et une normalisation est appliquée après chaque calcul.

Cela permet d’obtenir une exécution conforme au comportement “machine”.

# 3.4.2 Domaine des intervalles

Dans interval_domain.ml, l’intervalle doit rester :

sonore (ne jamais oublier des valeurs possibles),

compatible avec les tests initiaux quand -int32 n’est pas activée.

Quand -int32 est activée, le wrap-around provoque souvent des ensembles non convexes (ex : saut de max_i32 à min_i32), impossible à représenter précisément avec un seul intervalle.

Donc, en cas de doute (overflow possible), on utilise une approximation sûre :

[min_i32;max_i32]
[min_i32;max_i32]

Exemple typique :

2147483647 + 1 = -2147483648

# 3.4.3 Domaine des constantes

Dans constant_domain.ml, les opérations constantes sont adaptées :

const, neg, add, sub, mul, div
utilisent la sémantique int32 (donc normalisation après calcul).

Ainsi, si un calcul constant overflow, la valeur devient celle attendue en int32.

# 3.4.4 Domaine de parité

Dans parity_domain.ml, la parité est conservée de manière habituelle.

On applique simplement normalize à :

const et aux bornes de rand

La parité est stable modulo 2^32, donc ce domaine reste correct.

# 3.4.5 Produit réduit (Parité ∧ Intervalles)

Le produit réduit a été conservé (répertoire tests/20_reduced) :

si l’intervalle est singleton, il impose une parité précise,

si la parité est Even/Odd, l’intervalle est resserré en ajustant les bornes.

Cela permet de conserver la précision attendue sur les tests du produit réduit.

# 3.5 Tests ajoutés

Les tests de l’extension se trouvent dans :

tests/30_extension/


Ils s’exécutent avec :

-interval -int32


Ces tests illustrent des comportements impossibles en ℤ.

# 3.6 Description des tests (30_extension)
3001_add_overflow.c — overflow addition

Vérifie que : 2147483647 + 1 wrap vers -2147483648.

3002_sub_underflow.c — underflow soustraction

Vérifie que : -2147483648 - 1 wrap vers 2147483647.

3003_mul_overflow_by2.c — overflow multiplication

Exemple : 1073741823 * 2 = -2 en int32.

3004_mul_min_by_minus1.c — cas spécial INT_MIN * (-1)

En int32 : -2147483648 * (-1) overflow et redonne -2147483648.

3005_square_overflow.c — overflow par carré

Un carré dépasse la plage int32 et wrap.

3006_div_min_by_minus1.c — division “dangereuse”

En int32 : min_i32 / -1 overflow (car 2147483648 n’existe pas en signé).

3007_if_after_overflow.c — branche impossible après wrap

Montre qu’après overflow, une condition peut rendre une branche inatteignable (⊥).

3008_two_steps_wrap.c — wrap en deux étapes

Deux opérations successives provoquent un wrap attendu.

3009_loop_bounded_wrap.c — wrap dans une boucle

Montre que l’analyse reste sonore avec boucle et widening.

3010_assert_wrap_property.c — assertion après wrap

Montre qu’une propriété vraie en ℤ peut devenir fausse après wrap-around.

# 3.7 Conclusion

Cette extension montre l’impact du passage :

des entiers mathématiques ℤ

aux entiers machine int32

Les overflows rendent les ensembles de valeurs souvent non convexes, donc le domaine des intervalles perd en précision dans certains cas (ce qui est normal et attendu).

Les tests ajoutés illustrent :

overflow / underflow,

cas particulier de INT_MIN,

effets en branches et boucles,

différences visibles sur assertions.
