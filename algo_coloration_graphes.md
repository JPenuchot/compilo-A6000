# Algo de coloration des graphes

On se donne K couleurs

Un sommet S de degré < K ne pose jamais de problème
On peur le garder pour la fin
On se concentre sur G \ {S}

Algo :

```
Prendre un sommet S de degré minimal
Le retirer
Colorer G \ {S} (appel récursif)
Affecter à S une couleur compatible avec G \ {S}
```

Une fois la coloration obtenue :

- Les couleurs 0 à K - 1 sont associées à des registres
- Les autres vont dans la pile

NB : On peut garder deux registres de travail qui serviront à stocker les valeurs à aller chercher dans la pile

On aimerait quand même garder en registre des variables qu'on utilise beaucoup... Normal.

Algo modifié :

```
S'il existe un sommet de degré < K
  -> Simplifier
Sinon, on a des sommets qui seront potentiellement dans la pile. De préférence, prendre un sommet peu utilisé et de grand degré.
  -> "Spill"
```

--> Recommencer jusqu'à épuiser le graphe

Dernière phase : Sélection

```
On prend les sommets dans la pile
Un sommet "simplifié" sera coloriable
Un sommet "spill" a peut-être une (potential spill) couleur dispo (sinon actual spill)
```

On peut ajouter un 2ème type d'arêtes au graphe : "préférence" CàD essayer de donner la même couleur

Deux sommets liés par "préférence" et "interférence" : interférence gagne