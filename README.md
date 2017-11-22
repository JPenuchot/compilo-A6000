# [Compilo A6000 - Jules Pénuchot](https://github.com/JPenuchot/compilo-a6000)

A6000 est un langage créé à l'occasion du développement d'un compilateur dans le cadre de l'UE de compilation de [Thibaut Balabonski](https://www.lri.fr/~blsk/). Sa syntaxe et ses fonctionnalités sont relativement basique, il ne permet pas encore de créer des fonctions mais les boucles, variables ainsi qu'une fonction `print` ont été implémentées.

##### Exemple

```
main(integer x) (
  var integer i;
  var integer j;
  var boolean continue;

  continue := true;
  i := 0;

  while (continue) (
    continue := false;
    j := 0;
    while j < x+1 (
      if i*i + j*j < x*x then (
        print(46);
  continue := true;
      ) else (
        print(35);
      );
      print(32);
      j := j+1;
    );
    print(10);
    i := i+1;
  );
)
```

## A propos du compilateur

Les appels de fonctions et de procédures sont implémentés, il ne reste que des raccords (concaténation) à la fin d'AllocatedToMips.ml. Le moteur de typage est cassé.