# TRAVAIL DE SESSION – R

## Logiciels statistique

### Dans le cadre du cours de logiciels statistiques de la session d’hiver 2021, j’ai travaillé sur un mini-projet de programmation sur le logiciel R. L’objectif est d’utiliser plusieurs moyens d’agrégation des données d’une table. Je me suis ensuite penché sur les visualisations cartographiques.

## Introduction

Avant de débuter ce projet, je pense important de mentionner que j’ai effectué un premier nettoyage succin pour obtenir toutes les observations liées au niveau géographique des CLSC. Pour rappel la base de données originale contient plusieurs régions administratives, qui comprennes plusieurs régions : En ordre décroissant de « granularité » nous avons les observations liées au Québec, RSS, RTS, RLS, CLSC. Pour chacune de ces régions, nous avons les observations de la population masculine, féminine, total, ainsi que par année. Nous avons 46 années dans notre base de données : de 1996 à 2041.

### 1)	La fonction aggregate()

La fonction aggregate, bien que puissante, est relativement difficile d’utilisation. On verra ici que sa syntaxe peut varier et ses sorties également.

Partons des apprentissages réalisés dans le cadre du cours. Afin d’avoir le portrait de la population moyenne entre 1996 et 2041 par CLSC nous écrivons :

```r
testclsc_base_H=subset(clsc_base,clsc_base$Sexe=="Total")
test=aggregate(clsc_base_H$TousLesAges~clsc_base_H$Territoire, 
               FUN=mean, 
               na.rm = TRUE) # On perd le titre des colonnes
```

