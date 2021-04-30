# TRAVAIL DE SESSION – R

## Logiciels statistique

### Dans le cadre du cours de logiciels statistiques de la session d’hiver 2021, j’ai travaillé sur un mini-projet de programmation sur le logiciel R. L’objectif est d’utiliser plusieurs moyens d’agrégation des données d’une table. Je me suis ensuite penché sur les visualisations cartographiques.

# BLOC #1: Les aggrégations études des différentes méthode et de leur potentiel
## Introduction

Avant de débuter ce projet, je pense important de mentionner que j’ai effectué un premier nettoyage succin pour obtenir toutes les observations liées au niveau géographique des CLSC. Pour rappel la base de données originale contient plusieurs régions administratives, elles même, comprennes plusieurs sous-régions : En ordre décroissant de « granularité » nous avons les observations liées au Québec, RSS, RTS, RLS, CLSC. Par ailleurs, pour chacune de ces régions, nous avons les observations de la population masculine, féminine, total, ainsi que par année. Nous avons 46 années dans notre base de données : de 1996 à 2041.

## 1)	La fonction aggregate()

La fonction aggregate, bien que puissante, est relativement difficile d’utilisation. On verra ici que sa syntaxe peut varier et ses sorties également.

Partons des apprentissages réalisés dans le cadre du cours. Afin d’avoir le portrait de la population moyenne entre 1996 et 2041 par CLSC nous procédons comme suit :
Dans un premier temps on doit filtrer notre table de base créer précédemment afin de n’avoir que les observations liées à la population totale.


```r
clsc_base_T=subset(clsc_base,clsc_base$Sexe=="Total") #Nous devons au préalable filtrer notre table pour n'avoir que les observations du total de la population (Masculin+Féminin)
```

Dans un deuxième temps nous sommes en mesure d’agréger ces observations :

```r
pop_moy_clsc=aggregate(clsc_base_T$TousLesAges~clsc_base_T$Territoire, #aggreger la variable TousLesAges en fonction de Territoire
               FUN=mean, #utiliser la moyenne pour aggréger
               na.rm = TRUE) #ignorer les valeurs nulles (facultatif)

```
Notons la syntaxe particulière de la fonction agregate qui utilse ~ pour dire « en fonction de ». Ici l’argument na.rm est facultatif puisque nous n’avons pas d’observations nulles.

On constate qu’en moyenne, la population de la région de kamouraska est de 21029,54 personnes entre 1996 et 2041.

Remarques :
- Nos colonnes sont mal étiquetées, ce qui nous oblige à les étiqueter avec une nouvelle ligne de code :
``` R
colnames(test)=c("Territoire","Population")
``` 
- Nous avons dû entrer une ligne de code au préalable pour utiliser les observations qui utilisent le Total (Féminin Masculin)
- Nous ne sommes pas en mesure d’arrondir le résultat de notre agrégation. Pour pallier ce problème, nous devons utiliser la fonction liste pour définir notre variables référence (ici Territoire) pour l’agrégation des observations de la variable numérique (ici TousLesAge). Nous sommes en mesure de créer une fonction imbriquée qui pourra calculer l’arrondi de notre moyenne. Ainsi :

``` R
age_moyen_clsc=aggregate(clsc_base_T$TousLesAges,list(clsc_base_T$Territoire),function(x) round(mean(x,0))) #utiliser list() nous permets d'executer une fonction qui prend cette fois la moyenne et son arrondie
```
On peut aussi utiliser des fonctions créer précédemment, et disponible dans notre environnement global pour les utiliser ensuite dans notre fonction aggregate. Dans le code ci-dessous nous souhaitons obtenir la deuxième plus grande valeur parmi les observations agrégées.

``` R
second = function(x) {
  if (length(x) == 1)
    return(x)
  return(sort(x, decreasing = TRUE)[2])}

agg = aggregate(clsc_base$TousLesAges,
                by = list(clsc_base$Territoire, clsc_base$Sexe),
                FUN = second)
```

Une bonne chose de faite ! Cependant, le nom de colonnes a cette fois changé : on remarque qu’en utilisant les listes, la fonction aggregate() donne automatiquement le nom Group.1 pour la variable de référence (de regroupement) et x pour la variable à agréger. Pour faire appel à notre fonction round, nous devons par conséquent préciser l’appliquer sur cette nouvelle variable nommée x.

Nous allons essayer maintenant de complexifier notre code pour obtenir des noms de colonnes de référence cohérents. Aussi nous allons essayer d’inclure un énoncé conditionnel au sein de notre liste. Autrement dit nous allons filtré notre base de données directement dans la fonction aggregate(). Nous verrons également qu’il est facile d’ajouter des niveaux de granularité en ajoutant des arguments à la fonction de liste. Le code ci-dessous démontre les avantages de la fonction list nous offre ici :

``` R
test<-aggregate(clsc_base$TousLesAges,list(Territoire=clsc_base$Territoire, Sexe=clsc_base$Sexe=="Total"),function(x) round(mean(x,0))) #utiliser list() nous permets de selectionner les observations de notre colonne SEXE
```
Également notre nouvelle colonne Sexe retourne un booléen pour nous indiquer lorsque la condition de Sexe=="Total" est remplie. Pas idéal, mais sur beaucoup de données, il sera facile de faire une requête pour obtenir notre table finale.

Par ailleurs, on remarque que la dernière colonne n’a pas été nommé. Pour résoudre ce problème nous pouvons utiliser l’argument data dans la fonction aggregate() qui nous permettra de ne pas avoir à spécifier le jeu de données suivi d’un $, mais directement le nom de la variable que nous voulons utiliser. Notons que ce petit argument rend le code beaucoup plus lisible. En utilisant cbind() on est en mesure de spécifier plusieurs colonnes sur lesquelles appliquer l’agrégation (déjà vu en cours).
``` R
aggregate(cbind(TousLesAges,X30)~Territoire+Sexe, data=clsc_base, FUN=mean, na.rm = TRUE) # On guarde le titre des colonnes
```

En revanche, cette syntaxe nous fait perdre l’utilité de la fonction liste() décrite précédemment. Nos moyennes ne sont plus arrondies ! 

Au-delà de cet exemple, nous pouvons également utiliser différentes fonctions dans l’argument FUN. Ici nous allons produire une table qui nous donnera le résultat de quantile 95%. On note ici que l’on a utilisé la fonction substr pour n’obtenir les résultats que sur les différents région RSS (composé de 2 chiffres d’identification). La fonction quantile nous permet d’observer les estimés de population sur un certaine quantile (en admettant une certaine distribution)

```R
pop_quant_rss=aggregate(clsc_base_T$TousLesAges, by=list(RSS=substr(clsc_base_T$Territoire,1,2)), FUN=quantile, probs=0.95) 
```

Aussi, la fonction Lenght nous renseigne sur le nombre de ligne contenu par observation agrégée. 

```R
nb_clsc=aggregate(clsc_base_T$CLSC_code, by=list(RSS=substr(clsc_base_T$Territoire,1,2), Annee=clsc_base_T$Annee), FUN=length)#nombre de CLSC par RSS par année 
```

Ici nous pouvons par conséquent déduire que la région « 03 » compte 13 CLSC en 1996. On note que la variable à agréger importe peu, on aurait pu le faire selon la variable TousLesAges et nous serions arrivé au même résultat.

On remarque que cette syntaxe est relativement complexe à maitriser, et arriver au bon résultat demande beaucoup d’essaies erreur au début. 

## 2)	Librairie sqldf

Cette librairie nous permet d’utiliser la syntaxe de requêtes SQL pour produire des tableaux. Grâce à cette librairie, nous nous appliquerons à développer la hiérarchie des régions administratives du ministère de la santé. La particularité de cette fonction, c’est qu’elle nous permet d’écrire en caractère la requête que nous aurions écrite en langage SQL. 

Tout d’abord essayons de reproduire notre premier exemple de la section précédente en déterminant la moyenne de population par CLSC entre 1996 et 2041 :

```R
pop_moy_clsc2=sqldf("select Territoire, avg(TousLesAges) from clsc_base_T group by Territoire")
```

Il est amusant de constater que nous obtenons les mêmes résultats, à l’exception des noms de colonnes qui sont les mêmes que ceux que nous avons lors d’une requête SQL. Heureusement la syntaxe SQL nous permet de préciser le nom des colonnes directement dans la requête tel que : 

```R
pop_moy_clsc2=sqldf("select Territoire, avg(TousLesAges) as moyennepop from clsc_base_T group by Territoire")
```
Finalement, nous alons tenter de décortiquer un peu mieux notre base de données grâce à cette librairie. Notre objectif est d’avoir un aperçu de la hiérarchie du nombre de régions RTS, RLS et CLSC par région administrative (RSS).
```R
hierarchie=sqldf("select distinct RSS_code, count(distinct RTS_code) as RTS_code, count(distinct RLS_code) as RLS_code, count(distinct CLSC_code) as CLSC_code from clsc_base group by RSS_code, Annee")
```
Nous constatons que notre requête SQL fonctionne bien ! Nous obtenons le nombre de régions en fonction des région RSS : on peut voir que la région de Montréal (RSS=06) est celle qui contient le plus de sous régions.

## 3) La librairie dplyr

La librairie dplyr est incontournable lorsque l’on souhaite faire du traitement de données et notamment lorsque l’on souhaite les agréger. Nous examinerons par conséquent les diverses options d’agrégation de la fonction summarise de la librairie dplyr.

Ce premier pipe de données nous permets de directement choisir les données que l’on souhaite dans utiliser (select()) et nous pouvons également les filtre (filer()). Comme nous l’avons vu en cours, pour agréger des données nous avons besoin de deux étapes dans le pipe ; group_by et summarise. Group_by nous renseigne sur le groupe de référence sur lequel les données seront agrégées. Summarise contient les variables à agréger ainsi que la fonction d’agrégation que nous utiliserons. Notons ce premier code qui nous permets d’extraire la population totale du Québec par année (entre 1996 et 2041) :

```R
Quebec= clsc_base %>%
  select(CLSC_code,RLS_code,RTS_code,RSS_code, Annee, Sexe, TousLesAges, c(9:99)) %>%
  filter(Sexe=="Total") %>%
  group_by(Annee) %>%
  summarise(Population=sum(TousLesAges))
```
 

Entre 1996 et 2008, la population du Québec semble avoir augmenté de 500 000 personnes !

Avec dyplr nous sommes en mesure d’utiliser un nombre important de fonction : ici, nous obtenons pour chaque année la plus petite valeur observées (correspond à un clsc) ainsi que la plus grande :
```R
Quebec= clsc_base %>%
  select(CLSC_code,RLS_code,RTS_code,RSS_code, Annee, Sexe, TousLesAges, c(9:99)) %>%
  filter(Sexe=="Total") %>%
  group_by(Annee) %>%
  summarise(test=range(TousLesAges))
```
 

Un des avantages également et que nous n’avons pas pu expérimenter lors des précédents exemples, c’est de créer plusieurs fonctions d’agrégations pour différentes variables de notre dataframe. Ici la fonction tibble nous permet d’énoncer plusieurs agrégations différentes pour la variable TousLesAges.
```R
Quebec= clsc_base %>%
  select(CLSC_code,RLS_code,RTS_code,RSS_code, Annee, Sexe, TousLesAges, c(9:99)) %>%
  filter(Sexe=="Total") %>%
  group_by(Annee) %>%
  summarise(tibble(min=min(TousLesAges),max=max(TousLesAges),moyenne=mean(TousLesAges),total=sum(TousLesAges)))
```
 

Tout comme nous avons pu le faire dans la fonction aggregate nous pouvons également inclure des fonctions prédéfinies, ici nous créons une fonction qui retournera les quantiles 0.25 0.5 et 0.75. Par conséquent nous obtenons 3 observations par années !
```R
quibble2 <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble("{{ x }}" := quantile(x, q), "{{ x }}_q" := q)
}

Quebec= clsc_base %>%
  select(CLSC_code,RLS_code,RTS_code,RSS_code, Annee, Sexe, TousLesAges, c(9:99)) %>%
  filter(Sexe=="Total") %>%
  group_by(Annee) %>%
  summarise(quibble2(TousLesAges, c(0.25, 0.5, 0.75)))
```
 

Nous avons vu que dplyr nous donne plus de flexibilité, et est plus simple d’utilisation que la fonction de base de R. Cependant il y a également autre chose que nous n’avons pas pu réussir, c’est d’agréger les 101 âges. A la main cela prendrai trop de temps. Heureusement dplyr nous permet de le faire selon deux manières :

La première manière pour y parvenir est d’utiliser les fonction accross(everything, list(mean))) :
```R
Quebec= clsc_base %>%
  select(CLSC_code,RLS_code,RTS_code,RSS_code, Annee, Sexe, TousLesAges, c(9:99)) %>%
  filter(Sexe=="Total") %>%
  group_by(Annee) %>%
  summarise(across(everything(), list(mean)))
```
 
Ici la fonction accro nous permet de définir les variables ainsi que la fonction qui doit leur être appliqués. Notons que les variables catégorielles sont désormais absentes (NA) puisqu’il est impossible d’appliquer une agrégation sur des variables caractères.

La deuxième manière pour y parvenir est d’utiliser une fonction summarise différente, il en existe un certain nombre, ici nous utiliserons summarise_all.
```R
Quebec= clsc_base %>%
  select(CLSC_code,RLS_code,RTS_code,RSS_code, Annee, Sexe, TousLesAges, c(9:99)) %>%
  filter(Sexe=="Total") %>%
  group_by(Annee) %>%
  summarise_all(.funs = c(mean="mean"))
```

On remarque cependant que l’on reproduit notre problème sur les variables catégorielles.

# BLOC #2 Visualisations de cartes dans R

## Introduction
Pour représenter graphiquement les différentes zones administratives du québec, j’ai dû au préalable charger plusieurs fichiers de données de type « shape files ». Ces fichiers de données doivent être lu au préalable grâce à la fonction st_read() de la librairie sf.
J’ai pu importer deux jeux de données contenant des données de type geometry qui contiennent les formes des régions des RSS. Il est important de noter que lorsque l’on joint les données, nous devons spécifier en premier le fichier contenant les formes géométriques.

Pour charger cet ensemble de données dans R, nous appelons la fonction st_read de sf (toutes les fonctions du paquet sf commencent par le préfixe st_, qui signifie spatiotemporel) et fournissons le chemin d'accès au fichier .shp.

Un premier fichier contiendra la population du Québec sur les 18 régions étudiées précédemment à l’année 2010.

## 1) La librairie ggplo2

La puissance de ggplot2 s’illustre par sa diversité d’application. J’ai en effet pu l’utiliser pour représenter mes données avec lesquelles nous avons travaillés au bloc précédent. Cependant, en matière de carte, ggplot2 fut particulièrement long à exécuter. La taille et la complexité des formes géographiques en sont probablement la raison. 

La première étape fut d’obtenir une carte représentant les régions RSS du québec grâce à la dimension geometry de notre dataframe. Lorsque l'on trace un ensemble de données vectorielles à partir d'un objet sf, on utilise la couche geom_sf pour afficher les caractéristiques spatiales sur une carte. Il n'est pas nécessaire de spécifier les mappings x et y dans aes, puisque ceux-ci sont définis par l'objet sf lui-même. Les lignes du graticule sont également dessinées automatiquement.

```R
ggplot(rssshp1) + 
  geom_sf()
```
On voit que cette carte n’est pour l’instant pas très informative. Ggplot a cependant bien lu la variable contenant les formes géométriques et les as bien placé dans le monde (on distingue les latitudes et les longitudes en abscisse et en ordonnée).

Comme observé pendant le cours, ggplot2 fonctionne un peu à la manière dplyr, en ajoutant des étapes subséquentes à chaque ligne grâce au + (et non avec >%>). Nous décidons alors d’utiliser notre variables population pour obtenir une carte de la répartition de la population sur le territoire du Québec. On remarque également, qu’en appelant la source de données dans la première fonction gglot, nous pouvons directement appeler les variables dans les étapes suivantes (ici geom_sf).
```R
ggplot(data=rssshp1) +
  geom_sf(aes(fill=Population))
```
 

On remarque que les couleurs sont relativement mal ajustés, on aimera pouvoir les inverser pour avoir les régions les plus peuplé en foncé et les moins peuplé en clair :
```R
ggplot(data=rssshp1) +
  geom_sf(aes(fill=Population)) +
  scale_fill_gradient (low= "#E69F00", high= "#56B4E9")
```
 

On observe que la région de Montréal semble la plus peuplé, mais on a du mal à la distinguer. On aimera zoomer sur cette région. Cependant ggplot2 ne nous donne pas l’occasion d’interagir avec les graphes, il nous faudra donc préciser une nouvelle ligne de code qui nous permettra de préciser les coordonnées de notre fenêtre :
```R
ggplot(data=rssshp1) +
  geom_sf(aes(fill=Population)) +
  scale_fill_gradient (low= "#56B4E9", high= "#E69F00") +
  coord_sf(xlim = c(-76, -70), ylim = c(45, 50))
```

On peut voir que notre fenêtre est vide. En cause : le Coordinate Reference System (CRS) de notre jeu de données n’est pas bien référencé. Nous créons un nouveau dataframe qui prendra un CRS différent de notre dataframe :
```R
rssshp2 <- st_transform(rssshp1, crs = 4269)
```
On peut vérifier que nous modifications on bien été prise en compte en comparant les crs de notre nouveau dataframe avec celui de rssshp1 avec la fonction suivante
st_crs(mrc_proj)

Nous pouvons réessayer de notre zoom précédent avec la fonction coord_sf 
Avant :
 

Après:
 
Nous sommes désormais en mesure de pouvoir utiliser la fonction coord_sf qui nous permet de préciser la fenêtre que l’on souhaite visualiser sur notre carte ! 
```R
ggplot(data = rssshp2) +
  geom_sf(aes(fill=Population)) +
  scale_fill_gradient (low= "#56B4E9", high= "#E69F00") +
  coord_sf(xlim = c(-75, -70), ylim = c(45, 47))
```
On souhaite désormais aider un ami qui nous mentionne qui n’est toujours pas capable de localiser Montréal. Pour l’aider on va afficher un point sur la carte et identifier les étiquettes de chaque région. La fonction annotate nous permet de dessiner une figure sur notre carte à un endroit précis : ici les coordonnées de Montréal.
```R
ggplot(data = rssshp2) +
  geom_sf(aes(fill=Population)) +
  scale_fill_gradient (low= "#56B4E9", high= "#E69F00") +
  coord_sf(xlim = c(-75, -70), ylim = c(45, 47)) +
  annotate("point", x = -73.55249779, y = 45.505331312, colour = "red", size = 2) +
  geom_sf_label(aes(label = Etiquette),label.size = 0.1, nudge_y = 0.2)
```
 
2)	La librairie Tmap

Cette librairie c’est révélé plus agréable d’utilisation : en effet, nous verrons qu’au lieu d’afficher un graph, la visualisation peut se faire au format html dans la fenêtre viewer. 

Mais tout d’abord, j’ai voulu reproduire nos étapes réalisées avec ggplot2. Dans un premier temps on peut observer la carte du Québec sans avoir précisé quelles variables seront utilisé pour la caractériser : 
```R
tm_shape(rssshp1)+
  tm_polygons()
```
On remarque ici que l’on doit préciser un identifiant pour les régions pour permettre à Tmap à représenter sa forme.

Nous observons une carte étant presque en tous points similaires à notre première carte, au détail près que nous n’observons pas les lignes de longitude et latitude en arrière-plan.
 

Avant de continuer, on va désormais utiliser le viewer de R. Pour cela il suffit de spécifier à tmap d’utiliser le viewer qui nous permet d’intéragir avec la carte en zoomant ou en consultant le contenu de chaque région en passant notre souris sur la carte.
```R
tmap_mode("view")

tm_shape(rssshp1)+
  tm_polygons(id="RSS_code")
```

Désormais nos cartes apparaîtrons dans le viewer. Nous essayons maintenant de reproduire notre carte avec la population totale. On note ici que l’appel des différentes variables se fait différemment de la librairie ggplot2 : elles doivent être entre guillemet !
```R
tm_shape(rssshp1) +
  tm_polygons("Population", id="RSS_code", palette="Oranges")
```
Nous observons un visuel, plus agréable qu’obtenue avec ggplot2. L’interface est beaucoup plus interactive et on peut interagir en consultant les informations de caques régions socio sanitaire (RSS). On peut également changer la couche de la carte.

## Conclusion
Il existe de très nombreuses librairies qui nous permettent de manipuler des données géographiques ou des formes. Notamment nous pourrions développer des cartes interactives ou qui fluctuent dans le temps avec d’autre librairies. Ce type de visualisation est un peu particulier dans la mesure où les données d’une carte peuvent être de plusieurs natures (geometry par exemple) et qu’elle repose sur un système de coordonnées un peu particulier, comme nous avons pu le voir dans ggplot2. 
