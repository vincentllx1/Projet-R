### PROJET DE SESSION R ####
### VINCENT LALOUX ###

library("sqldf")
library("tidyr")
library("dplyr")
library("stringr")
library("ggplot2")
library("ggmap")
library("rgdal")
library("sf")
library("tmap")
library("plotly")

setwd("/Users/vincentllx/Desktop/02-Logiciels Statistiques/Projet/R")

mvmt=read.csv("estimationprojectionuniquedonneesouvertes.csv")
shaperss=st_read("/Users/vincentllx/Desktop/02-Logiciels Statistiques/Projet/R/Territoires_RSS_2020/Territoires_RSS_2020.shp")
shapeclsc=st_read("/Users/vincentllx/Desktop/02-Logiciels Statistiques/Projet/R/Territoires_CLSC_2020/Territoires_CLSC_2020.shp")


# Pré-traitement de notre BD : 

#On ne garde que les observation liées aux CLSC
clsc_base=subset(mvmt,mvmt$NiveauGeographique=="CLSC")
#On récupère l'identifiant des régions depuis la variable territoire
clsc_base$RSS_code=substr(clsc_base$Territoire,1,2)
clsc_base$RTS_code=substr(clsc_base$Territoire,1,3)
clsc_base$RLS_code=substr(clsc_base$Territoire,1,4)
clsc_base$CLSC_code=substr(clsc_base$Territoire,1,5)

#################
####BLOC 1#######
#################


###FONCTION AGGREGATE####

#Nous souhaitons connaitre la population moyenne par région CLSC au Québec

clsc_base_T=subset(clsc_base,clsc_base$Sexe=="Total") #Nous devons au préalable filtrer notre table pour n'avoir que les observations du total de la population (Masculin+Féminin)
pop_moy_clsc=aggregate(clsc_base_T$TousLesAges~clsc_base_T$Territoire, #aggreger la variable TousLesAges en fonction de Territoire
               FUN=mean, #utiliser la moyenne pour aggréger
               na.rm = TRUE) #ignorer les valeurs nulles (facultatif)

colnames(pop_moy_clsc)=c("Territoire","Population") #on renomme nos colonnes

#On souhaite reproduire ce résultat, mais en arrondissant nos valeurs d'aggrégation

pop_moy_clsc=aggregate(clsc_base_T$TousLesAges,list(clsc_base_T$Territoire),function(x) round(mean(x,0))) #utiliser list() nous permets d'executer une fonction qui prend cette fois la moyenne et son arrondie
colnames(pop_moy_clsc)=c("Territoire","Population") #on renomme nos colonnes

#On peut également créer notre propre fonction pour l'utiliser dans notre fonctino aggregate
second = function(x) {
  if (length(x) == 1)
    return(x)
  return(sort(x, decreasing = TRUE)[2])}

agg = aggregate(clsc_base$TousLesAges,
                by = list(clsc_base$Territoire, clsc_base$Sexe),
                FUN = second)


#On souhaite reproduire ce résulat, mais avec des noms de colonnes adéquats

pop_moy_clsc=aggregate(clsc_base$TousLesAges,list(Territoire=clsc_base$Territoire, Sexe=clsc_base$Sexe=="Total"),function(x) round(mean(x,0))) #utiliser list() nous permets de selectionner les observations de notre colonne SEXE
colnames(pop_moy_clsc)=c("Territoire","Population") #on renomme nos colonnes

#On souhaite reproduire ce résulat, mais avec des noms de colonnes adéquats

pop_moy_clsc=aggregate(cbind(TousLesAges,X30)~Territoire+Sexe, data=clsc_base, FUN=mean, na.rm = TRUE) # On guarde le titre des colonnes !

#autres fonctions d'aggrégations disponibles


#fonction Quantile
pop_quant_rss=aggregate(clsc_base_T$TousLesAges, by=list(RSS=substr(clsc_base_T$Territoire,1,2)), FUN=quantile, probs=0.95)

#fonction Length
nb_clsc=aggregate(clsc_base_T$CLSC_code, by=list(RSS=substr(clsc_base_T$Territoire,1,2), Annee=clsc_base_T$Annee), FUN=length)#nombre de CLSC par RSS par année

####LIBRAIRIE SQLDF####

#on reproduit les exemples précédent : population moyennes par CLSC
pop_moy_clsc2=sqldf("select Territoire, avg(TousLesAges) from clsc_base_T group by Territoire")
pop_moy_clsc2=sqldf("select Territoire, avg(TousLesAges) as moyennepop from clsc_base_T group by Territoire") #on obtient une table avec les bons noms de colonnes bien plus facilement !

#on souhaite utiliser cette fonction pour analyser les hierarchie de régions par rapport au région admin RSS
hierarchie=sqldf("select distinct RSS_code, count(distinct RTS_code) as RTS_code, count(distinct RLS_code) as RLS_code, count(distinct CLSC_code) as CLSC_code from clsc_base group by RSS_code, Annee")


####LIBRAIRIE DPLYR####

#Population du québec par année
Quebec= clsc_base %>%
  select(CLSC_code,RLS_code,RTS_code,RSS_code, Annee, Sexe, TousLesAges, c(9:99)) %>%
  filter(Sexe=="Total") %>%
  group_by(Annee) %>%
  summarise(Population=sum(TousLesAges))

#Population du québec la plus importante et la plus faible par CLSC entre 1996 et 2041
Quebec= clsc_base %>%
  select(CLSC_code,RLS_code,RTS_code,RSS_code, Annee, Sexe, TousLesAges, c(9:99)) %>%
  filter(Sexe=="Total") %>%
  group_by(Annee) %>%
  summarise(test=range(TousLesAges))

#Différentes fonctions appliquées sur la population total du québec par clsc entre 1996 et 2041
Quebec= clsc_base %>%
  select(CLSC_code,RLS_code,RTS_code,RSS_code, Annee, Sexe, TousLesAges, c(9:99)) %>%
  filter(Sexe=="Total") %>%
  group_by(Annee) %>%
  summarise(tibble(min=min(TousLesAges),max=max(TousLesAges),moyenne=mean(TousLesAges),total=sum(TousLesAges)))

#déterminer les quantiles grâce à une fonction créer au préalable

quibble2 <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble("{{ x }}" := quantile(x, q), "{{ x }}_q" := q)
}

Quebec= clsc_base %>%
  select(CLSC_code,RLS_code,RTS_code,RSS_code, Annee, Sexe, TousLesAges, c(9:99)) %>%
  filter(Sexe=="Total") %>%
  group_by(Annee) %>%
  summarise(quibble2(TousLesAges, c(0.25, 0.5, 0.75)))

#Comment aggréger toutes nos variables ?

#option1
Quebec= clsc_base %>%
  select(CLSC_code,RLS_code,RTS_code,RSS_code, Annee, Sexe, TousLesAges, c(9:99)) %>%
  filter(Sexe=="Total") %>%
  group_by(Annee) %>%
  summarise(across(everything(), list(mean)))

#option2
Quebec= clsc_base %>%
  select(CLSC_code,RLS_code,RTS_code,RSS_code, Annee, Sexe, TousLesAges, c(9:99)) %>%
  filter(Sexe=="Total") %>%
  group_by(Annee) %>%
  summarise_all(.funs = c(mean="mean"))

#################
####BLOC 2#######
#################

#préparation des données

rssshp <- shaperss %>% 
  select(RSS_code,Etiquette,Shape_Leng,Shape_Area,geometry)
nrow(rssshp)

clsc_base_shp=clsc_base_T%>%
  select(RSS_code,Annee,TousLesAges) %>% 
  filter(Annee==c(2010)) %>% 
  group_by(RSS_code) %>% 
  summarise(Population=sum(TousLesAges))

rssshp1 = inner_join(rssshp,clsc_base_shp)

### LIBRAIRIE GGLOT2 ###

#reprsentation de base
carte_base=ggplot(rssshp1)+ #on indique en entrée le dataframe qui modélise les formes géométrique dans l'espace, le dataframe sera utilisé automatiquement dans les étapes suivantes
  geom_sf()

#on ajoute la population
carte_pop= ggplot(data=rssshp1) +
  geom_sf(aes(fill=Population)) #on indique ici de distinguer les formes géométriques selon la variable population 

#on décide de changer les couleurs
carte_pop=ggplot(data=rssshp1) +
  geom_sf(aes(fill=Population)) +
  scale_fill_gradient (low= "#56B4E9", high= "#E69F00") #cette fonction ggplot nous permet de personnalisé les couleur que l'on souhaite utiliser selon les données de Population (high à low) 

#on aimerait avoir plus de précision sur la région de montréal

ggplot(data=rssshp1) +
  geom_sf(aes(fill=Population)) +
  scale_fill_gradient (low= "#56B4E9", high= "#E69F00") +
  coord_sf(xlim = c(-76, -70), ylim = c(45, 50)) #on indique la fenêtre grâce au longitude et latitude

#notre carte a disparue ! On doit déterminer le CRS de référence :

rssshp2 <- st_transform(rssshp1, crs = 4269) #ce CRS est un référenciel classique, qui correspond au coordonnée que l'on souhaite utiliser pour zoomer sur montréal
st_crs(rssshp1)
st_crs(rssshp2) #cette fonction nous permets de nous assurer que le CRS à bel et bien changé

#nous sommes maintenant en mesure de zoom sur la région de montréal

ggplot(data = rssshp2) +
  geom_sf(aes(fill=Population)) +
  scale_fill_gradient (low= "#56B4E9", high= "#E69F00") +
  coord_sf(xlim = c(-75, -70), ylim = c(45, 47)) #on indique la fenêtre grâce au longitude et latitude

#On peut également indiqué un lieu précis grace à annotate
ggplot(data = rssshp2) +
  geom_sf(aes(fill=Population)) +
  scale_fill_gradient (low= "#56B4E9", high= "#E69F00") +
  coord_sf(xlim = c(-75, -70), ylim = c(45, 47)) +
  annotate("point", x = -73.55249779, y = 45.505331312, colour = "red", size = 2)  #Nous sommes en mesure de placer un point ici

#on precise les etiquettes des régions
ggplot(data = rssshp2) +
  geom_sf(aes(fill=Population)) +
  scale_fill_gradient (low= "#56B4E9", high= "#E69F00") +
  coord_sf(xlim = c(-75, -70), ylim = c(45, 47)) +
  annotate("point", x = -73.55249779, y = 45.505331312, colour = "red", size = 2) +
  geom_sf_label(aes(label = Etiquette),label.size = 0.1, nudge_y = 0.2) #ggplot nous permet également de labeliser nos régions selon la variable de notre choix

### LIBRAIRIE TMAP ###

#on utilise le mode plot de la librairie
tmap_mode("plot")

#on représente notre première carten, sans nuance de couleur
tm_shape(rssshp1)+
  tm_polygons() #nous permet de spécifier la couche à utiliser, tm_polygons cherche automatiquement les données geometry

#on utilise le mode view de la librairie pour obtenir une carte interactive
tmap_mode("view")

#interactive, sans couleur
tm_shape(rssshp1)+
  tm_polygons(id="RSS_code") #nous permet d'identifier les différentes zones

#on utilise de nouveau la population pour distinguer les couleur des régions
tm_shape(rssshp1) +
  tm_polygons("Population", id="RSS_code", palette="Oranges") #nous permet d'identifier les différentes zones oar rapport à leur population !




