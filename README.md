# À propos

## But de l'application 

Cette application permet d'analyser les données ouvertes d'AirBNB de la ville de Québec.

Le but visé de l'application est de faciliter le travail d'exploration des données qui précède souvent la mise en place d'un modèle d'apprentissage supervisé. L'accent a été mis sur la modélisation du prix de location de logements AirBNB selon leurs caractéristiques.

Le premier onglet de l'application permet de sélectionner un quartier au choix de la ville et de visualiser les positions et les prix des AirBNB du quartier choisi sur une carte. Il est également possible de sélectionner tous les quartiers à la fois.

Les deuxième et troisième onglets permettent ensuite de conduire une analyse univariée ou une analyse bivariée de notre choix sur les données du quartier choisi. À noter toutefois que seuls le prix de location quotidien ou son logarithme peuvent être choisis comme variable "y" de l'analyse bivariée.


## Données utilisées

Lien vers les données ouvertes : http://insideairbnb.com/get-the-data.html

Les données complètes (listings.csv) ont été récupérées, puis seules les variables jugées d'intérêt par l'auteur
ont été conservées pour les outils d'analyse univariée et bivariée.


## Liste des paquetages R nécessaires
 - shiny
 - shinythemes
 - shinyjs
 - leaflet
 - rgdal
 - RColorBrewer
 - ggplot2


## Auteur
[Matis Brassard-Verrier](https://www.linkedin.com/in/matis-brassard-verrier-372620152)
