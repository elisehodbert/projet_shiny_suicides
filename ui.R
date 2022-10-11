#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
# Define UI for application that draws a histogram
shinyUI(
  # navbarPage
  navbarPage("Etude du taux de tentative de suicide",
             theme = bs_theme(version = 4, bootswatch = "flatly", fg = "#606060", bg = "#FFFFFF"),
             
             # premier onglet Data
             
             tabPanel("Présentation générale", 
                      navlistPanel(
                        
                        widths = c(2,10),
                        
                        #onglet présentation du projet
                        
                        tabPanel("Etude",
                                 h5("Notre projet porte sur l'analyse des résultats d'une enquête sur les comportements à risque, pouvant amener à une tentative de suicide."),
                                 br(),
                                 h3("Enquête"),
                                 br(),
                                 p("Cette enquête a été réalisée dans le cadre d’un projet de l’Organisation Mondiale de la Santé intitulé
                                 GSHS (Global school-based student health survey). Il s’agit d’un projet conçu pour aider les pays à analyser 
                                 les facteurs associés à des comportements à risque chez les jeunes âgés de 13 à 17 ans.
                                   Cette enquête a été menée dans plusieurs pays, en milieu scolaire. Des questionnaires ont été distribués 
                                   auprès d’élèves âgés de 13 à 17 ans, qui ont été sélectionnés par un 'standardized scientific 
                                   sample selection process'. Ces questionnaires comprennent 10 modules : alcohol use, dietary behaviours, 
                                   drug use, hygiene, mental health, physical activity, protective factors, sexual behaviours, 
                                   tobacco use et violence and unintentional injury.", align = "justify"),
                                 br(),
                                 p("Pour plus d'information sur cette enquête, vous pouvez aller sur ce lien : https://www.who.int/teams/noncommunicable-diseases/surveillance/systems-tools/global-school-based-student-health-survey . "),
                                 br(),
                                 h3("Notre démarche"),
                                 br(),
                                 p("Nous avons décidé d'étudier l'effet de plusieurs variables des modules, sur le taux de suicide.")),

                        #onglet Jeu de données

                        tabPanel("Jeu de données",
                                 radioButtons(inputId = "donnees", 
                                              label = "Choisissez la forme des données",
                                              selected = "Preview",
                                              choices = c("Preview", "Structure", "Summary")),
                                 
                                 verbatimTextOutput("affichage_donnees")),
                                 
                        
                        #onglet carte des pays étudiés

                        tabPanel("Pays étudiés",
                                 p("Pays étudiés"),
                                 leafletOutput("Carte1",width=900,height=600),
                                 p("Les pays étudiés sont colorés en noir, les autres pays sont colorés en blanc")),

                        #onglet carte 1
                        
                        tabPanel("Taux de suicides par pays",
                          wellPanel(width = 4,"",
                                        selectInput(inputId = "Year", label= "Pour quelle année souhaitez-vous afficher les données?", choices = sort(dta$Year)),
                                        selectInput(inputId = "Age_Group", label= "Pour quelle classe d'âge souhaitez vous afficher les données ? (unité: année)", choices = dta$Age_Group)
                              ),
                          mainPanel(width = 12,
                                        "", leafletOutput('Carte2'))

                            
                          
                        )#fin tab panel taux suicide par pays

                          )), ##fin du 1er onglet de présentation générale
             
             
 
             #Deuxième onglet AFM
             
             tabPanel("AFM",
                      navlistPanel(
                            widths = c(2, 10),
                            
                            #onglet démarche de l'AFM
                            
                            tabPanel("Démarche de l'AFM",
                                     h3("Jeu de données"),
                                     br(),
                                     p("Pour la réalisation de l'AFM, notre jeu de données est composé de 17 variables (13 variables quantitatives et 4 variables qualitatives)
                                     que nous avons décidé de regrouper selon les six groupes suivants :", align = "justify"),
                                     p("-Pays : variable 'Country'"),
                                     p("-Année : variable 'Year'"),
                                     p("-Civilité : variables 'Age_Group' et 'Sex'"),
                                     p("-Conso : variables liées à la consommation qui sont 'Currently_Drink_Alcohol', 'Really_Get_Drunk', 'Overweight', 'Use_marijuana' et 'Smoke_cig_curently'", align = "justify"),
                                     p("-Social : variables liées à l'environnement social 'Have_Understanding_parents', 'Missed_classes_without_permission', 'Had_sexual_relation', 'Had_fights', 'Bullied', 'Got_Seriously_injured' et 'No_close_friends'", align = "justify"),
                                     p("-Suicide : variable 'Attempted_suicide'"),
                                     p("Nous avons décidé de répartir les variables quantitatives en trois groupes. La variable 'Attempted_suicide' 
                                        est la variable réponse et nous voulons voir l'effet des autres variables quantitatives sur cette variable.
                                        Les autres variables ne renseignent pas sur les mêmes domaines, et nous trouvons intéressant de tester s'il y a une différence
                                        entre les effets des environnement sociaux des individus et leurs consommations personnelles.", align = "justify"),
                                     br(),
                                     h3("Réalisation de l'AFM"),
                                     br(),
                                     p("Pour construire notre AFM, nous utilisons les groupes 'Conso' et 'Social' en actifs. En variables supplémentaires, nous utilisons tout d'abord la variable quantitative 'Attempted_suicide'. Nous souhaitons voir 
                                     s'il existe un lien entre cette variable et les variables liées à la consommation et à l'environnement social des individus. Nous utilisons ensuite les groupes de variables qualitatives 'Pays', 'Année' et 'Civilité'. Nous souhaitons voir s'il y a un effet du pays, de l'année, et de la civiloité sur les
                                       variables actives", align = "justify"),
                                     br(),
                                     p("Nous avons choisi de réduire les variables afin de leur donner la même importance", align = "justify"),
                                     br(),
                                     p("Nous avons choisi d'interpréter seulement les deux premières dimensions de l'AFM, au vu du graphe des valeurs propres de cette dernière", align = "justify")
                                     ),
                            
                            #onglet sur l'étude des graphiques des individus et des variables
                            
                            tabPanel("Etude des graphiques des individus et des variables quantitatives",

                              fluidRow(
                                column(width=8,"Graphique des individus",
                                   plotOutput("graphe_afm1")),
                                column(width = 4, "Description du graphique des individus",
                                       br(),
                                       br(),
                                       p("En haut à droite : Samoa et Vanuata"),
                                       p("En bas à droite : Argentine et Seychelles"),
                                       p("A gauche : Indonesia"))),
                          
                              fluidRow(
                                column(width = 8, "Graphique des variables",
                                  plotOutput("graphe_afm2")),
                                column(width = 4, "Description du graphique des variables",
                                       br(),
                                       br(),
                                       p("La projection des variables n'est pas très bonne, quelque soit le groupe.", align = "justify"),
                                       br(),
                                       p("Les variables du groupes 'Social' sont plus dans la direction en haut à droite, et celles
                                         du groupe 'Conso' sont plus en bas à gauche. Cela montre qu'il y a peu de corrélation entre
                                         les groupes Social et Conso. Cette idée est renforcée par la valeur de RV qui est égale à 0.27 seulement", align = "justify"))),
                              
                              fluidRow("Interprétation de la variable quantitative supplémentaire",
                                       br(),
                                       br(),
                                       p("La variable 'Attempted_Suicide' est dans la direction en haut à droite. 
                                         Elle serait donc plus liée à environnement social ( et notamment aux variables suivantes : 'Bullied', 'Had fight', 'no close friends', 'Missed_cours_without_permissions') 
                                         qu'aux consommations", align = "justify"))
                              ),
                            
                            #onglet de présentation des variables qualitatives supplémentaires

                            tabPanel("Analyse des variables qualitatives supplémentaires",
                                     fluidRow(
                                       column(width=8, "Graphique des individus en fonction des pays",
                                              plotOutput("graphe_pays")
                                              ),
                                       column(width = 4, "Description du graphique",
                                              br(),
                                              br(),
                                              p("Toutes les ellipses de confiance se recoupent, hormis celle du pays Vanuatu. De manière générale, les deux premières dimensions 
                                              de l'AFM ne permettent pas de différencier significativement les pays.", align = "justify"))),
                                     fluidRow(
                                       column(width=8, "Graphique des individus en fonction des années",
                                              plotOutput("graphe_annees")
                                              
                                              ),
                                       column(width = 4, "Description du graphique",
                                              br(),
                                              br(),
                                              p("Toutes les ellipses de confiance se recoupent. De manière générale, les deux premières dimensions 
                                              de l'AFM ne permettent pas de différencier significativement les années de l'enquête.", align = "justify"))
                                     )
                                     ),
                            
                            #onglet conclusion de l'AFM
                            
                            tabPanel("Conclusion",
                                     h3("Conclusion"),
                                     br(),
                                     p("Cette AFM met en évidence la présence d'une différence entre les variables liées aux consommations
                                       et celles liées à l'environnement social. Le taux de suicide serait plus lié à ces dernières. Néanmoins,
                                       une analyse approchondie est nécessaire pour mettre en évidence ce lien.", align = "justify")
                                     )
                          
                            )# fin du navlistpanel
                      
                          ), ##fin du 2ème onglet AFM
             
             
             # troisième onglet "modèle et prédictions"
             
             tabPanel("Modèle et prédictions", 
                      navlistPanel(
                        widths = c(2, 10),
                        
                        
                        #onglet modèles et coefficients
                        


                       tabPanel("Modèle et coefficients",
                                flowLayout(width= 12, "Modèle : ", p(" Attempted_suicide ~ Country + Missed_classes_without_permssion + Had_fights + Bullied + Got_Seriously_injured")),

                          flowLayout("Coefficient intercept"),
                          flowLayout(tableOutput("coef_intercept")),
                          flowLayout("Coefficient des pays"),
                          flowLayout(tableOutput("coef_pays1")),
                          flowLayout(tableOutput("coef_pays2")),
                          flowLayout("Coefficient des var. quantitatives"),
                          flowLayout(tableOutput("coef_quanti"))
                                  ),
                     
                        
                      
                      #Onglet PREDICTION
                      
                        tabPanel("Faire une prédiction",
                          fluidRow(
                            sidebarLayout(
                              
                              #première colonne: on entre les pourcentages (sur le teco)
                              sidebarPanel(
                                width = 4, "Entrez vos données",
                            selectInput(inputId = "Country", label= "Dans quels pays souhaitez vous faire la prédiction?", choices = dta$Country),

                            numericInput(inputId = "Missed_classes_without_permssion", 
                                         label = "Quel est le pourcentage qui ont séché des cours?", value= NA, min = 0 , max = 100),
                            
                            numericInput(inputId = "Had_fights", 
                                         label = "Quel est le pourcentage de jeunes s'étant déjà battus?", value= NA, min = 0 , max = 100),
                            
                            numericInput(inputId = "Bullied", 
                                         label = "Quel est le pourcentage de jeunes s'étant fait harceler?", value= NA, min = 0 , max = 100),
                            
                            numericInput(inputId = "Got_Seriously_injured", 
                                         label = "Quel est le pourcentage de jeunes ayant déjà été sérieusement blessé physiquement?", value= NA, min = 0 , max = 100),
                            
                            verbatimTextOutput(outputId = "result")
                            ),
                            

                            mainPanel("Carte",leafletOutput('Carte3',width = 650, height = 500))
                                       
                                
                          
                            )
                            )
                          )
                        )                            
                      )
  )
)
            
  
                            
                        
                      
                        


