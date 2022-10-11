#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output){
  
  
  # affichage des données
  output$affichage_donnees <- renderPrint({
    affichage <- switch(input$donnees,
                   Preview = head,
                   Structure = str,
                   Summary = summary)
    
    affichage(dta)
  })
  # graphes afm
  output$graphe_afm1 <- renderPlot({
    plot.MFA(afm,choix = c("ind"), title = "Graphique des individus", habillage = "Country", lab.grpe = FALSE, lab.var = FALSE)
  })
  
  output$graphe_afm2 <- renderPlot({
    plot.MFA(afm, choix = c("var"), title = "Graphique des variables")
  })
  
  output$graphe_pays <- renderPlot({
    plotellipses(afm, keepvar = "Country", title = "Ellipses de confiance en fonction des pays")
  })
  
  output$graphe_annees <- renderPlot({
    plotellipses(afm, keepvar = "Year", title = "Ellipses de confiance en fonction des années")
  }) 
  
  
  

  #cartographie


  World <- geojsonio::geojson_read("./ne_10m_admin_0_scale_rank_minor_islands/ne_10m_admin_0_scale_rank_minor_islands.shp", what = "sp")
  World@data[["sr_subunit"]] <- as.factor(World@data[["sr_subunit"]])
  all_countries <- World@data[["sr_subunit"]]
  select_countries <- data_afm$Country
  
  #carte 1
  
  col_countries_carte1 <- sapply(all_countries,function(x){
    if (x %in% select_countries){
      valeur = 1
    }
    else{
      valeur = 0
    }
    return (valeur)
  })
  
  factpal <- colorFactor(c("White","Black"), col_countries_carte1)
  
  output$Carte1 <- renderLeaflet({
    leaflet(World) %>% 
      addTiles() %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3, 
                  fillOpacity = 1,
                  fillColor = ~factpal(col_countries_carte1), 
                  popup=~World@data[["sr_subunit"]]) %>%
      setView(0, 0, zoom = 1.5)

  })

  
  # carte 2
  tab2 = reactive({
    subset(
      data_afm,
      as.character(Age_Group) == as.character(input$Age_Group) &
        as.character(Year) == as.character(input$Year))
  })
    
    output$Carte2 <- renderLeaflet({
      col_countries_carte2 <- sapply(all_countries,function(x){
        if (!(x %in% data_afm$Country)){ # si le pays n'a pas été étudié
          valeur = 0
        }
        else if (!(x %in% tab2()$Country)){ # si pas de donnée pour ce pays et cette année
          valeur = NA
        }
        else { # sinon
          valeur = mean(tab2()$Attempted_suicide[which(as.character(tab2()$Country) == as.character(x))])
        }
        return (valeur)
      })
      
      pal <- colorBin(("BuPu"), domain = col_countries_carte2)
      
      labels_carte2_femmes <- sapply(all_countries, function(x){
        if (!(x %in% data_afm$Country)){
          valeur = "Aucune donnée"
        }
        else if (!(x %in% tab2()$Country)){
          valeur = "Pas de données pour cette année"
        }
        else {
          valeur = tab2()$Attempted_suicide[which(as.character(tab2()$Country) == as.character(x) & tab2()$Sex == "Female")]
        valeur = round(valeur,2)
          }
        return (valeur)
      })
      
      labels_carte2_hommes <- sapply(all_countries,function(x){
        if (!(x %in% data_afm$Country)){
          valeur = "Aucune donnée"
        }
        else if (!(x %in% tab2()$Country)){
          valeur = "Pas de données pour cette année"
        }
        else { # sinon
          valeur = tab2()$Attempted_suicide[which(as.character(tab2()$Country) == as.character(x) & tab2()$Sex == "Male")]
        valeur=round(valeur,2)
          }
        return (valeur)
      })
      
      if ( !(input$Year == "") & !(input$Age_Group == "") & nrow(tab2())>1){
      leaflet(World) %>%
        addTiles() %>%
        addPolygons(stroke = FALSE,
                    smoothFactor = 0.3,
                    fillOpacity = 1,
                    fillColor = ~pal(col_countries_carte2),
                    popup=~paste("Pays : ",
                                 World@data[["sr_subunit"]],
                                  "<br>",
                                  "Taux de tentatives de suicides des femmes: ",
                                  "<br>",
                                  labels_carte2_femmes,
                                  "<br>",
                                  "Taux de tentatives de suicides des hommes: ",
                                  "<br>",
                                  labels_carte2_hommes)) %>%
        setView(0, 0, zoom = 1.5) %>%
        addLegend("topright",
                  pal = pal,
                  values = ~col_countries_carte2,
                  title = "Taux de tentatives de suicide moyen pour les hommes et les femmes",
                  opacity = 1,
                  na.label = paste("Pas de données pour cette année"))
      }
      else{
        leaflet(World) %>%
          addTiles() %>%
          setView(0, 0, zoom = 1.5)
      }
      
      
  })
    

  # carte 3
    
    output$Carte3 <- renderLeaflet({
      
      col_countries_carte3 <- sapply(all_countries,function(x){
        if (!(x %in% data_afm$Country)){ # si le pays n'a pas été étudié
          valeur = 0
        } 
        else { # sinon 
          coef1 = paste0("Country - ",x)
      
          valeur = modele.complet.bis9$Ttest["(Intercept)",1] +
            modele.complet.bis9$Ttest[coef1,1]+
            as.numeric(input$Missed_classes_without_permssion)*modele.complet.bis9$Ttest["Missed_classes_without_permssion",1]+
            as.numeric(input$Had_fights)*modele.complet.bis9$Ttest["Had_fights",1]+
            as.numeric(input$Bullied)*modele.complet.bis9$Ttest["Bullied",1]+
            as.numeric(input$Got_Seriously_injured)*modele.complet.bis9$Ttest["Got_Seriously_injured",1]

          }
        return (valeur)
      })
      
      labels_carte3 <- sapply(all_countries,function(x){
        if (!(x %in% data_afm$Country)){ # si le pays n'a pas été étudié
        valeur = "Aucune donnée"
      }
      else {
        coef1 = paste0("Country - ",x)
        valeur = modele.complet.bis9$Ttest["(Intercept)",1] +
          modele.complet.bis9$Ttest[coef1,1]+
          input$Missed_classes_without_permssion*modele.complet.bis9$Ttest["Missed_classes_without_permssion",1]+
          input$Had_fights*modele.complet.bis9$Ttest["Had_fights",1]+
          input$Bullied*modele.complet.bis9$Ttest["Bullied",1]+
          input$Got_Seriously_injured*modele.complet.bis9$Ttest["Got_Seriously_injured",1]
      valeur = round(valeur,2)
        }
      return (valeur)
      })
      
      pal <- colorBin(("BuPu"), domain = col_countries_carte3)
      
      if (!is.na(input$Missed_classes_without_permssion) &
          !is.na(input$Had_fights) &
          !is.na(input$Bullied) &
          !is.na(input$Got_Seriously_injured)
          ){
        leaflet(World) %>%
          addTiles() %>%
          addPolygons(stroke = FALSE,
                      smoothFactor = 0.3,
                      fillOpacity = 1,
                      fillColor = ~pal(col_countries_carte3),
                      popup=~paste("Pays : ",
                                   World@data[["sr_subunit"]],
                                   "<br>",
                                   "Taux de tentatives de suicides prédit : ",
                                   labels_carte3)) %>%
        setView(0, 0, zoom = 1.4) %>%
        addLegend("topright",
                  pal = pal,
                  values = ~col_countries_carte3,
                  title = "Taux de tentatives de suicide prédit",
                  opacity = 1)
      }
      else{
        leaflet(World) %>%
          addTiles() %>%
          setView(0, 0, zoom = 1.4)
      }
      })
  #output des coefficients du modele
  output$coef_intercept <- renderTable({
    coefficients_intercept
  })
  output$coef_pays1 <- renderTable({
    coefficients_pays1
  })
  output$coef_pays2 <- renderTable({
    coefficients_pays2
  })
  output$coef_quanti <- renderTable({
    coefficients_quanti
  })
  output$titre_inter <- renderText({
    "Coefficient intercept"
  })
  
  # autre
  output$result <- renderPrint({
    coef_country <- paste0("Country - ", as.character(input$Country))
    if (!is.na(input$Missed_classes_without_permssion) && 
        !is.na(input$Had_fights) &&
        !is.na(input$Bullied) && 
        !is.na(input$Got_Seriously_injured)){
      estimate <-  modele.complet.bis9$Ttest["(Intercept)",1] + modele.complet.bis9$Ttest[coef_country,1]+
      as.numeric(input$Missed_classes_without_permssion)*modele.complet.bis9$Ttest["Missed_classes_without_permssion",1]+
      as.numeric(input$Had_fights)*modele.complet.bis9$Ttest["Had_fights",1]+
      as.numeric(input$Bullied)*modele.complet.bis9$Ttest["Bullied",1]+
    (input$Got_Seriously_injured)*modele.complet.bis9$Ttest["Got_Seriously_injured",1]
     }
    else {
      estimate <- 0
    }
    print(paste("Le taux de suicide estimé est de : ", round(estimate,digits = 2),"%."))
  })
})



 
