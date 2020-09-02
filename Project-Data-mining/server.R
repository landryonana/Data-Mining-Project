# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
library(shiny)
library(cluster)
library(rpart)
library(rpart.plot)

source('C:/Users/JESUS-CHRIST/Desktop/TP-Data-mining/TP-M1-2.R', encoding = 'UTF-8')
source('C:/Users/JESUS-CHRIST/Desktop/TP-Data-mining/main.R')

function(input, output) {
  
#=====================================TP 1==========================================
  
  output$summary = renderPrint({
    summary(vente)
  })
  
  output$tableTP1 = DT::renderDataTable({
    DT::datatable(vente)
  })
  
  output$rule = DT::renderDataTable({
    DT::datatable(rule_sans) 
  })

  #===================================================================================
  
  #=====================================TP 2==========================================
  
  #summary des données
  output$summary2 = renderPrint({
    str(data)
    summary(data)
    
  })
  
  output$table2 = DT::renderDataTable({
    
    DT::datatable(data)
    
  })
  
  #Prétraitement
  output$tableP = DT::renderDataTable({
    
    if( input$dataP == "Suppression des Valeurs manquantes"){
      DT::datatable(data_sans_NAs)
    }else{
      if( input$dataP == "Remplacement des Valeurs manquantes"){
      DT::datatable(data_pr_2)
      }else{
        return()
      }
    }
    
  })
  
#===================================Classification supervisée  
  #Arbre de décision
  output$testArbre = renderPrint({
    print(tabTree1)
    print(tabTree2)
    print(precision)
  })
    #plot de l'arbre
  output$arbrePlot = renderPlot({
    rpart.plot(tree, extra = 4)
  })
  
  #Réseaux de Neurone
  output$testneurone = renderPrint({
    print(nerone)
    print(tabNet_neurone1)
    print(tabNet_neurone2)
          
  })
    
  output$neurone = renderPlot({
      plot(neroneNeural, rep = "best")
  })
#===================================Classification non supervisée  
  #l’algorithme kmeans
  output$kmeansPlot = renderPlot({
    clusplot(data_sans_NA,lines=2,algo_kmeans$clustering)
    
  })
  
  #Dendrogramme
  output$classNhPlot = renderPlot({
    plot(cah.ward)
  },width = 1500, height = 1500)
  
#===================================================================================
  
}