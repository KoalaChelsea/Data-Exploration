#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(xlsx)
library(DT)
library(plotly)
library(RColorBrewer)
library(openssl)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$fileInclude <- renderUI({
    h3("CSV(.csv), TXT(.txt), Excel(.xlsx) dataset.")
  })
  
  readData <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    
    if(length(grep(".txt", inFile,ignore.case = TRUE)) > 0){
      datafile <- read.table(inFile$datapath, header = input$header)
    }else if(length(grep(".csv", inFile, ignore.case = TRUE)) > 0){
      datafile <- read.csv(inFile$datapath,header = input$header, sep=",")
    }else if(length(grep(".xlsx", inFile, ignore.case = TRUE)) > 0){
      datafile <- read.xlsx(inFile$datapath, 1)
    }
  })
    
  # varnames <- reactive({
  #   datafile <- readData()
  #   names(datafile)[sapply(datafile,is.numeric)]
  # })
  # 
  # factornames <- reactive({
  #   datafile <- readData()
  #   names(datafile)[sapply(datafile,is.factor)]
  # })
  # 
  # 
  # output$numeric <- renderUI({ 
  #   selectInput("selectNum", "Select a numerical variable", varnames(),selected= "None")
  # })
  # 
  # output$factor <- renderUI({ 
  #   selectInput("selectFactor", "Select a categorical variable", factornames(),selected= "None")
  # })
  
  output$display <- renderDataTable({
    datafile <- readData()
    datatable(datafile)
  })
  
  output$summary <- renderPrint({
    datafile <- readData()
    summary(datafile)
  })
  
  output$str <- renderPrint({
    datafile <- readData()
    str(datafile)
  })
  output$variable1 <- renderUI({ 
    datafile <- readData()
    selectInput("select1", "Select the first variable", colnames(datafile))
  })
  
  output$variable2 <- renderUI({ 
    datafile <- readData()
    selectInput("select2", "Select the second variable", colnames(datafile), selected = names(datafile)[[2]])
  })
  output$plot1 <- renderPlot({
    datafile <- readData()
    variable = input$select1

    if (is.numeric(datafile[,variable])){
      ggplot(datafile, aes(x = datafile[,variable])) +
      geom_histogram(aes(y = ..count..),
                     colour = "#1F3552", fill = "#4271AE") +
      scale_x_continuous(name = paste0(variable)) +
      scale_y_continuous(name = "Count")+
      ggtitle("Histogram for first variable",variable)+
      theme_bw()
    }
    else if(is.factor(datafile[,variable])){
      if(sum(sapply(datafile, is.factor)) == 1){
        ggplot(datafile, aes(x = datafile[,variable])) +
          geom_bar(width=0.4, fill="lightblue3")+
          scale_x_discrete(name = paste0(variable)) +
          scale_y_continuous(name = "Count")+
          ggtitle("BarChat for first variable",variable)+
          theme_bw()
      }else{
        ggplot(datafile, aes(x = datafile[,variable])) +
          geom_bar(fill = brewer.pal(length(unique(datafile[,variable])), "Set3")) +
          scale_x_discrete(name = paste0(variable)) +
          scale_y_continuous(name = "Count")+
          ggtitle("BarChat for first variable",variable)+
          theme_bw()
      }
      
    }
    else{
      print("Invalid")

    }

    
  })
  
  output$plot2 <- renderPlot({
    datafile <- readData()
    variable = input$select2
    
    if (is.numeric(datafile[,variable])){
      ggplot(datafile, aes(x = datafile[,variable])) +
        geom_histogram(aes(y = ..count..),
                       colour = "#1F3552", fill = "#4271AE") +
        scale_x_continuous(name = paste0(variable)) +
        scale_y_continuous(name = "Count")+
        ggtitle("Histogram for second varible",variable)+
        theme_bw()
    }
    else if(is.factor(datafile[,variable])){
      if(sum(sapply(datafile, is.factor)) == 1){
        ggplot(datafile, aes(x = datafile[,variable])) +
          geom_bar(width=0.4, fill="lightblue3")+
          scale_x_discrete(name = paste0(variable)) +
          scale_y_continuous(name = "Count")+
          ggtitle("BarChat for first variable",variable)+
          theme_bw()
      }else{
        ggplot(datafile, aes(x = datafile[,variable])) +
          geom_bar(fill = brewer.pal(length(unique(datafile[,variable])), "Set3")) +
          scale_x_discrete(name = paste0(variable)) +
          scale_y_continuous(name = "Count")+
          ggtitle("BarChat for first variable",variable)+
          theme_bw()
      }
    }
    else{
      print("Invalid")
      
    }
    
  })
  
  
  output$plot3 <- renderPlot({
    datafile <- readData()
    variable1 = input$select1
    variable2 = input$select2
    
    if (is.numeric(datafile[,variable1]) && is.numeric(datafile[,variable2])){
      ggplot(datafile, aes(x=datafile[,variable1], y=datafile[,variable2], color = datafile[,variable1])) + 
        geom_point(shape = 16, size = 4, show.legend = FALSE)+
        ylab(variable2)+
        xlab(variable1)+
        ggtitle("Scatterplot for two variables")+
        theme_bw()
    }
    else if(is.factor(datafile[,variable1]) && is.numeric(datafile[,variable2])){
      ggplot(datafile, aes(datafile[,variable1], datafile[,variable2]))+
        geom_boxplot(fill = "deepskyblue4", colour = "#1F3552", alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20)+
        ylab(variable2)+
        xlab(variable1)+
        ggtitle("Boxplot for two variables")+
        theme_bw()
    }
    else if(is.factor(datafile[,variable2]) && is.numeric(datafile[,variable1])){
      ggplot(datafile, aes(datafile[,variable2], datafile[,variable1]))+
        geom_boxplot(fill = "deepskyblue4", colour = "#1F3552", alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20)+
        ylab(variable1)+
        xlab(variable2)+
        ggtitle("Boxplot for two variables")+
        theme_bw()
    } 
    else if(is.factor(datafile[,variable1]) && is.factor(datafile[,variable2])){
      #CrossTable(datafile[,variable1], datafile[,variable2])
      ggplot(datafile, aes(datafile[,variable1], ..count..)) + geom_bar(aes(fill = datafile[,variable2]), position = "dodge")
    }
    else{
      print("Invalid")
      
    }
    
  })
  
  
  # output$numplots <- renderPlot({
  #   datafile <- readData()
  #   variable = input$selectNum
  #   
  #   ggplot(datafile, aes(x = datafile[,variable])) +
  #     geom_histogram(aes(y = ..count..),
  #                    colour = "#1F3552", fill = "#4271AE") +
  #     scale_x_continuous(name = paste0(variable)) +
  #     scale_y_continuous(name = "Count")+
  #     ggtitle("Histogram for ",variable)+
  #     theme_bw()
  # })
  # 
  # output$factorplots <- renderPlot({
  #   
  #   datafile <- readData()
  #   variable = input$selectFactor
  #   
  #   validate(
  #     need(sum(sapply(datafile, is.factor)) > 1,
  #          message = "Less than 2 categorical variables")
  #   )
  # 
  #   ggplot(datafile, aes(x = datafile[,variable])) +
  #     geom_bar()+
  #     #geom_bar(fill = brewer.pal(length(unique(datafile[,variable])), "Set3")) +
  #     scale_x_discrete(name = paste0(variable)) +
  #     scale_y_continuous(name = "Count")+
  #     ggtitle("BarChat for ",variable)+
  #     theme_bw()
  # })
  # 
  # 
  # output$mixplots <- renderPlot({
  #   
  #   datafile <- readData()
  #   variable1 = input$selectNum
  #   variable2 = input$selectFactor
  #   legend_title <- variable2
  #   
  #   ggplot(datafile, aes(x = datafile[,variable1])) +
  #     geom_histogram(color="black", aes(fill=datafile[,variable2])) + 
  #     scale_x_continuous(name = paste0(variable1)) +
  #     scale_y_continuous(name = "Count")+
  #     ggtitle("Histogram with group",variable1)+
  #     theme_bw()+
  #     guides(fill=guide_legend(title=variable2))
  #   
  # })
  # 
  
  ############################################################
  ###########################################################
  
  
  
  readsamplefile <- reactive({
    if(input$sampleData==1){
      datafile <- read.csv("Diamonds.csv", sep = ",", header=TRUE)			
    } else if(input$sampleData==2) {
      datafile <- read.csv("Flash.csv", sep = ",", header=TRUE)		
    }
  })
  
  output$samplevariable1 <- renderUI({
    datafile <- readsamplefile()
    selectInput("select1", "Select the first variable", colnames(datafile))
  })
  
  output$samplevariable2 <- renderUI({ 
    datafile <- readsamplefile()
    selectInput("select2", "Select the second variable", colnames(datafile), selected = names(datafile)[[2]])
  })
  
  output$sampledisplay <- renderDataTable({
    datafile <- readsamplefile()
    datatable(datafile)
  })
  
  output$samplesummary <- renderPrint({
    datafile <- readsamplefile()
    summary(datafile)
  })
  
  output$samplestr <- renderPrint({
    datafile <- readsamplefile()
    str(datafile)
  })
  
  output$sampleplot1 <- renderPlot({
    datafile <- readsamplefile()
    variable = input$select1
    
    if (is.numeric(datafile[,variable])){
      ggplot(datafile, aes(x = datafile[,variable])) +
        geom_histogram(aes(y = ..count..),
                       colour = "#1F3552", fill = "#4271AE") +
        scale_x_continuous(name = paste0(variable)) +
        scale_y_continuous(name = "Count")+
        ggtitle("Histogram for first variable",variable)+
        theme_bw()
    }
    else if(is.factor(datafile[,variable])){
      if(sum(sapply(datafile, is.factor)) == 1){
        ggplot(datafile, aes(x = datafile[,variable])) +
          geom_bar(width=0.4, fill="lightblue3")+
          scale_x_discrete(name = paste0(variable)) +
          scale_y_continuous(name = "Count")+
          ggtitle("BarChat for first variable",variable)+
          theme_bw()
      }else{
        ggplot(datafile, aes(x = datafile[,variable])) +
          geom_bar(fill = brewer.pal(length(unique(datafile[,variable])), "Set1")) +
          scale_x_discrete(name = paste0(variable)) +
          scale_y_continuous(name = "Count")+
          ggtitle("BarChat for first variable",variable)+
          theme_bw()
      }
      
    }
    else{
      print("Invalid")
      
    }
    
    
  })
  
  output$sampleplot2 <- renderPlot({
    datafile <- readsamplefile()
    variable = input$select2
    
    if (is.numeric(datafile[,variable])){
      ggplot(datafile, aes(x = datafile[,variable])) +
        geom_histogram(aes(y = ..count..),
                       colour = "#1F3552", fill = "#4271AE") +
        scale_x_continuous(name = paste0(variable)) +
        scale_y_continuous(name = "Count")+
        ggtitle("Histogram for second varible",variable)+
        theme_bw()
    }
    else if(is.factor(datafile[,variable])){
      if(sum(sapply(datafile, is.factor)) == 1){
        ggplot(datafile, aes(x = datafile[,variable])) +
          geom_bar(width=0.4, fill="lightblue3")+
          scale_x_discrete(name = paste0(variable)) +
          scale_y_continuous(name = "Count")+
          ggtitle("BarChat for first variable",variable)+
          theme_bw()
      }else{
        ggplot(datafile, aes(x = datafile[,variable])) +
          geom_bar(fill = brewer.pal(length(unique(datafile[,variable])), "Set1")) +
          scale_x_discrete(name = paste0(variable)) +
          scale_y_continuous(name = "Count")+
          ggtitle("BarChat for first variable",variable)+
          theme_bw()
      }
    }
    else{
      print("Invalid")
      
    }
    
  })
  
  
  output$sampleplot3 <- renderPlot({
    datafile <- readsamplefile()
    variable1 = input$select1
    variable2 = input$select2
    
    if (is.numeric(datafile[,variable1]) && is.numeric(datafile[,variable2])){
      ggplot(datafile, aes(x=datafile[,variable1], y=datafile[,variable2], color = datafile[,variable1])) + 
        geom_point(shape = 16, size = 4, show.legend = FALSE)+
        ylab(variable2)+
        xlab(variable1)+
        ggtitle("Scatterplot for two variables")+
        theme_bw()
    }
    else if(is.factor(datafile[,variable1]) && is.numeric(datafile[,variable2])){
      ggplot(datafile, aes(datafile[,variable1], datafile[,variable2]))+
        geom_boxplot(fill = "deepskyblue4", colour = "#1F3552", alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20)+
        ylab(variable2)+
        xlab(variable1)+
        ggtitle("Boxplot for two variables")+
        theme_bw()
    }
    else if(is.factor(datafile[,variable2]) && is.numeric(datafile[,variable1])){
      ggplot(datafile, aes(datafile[,variable2], datafile[,variable1]))+
        geom_boxplot(fill = "deepskyblue4", colour = "#1F3552", alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20)+
        ylab(variable1)+
        xlab(variable2)+
        ggtitle("Boxplot for two variables")+
        theme_bw()
    } 
    else if(is.factor(datafile[,variable1]) && is.factor(datafile[,variable2])){
      #CrossTable(datafile[,variable1], datafile[,variable2])
      ggplot(datafile, aes(datafile[,variable1], ..count..)) + 
        geom_bar(aes(fill = datafile[,variable2]), position = "dodge")+
        ylab("Count")+
        xlab(variable1)+
        guides(fill=guide_legend(title=variable2))+
        ggtitle("Boxplot for two variables")+
        theme_bw()
    }
    else{
      print("Invalid")
      
    }
    
  })
  
  diamondsdata = read.csv("Diamonds.csv")
  flashdata = read.csv("Flash.csv", sep = ",", header=TRUE)
  output$downloadData <- downloadHandler(
    # if(input$sampleData==1){
    #   filename = function() {
    #      paste('dimonds_', Sys.Date(), '.csv', sep='') 
    #   },
    #   content = function(file) {
    #      write.csv(diamondsdata, file)
    #   }
    # }else if(input$sampleData==2) {
      filename = function() {
        paste('diamonds_', Sys.Date(), '.csv', sep='') 
      },
      content = function(file) {
        write.csv(diamondsdata, file)
      }
    #}
    
    
  )
  
  output$downloadPlotPNG <- downloadHandler(
    
    
    filename <- function() { paste('exploration.png') },
    content <- function(file) {
      png(file)
    }
  )
  
  ###################################################################
  ##################################################################
  MData <- reactive({
    if(is.null(input$myData)) {
      return(NULL)
    }
    
    # myRow <- NULL
    # validate(
    #   need(!is.null(myRow),
    #        message = "Please paste some columns into box")
    # )
    
    tmp <- matrix(strsplit(input$myData, "\n")[[1]])
    mySep <- switch(input$mysep, '1'=",",'2'="\t",'3'=";")
    myCol <- strsplit(tmp[1], mySep)[[1]]
    data <- matrix(0, length(tmp)-1, length(myCol))
    colnames(data) <- myCol
    for(i in 2:length(tmp)){
      myRow <- as.numeric(strsplit(paste(tmp[i],mySep,mySep,sep=""), mySep)[[1]])
      data[i-1,] <- myRow[-length(myRow)]
    }
    data <- data.frame(data)	
    return(data)
    
  })
  
  
  output$filetable <- renderTable({
    print(MData())
  })
  
  varnames <- reactive({
      datafile <- MData()
      names(datafile)[sapply(datafile,is.numeric)]
    })
  
  output$pastevariable1 <- renderUI({
    datafile <- MData()
    selectInput("select1", "Select a variable", varnames(),selected= "None")
  })
  
  
  output$pastesummary <- renderPrint({
    datafile <- MData()
    summary(datafile)
  })
  
  output$pastestr <- renderPrint({
    datafile <- MData()
    str(datafile)
  })
  
  output$pasteplot1 <- renderPlot({
    datafile <- MData()
    variable = input$select1
    
      ggplot(datafile, aes(x = datafile[,variable])) +
        geom_histogram(aes(y = ..count..),
                       colour = "#1F3552", fill = "#4271AE") +
        scale_x_continuous(name = paste0(variable)) +
        scale_y_continuous(name = "Count")+
        ggtitle("Histogram for first variable",variable)+
        theme_bw()
    
  })
  
  output$pasteplot2 <- renderPlot({
    datafile <- MData()
    variable = input$select2
    
    ggplot(datafile, aes(x = datafile[,variable])) +
      geom_histogram(aes(y = ..count..),
                     colour = "#1F3552", fill = "#4271AE") +
      scale_x_continuous(name = paste0(variable)) +
      scale_y_continuous(name = "Count")+
      ggtitle("Histogram for first variable",variable)+
      theme_bw()
    
  })
  
  output$pasteplot3 <- renderPlot({
    datafile <- MData()
    variable1 = input$select1
    variable2 = input$select2
    
      ggplot(datafile, aes(x=datafile[,variable1], y=datafile[,variable2], color = datafile[,variable1])) + 
        geom_point(shape = 16, size = 4, show.legend = FALSE)+
        ylab(variable2)+
        xlab(variable1)+
        ggtitle("Scatterplot for two variables")+
        theme_bw()
  })  
  
})