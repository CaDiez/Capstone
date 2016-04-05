# Define server logic required to summarize and view the selected dataset
shinyServer(
  function(input, output, session) {
    #Set progress bar fou user
    progress <- shiny::Progress$new(session, min=1, max=3000)
    on.exit(progress$close())
    progress$set(message = 'Loading libraries and data model')
    
    #Loading libraries
    ##library('NLP')
    ##library('slam')
    ##library('tm')
    ##library('data.table')
    ##library('RColorBrewer')
    ##library('stringr')
    ##library('hashFunction')
    ##library('parallel')
    library('wordcloud')
    library('xtable')
    library('shiny')
    #Load prediction model
    source('Predictor.R')
    
    #Set timer to help loading libraries & model
    for (i in 1:3000) {
      progress$set(value = i)
      #Sys.sleep(0.5)
    }
    
    #Main function, gets reactions of user an generates prediction words
    generateWordList <- reactive({
      inputStr <- input$wt_1
      ptm <- proc.time()
      #Gets prediction and orders output
      myData<-PredictNextWord(inputStr, prob.adj)
      predictedWords<-myData[order(myData$weight, decreasing=TRUE), ]
      awc<<-input$a_wc
      #Analize user iteractions with the buttons
      for (e in names(input)){                
        inputTest <- unlist(gregexpr('labels_', e))
        if (inputTest == -1){
          next
        } 
        if (input[[e]][1] == 1){
          a<-as.numeric(gsub("[^0-9]","",e,""))
          newWord<-predictedWords[a,1]
          inputStr <- sub("\\s+$", "", inputStr)
          newText<-paste(inputStr, newWord, sep=' ')
          updateTextInput(session, "wt_1", value = newText)
          break
        }
      }  
      #Calculates processing time
      sysTime<-proc.time() - ptm
      sysTime <<- (ptm["sys.self"])
      #Return predicted words ordered by weight
      return(predictedWords)  
    })
    
    #Creates a list of buttons that contains the predicted words
    output$Dynamic <- renderUI({
      predictedWords<-generateWordList()
      buttonList <- vector("list") 
      for(i in 1:nrow(predictedWords)){
        labels<-predictedWords[i,1]
        buttonList[[i]] <- list(actionButton(
        inputId <- paste0('labels_',as.character(i)), label = labels, value = labels,
        style = "background-color:red;color:white;width:6em"))
      } 
      return(buttonList)
    })
    
    #Creates the table that shows predicted words and their weights
    output$matrix <- renderTable({
      predictedWords<-generateWordList()
      xtable(predictedWords)
    }, include.rownames = FALSE, sanitize.text.function = function(x) x)
    
  #Renders processing time
    output$time <- renderText({
      x<-generateWordList()
      return(paste0(sysTime, ' seconds'))
    })
    #Renders Word Cloud  
    output$plot <- renderPlot({
      predictedWords<-generateWordList()
      if (awc == TRUE){
       wordcloud(predictedWords$predict.word, predictedWords$weight,                 
                colors=brewer.pal(5, "Dark2"))
      }
    })   
  })
