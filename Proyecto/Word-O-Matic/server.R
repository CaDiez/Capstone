# Define server logic required to summarize and view the selected dataset
shinyServer(
  function(input, output, session) {
    #Loading libraries
    library('wordcloud')
    library('xtable')
    library('shiny')        
    library('data.table')
    
    ptm <- proc.time()
    
    ##' Clean the String inputs from the users
    ##'
    ##' Cleans the strings for prediction
    CleanInputString <- function(s){
      s <- tolower(s)
      s <- gsub("(\\w)-(\\w)", "\\1\1dd\\2", s)
      s <- gsub("(\\w)'(\\w)", "\\1\2dd\\2", s)
      s <- gsub("[[:punct:]]+", "", s)
      s <- gsub("\1dd", "-", s, fixed = TRUE)
      s <- gsub("\2dd", "'", s, fixed = TRUE)
      return(s)
    }
    
    ##' Word Prediction
    ##'
    ##' Predict the following words by looking up on the table and provide
    ##' the table of words with the adjusted probabilities
    PredictNextWord <- function(s, prob.adj.init){
      
      s <- CleanInputString(s)
      s.l <- strsplit(tolower(s), ' ')[[1]]
      n <- min(length(s.l), 2)
      prob.adj <- lapply(prob.adj.init, function(x) copy(x))
      ## Predict usual n-gram
      idxs <- list((-(n-1):0) + length(s.l), (-2*(n-1):0) + length(s.l))
      
      out.l <- list()
      i <- 1 # bad
      for(idx in idxs){
        s. <- paste0(s.l[idx], collapse=' ') 
        res <- prob.adj$n3[s.][order(-weight)]
        idx.l <- 3  
        if (any(is.na(res$s))){
          s.2 <- paste0(s.l[idx[-1]], collapse=' ')
          res <- prob.adj$n2[s.2][order(-weight)]
          idx.l <- 2
        } 
        out.res <- res[, list(predict.word = 
                                unlist(lapply(strsplit(s, ' '), '[', idx.l)),
                              weight)]
        out.l[[i]] <- head(data.frame(out.res), 10)
        i <- i + 1
      }      
      ## Predict with skipngram
      out.res <- na.omit(rbindlist(out.l))
      out.res <- data.table(out.res)
      # print(out.res)
      out.res <- out.res[, list(weight=mean(weight)), by=predict.word]
      ## rownames(out.res) <- NULL
      return(data.frame(out.res))
    }
    
    #Set progress bar fou user
    progress <- shiny::Progress$new(session, min=1, max=500)
    on.exit(progress$close())
    progress$set(message = 'Loading libraries and data model')
    
    #Load prediction model
    load(file='ProbNgramModel30.Rdata') # load the Probabilistic 30% Model
    
    #Set timer to help loading libraries & model
    for (i in 1:500) {
      progress$set(value = i)
    }    
    

    #source('Predictor.R')
    
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
      proc.time() - ptm
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
