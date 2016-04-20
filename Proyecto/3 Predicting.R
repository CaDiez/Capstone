library('data.table')

load(file='~/Data Science/10 Data Science Capstone/Capstone/Proyecto/Word-O-Matic/ProbNgramModel30.Rdata') # load the prob.adj variable
#load(file='~/Data Science/10 Data Science Capstone/Programas Auxiliares/prob.ngmrams.Rdata') # load the prob.adj variable

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
##' the table of words with the adjusted weight

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
    
    out.res <- res[, list(predicted.words = 
                            unlist(lapply(strsplit(s, ' '), '[', idx.l)),
                          weight)]
    out.l[[i]] <- head(data.frame(out.res), 10)
    i <- i + 1
  }
  
  
  ## Predict with skipngram
  out.res <- na.omit(rbindlist(out.l))
  out.res <- data.table(out.res)
  out.res <- out.res[, list(weight=mean(weight)), by=predicted.words]
  
  return(data.frame(out.res))
}
#Test the funcion, to try others change the variable s to another phrase and
#run PredictNextWord (s, prob.adj) again, the model and function will be used
#to implement the shiny app.
s <- 'I expect to write a '
PredictNextWord(s, prob.adj)