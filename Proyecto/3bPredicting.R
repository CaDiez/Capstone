library('data.table')

load(file='~/Data Science/10 Data Science Capstone/Capstone/Proyecto/prob.ngmrams.Rdata') # load the prob.adj variable

##' Clean the String inputs from the users
##'
##' Cleans the strings for prediction
##' @title Clean input string  
##' @param s, a string character
##' @return a clean version of the argument (lowercase, no punctuation)
##' @author Carlos G
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
##' @title Predict next word
##' @param s, a string and we try to predict the next word
##' @param prob.adj.init a list of adjusted probability
##' @return a table with words
##' @author Carlos G
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
  # print(out.res)
  out.res <- out.res[, list(weight=mean(weight)), by=predicted.words]
  
  
  ## rownames(out.res) <- NULL
  return(data.frame(out.res))
}

s <- 'I expect to write a '
PredictNextWord(s, prob.adj)