#Create a probabililistic Model of the tokenized and cleaned data in step 1

library('tau')
library('tm')
library('Matrix')
library("RWeka")
library('ggplot2')
library('data.table')
library('reshape2')
library('parallel')
options('mc.cores' =  4)

#Load Tokenized 30% of the original model (Step 1)
load('~/Data Science/10 Data Science Capstone/Capstone/Proyecto/cleanCorpus30.RData') # load SampleDS as a corpus

# Count n-gram by skipping one word
# Function to take any text taking lower case and counting the number
# of sequence of words in the text by skpping one word a the time
CountNgramSkip <- function(s, n=3, skip=1){
  
  cl <- makeCluster(4)
  
  clusterEvalQ(cl, {
    library('data.table')
    library('parallel')
  })
  env <- new.env()
  assign('n',  n, env)
  assign('skip',  skip, env)
  clusterExport(cl, c('n', 'skip'), envir=env)
  
  res <- unlist(parLapply(cl, s, function(txt){
    txt <- tolower(txt) 
    t.l <- strsplit(txt, ' ')[[1]]
    
    if (length(t.l) < 5) {return(data.table())}
    
    res <- vapply(1:(length(t.l)-(2*n-1)), function(idx){
      paste(t.l[idx+((skip+1)*(1:n)-(skip+1))], collapse=' ')
    }, character(1))
    
    return(res)
  }))
  stopCluster(cl)
  print('End of cluster')
  print(paste('n:', n, 'skip:', skip))  
  res <- data.table(s=res)
  res.DT <- res[, .N, by='s'][order(-N)][N > 1]
  setnames(res.DT, 'N', 'count')
  return(res.DT)
}

#Finds appropiate N-gram
FindNgram <- function(x, n=3){
  content <- iconv(x$content, "utf-8", "ASCII", sub="")
  print('Compute n-gram')
  res.ngram <- CountNgramSkip(content, n, 0)
  if (n==1) return(res.ngram)
  print('Compute skip n-gram')
  res.ngram.skip <- CountNgramSkip(content, n, 1)
  res.ngram.all <- rbind(res.ngram, res.ngram.skip)  
  return(res.ngram.all)
}

# Creates the Ngram table according to the actual step
CreateNGramTable <- function(corp, n, v.size=0){
  res.l <- lapply(corp, FindNgram, n=n)
  
  res.dt <- rbindlist(res.l)[, list(count=sum(count)), by=s]
  res.dt <- res.dt[res.dt$count>2]
  res.dt <- CleanNGram(res.dt)
  res.dt <- res.dt[, list(count=sum(count)), by=s]
  
  if(n>1 & v.size > 0){ 
    res.adj.count <- GoodTuringSmoothing(FreqNGramVector(res.dt, n, v.size),
                                         v.size) # Compute the smoothing count
    setkey(res.dt, count) # Join the two datasets
    setkey(res.adj.count, count)
    res.dt <- res.adj.count[J(res.dt)]
  }
  setkey(res.dt, s)
  return(res.dt)
}

#Cleans the generated N-grams
CleanNGram <- function(n.gram.table){
  ngt <- n.gram.table
  ngt <- ngt[!grepl('- ', ngt[, s]),]
  ngt[, 's':= gsub('^-','', s)]
  return(ngt)
}

# Creates the "frequency of frequencies vector"
FreqNGramVector <- function(n.gram.table, n=1, v.size=9e5){
  res <- n.gram.table[, list(inv.freq=.N), by=count][order(count)]
  res <- rbind(data.table(count=0, inv.freq=v.size^2-sum(res$inv.freq)), res)
  return(res)
}

# Implenetation function of the Good-Turing Smoothing algorithm
# Highlight: When there are no n-gram with r+1, one uses the the alpha smoothing version
GoodTuringSmoothing <- function(DT, v.size, alpha = 0.00017, ngram=3){
  n <- DT[, sum(inv.freq)]
  DT[, adj.count:=0.0]
  DT[1, adj.count:=inv.freq/n]
  for (i in seq_along(DT$count)[-1]){
    if(DT[c(i-1, i), diff(count)!=1]) {
      DT[i, adj.count:=(count+alpha)*n/(n+alpha*v.size^ngram)] # alpha adjustement
    } else {
      DT[i, adj.count:=(DT[i, count])*(DT[i, inv.freq]/DT[i-1, inv.freq])]
    }
  }
  return(DT)
}

# Calculates the Conditional probability of trigram
# Use 3- and 2-grams to compute the conditional probability of a sentence
ConditionalProbNgram <- function(nb, ns, n){
  
  if (n == 1){
    res <-  nb[, list(s=s, count=count, adj.count=count,
                      cond.prob=count/sum(count))]
    return(res)
  }
  
  setkey(ns, s)
  
  nb[, key.b:= vapply(strsplit(nb$s, ' '), function(x){    
    paste0(x[-length(x)], collapse=' ')
  }, character(1))]
  
  setkey(nb, key.b)
  # Join and do the division on the adjusted count
  res <- ns[J(nb)][,list(key.w=s, s=i.s, adj.count=adj.count,
                         weight=i.count/count)]
  setkey(res, s)
  return(res)
}

# Discount Factor computation
ComputeDiscountFactor <- function(DT){
  res <- DT[, list(disc.f= 1 - sum(weight)), by = key.w]
  setnames(res, 'key.w', 's')
  setkey(res, s)
  return(res)
}

#Creates the adjusted prob tables
GenerateAdjustedProbTables <- function(SampleDS){
  
  unigram <- CreateNGramTable(SampleDS, 1L)
  v.size <- nrow(unigram)
  print('End of unigram')
  
  gc()
  trigram <- CreateNGramTable(SampleDS, 3L, v.size)
  print('End of trigram computation.')
  
  bigram <- CreateNGramTable(SampleDS, 2L, v.size)
  print('End of bigram computation.')
  
  bprob <- ConditionalProbNgram(bigram, unigram, 2)
  setkey(bprob, 'key.w')
  
  gc()
  
  tprob <- ConditionalProbNgram(trigram, bigram, 3)
  setkey(tprob, 'key.w')
  
  print('End of conditional probability computation')
  
  prob.adj <- lapply(list(n2=bprob, n3=tprob),
                     function(DT) {
                       res <- DT[(DT[c('', ' '), -.I])]
                       res <- res[!grep('\\d', res$s),]
                       res[, adj.count:=NULL]
                       setkey(res, 'key.w')
                     })
  gc()                     
  
  #Return the calculated tables that will be used for predictions
  return(prob.adj)
}

#Call the main function
prob.adj <- GenerateAdjustedProbTables(SampleDS)
print('I finished to compute the weights. Now saving them.')
#save final N-Gram final Model to be used in the predictions.
save(prob.adj, file='~/Data Science/10 Data Science Capstone/Capstone/Proyecto/Word-O-Matic/ProbNgramModel30.Rdata')