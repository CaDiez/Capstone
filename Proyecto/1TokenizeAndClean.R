#Tokenize and clean the source files
library('tm')
library('parallel')
options('mc.cores' =  4)

#Open Sampled Corpus created using de 30% of the original files.
src.dir <- '~/Data Science/10 Data Science Capstone/Dataset/sampledEn_US'  # Directory of the corpus
src <- DirSource(src.dir)
sampleDS <- Corpus(src, readerControl = list(reader = readPlain, language= "en",
                                         load = TRUE)) # A collection of 4 text 

# White space elminiation and lowercase conversion with stopword removal.
RemovePunctuation <- function(plainTxtDoc){
  s <- plainTxtDoc$content
  s <- gsub("(\\w)-(\\w)", "\\1\1dd\\2", s)
  s <- gsub("(\\w)'(\\w)", "\\1\2dd\\2", s)
  s <- gsub("[[:punct:]]+", "", s)
  s <- gsub("\1dd", "-", s, fixed = TRUE)
  s <- gsub("\2dd", "'", s, fixed = TRUE)
  plainTxtDoc$content <- s
  return(plainTxtDoc)
}

CleanText <- function(corp.txt){
  transformations <- getTransformations()
  for (f in transformations){
    if (f == 'removeWords'){
      bw <- scan('~/Data Science/10 Data Science Capstone/Dataset/badWords.txt', character(), sep = '\n')
      corps.txt <- tm_map(corp.txt, get(f), bw)
      next
    }
    
    if (f == 'removePunctuation'){
      corp.txt <- tm_map(corp.txt, function(txt) RemovePunctuation(txt))
      next
    }
    
    if (f == 'stemDocument') next
    
    corps.txt <- tm_map(corp.txt, get(f))
  }
  return(corp.txt)
}

SampleDS <- CleanText(sampleDS)
#Save cleaned and tokenized model, to be used in step 2 (Modelling)
save(SampleDS, file='~/Data Science/10 Data Science Capstone/Capstone/Proyecto/cleanCorpus30.RData')