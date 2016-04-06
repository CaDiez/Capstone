#Tokenize and clean the source files
library('tm')
library('parallel')
options('mc.cores' =  4)

src.dir <- '~/Data Science/10 Data Science Capstone/Dataset/en_US'  # Directory of the corpus
src <- DirSource(src.dir)
sampleDS <- Corpus(src, readerControl = list(reader = readPlain, language= "en",
                                         load = TRUE)) # A collection of 4 text 

length(sampleDS)

### tm_Map(x, fn) apply f to all element of x
### fn can be  removeNumbers, removePunctuation, removeWords, stemDocument,
### stripWhitespace

### tmFilter and tmIndex for filtering.
### searchFullText function look for regular expresions inside the text
### doclevel = T, for usage to document level.

### We will perform white space elminiation and lowercase conversion with stopword removal.

### Tockenzation will be perform by the tm::TermDocumentMatrix function.
### /usr/share/dict/words for the dictionnary
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
#save environment variables
save(SampleDS, file='~/Data Science/10 Data Science Capstone/Capstone/Proyecto/Otro/cleanCorpus2.RData')