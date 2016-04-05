# Quiz 1
library(R.utils)
## Q1. The en_US.blogs.txt file is how many megabytes?
print("Question 1: The en_US.blogs.txt file is how many megabytes?")
file.info("en_US.blogs.txt")$size / 1024**2 # 1024 (to kilobyte) * 1024 (to Megabyte)
## Q2. The en_US.twitter.txt has how many lines of text?
print("Question 2: The en_US.twitter.txt has how many lines of text?")
countLines("en_US.twitter.txt")
## Q3. What is the length of the longest line seen in any of the three en_US data sets?
print("Question 3: What is the length of the longest line seen in any of the three en_US data sets?")
twitter <- readLines("en_US.twitter.txt")
news <- readLines("en_US.twitter.txt")
blogs <- readLines("en_US.blogs.txt")
max_t <- max(nchar(twitter))
max_n <- max(nchar(news))
max_b <- max(nchar(blogs))
max(max_t, max_n, max_b)
## Q4. In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, about what do you get?
print("Question 4: In the en_US twitter data set, if you divide the number of lines where the word 'love'...?")
sum(grepl("love",twitter))/sum(grepl("hate",twitter))
## Q5. The one tweet in the en_US twitter data set that matches the word "biostats" says what?
print("Question 5: The one tweet in the en_US twitter data set that matches the word "biostats" says what?")
twitter[grep("biostats", twitter)]
## Q6. How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing". (I.e. the line matches those characters exactly.)
print("Question 6: How many tweets have the exact characters 'A computer once beat me at chess, but it was...'?")
sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing",twitter))