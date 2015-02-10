# Generate fibunocci sequence
fib <- vector(length=100,mode="integer")
fib[1] <-0
fib[2] <- 1
for (i in 3:97) {
  print(i)
  fib[i] <- fib[i-2] + fib[i-1]
}
# Get list of words
con <- url("http://www.cstutoringcenter.com/problems/files/words.txt")
open(con)
words <- scan(con,what=character())
close(con)
#Convert it to a list
wordlist <- strsplit(words,' ')

# Vectors used to translate letters in words to keypresses
pushes <- '12312312312312312341231234'
alpha <- 'abcdefghijklmnopqrstuvwxyz'
#
#Counts pushes for each letter of the word passed
#
countPushes <- function(word){
  tot <- 0
  for(i in 1:nchar(word))
    tot <- sum(tot,as.integer(chartr(alpha,pushes,substr(word,i,i))),na.rm = TRUE)
  return (tot)
}

# This creats a vector of keypushes for each word
pushvec <- lapply(wordlist,countPushes)

#This give counts of words with keypress total that are in the fibonacci sequence.
table(pushvec %in% fib)
