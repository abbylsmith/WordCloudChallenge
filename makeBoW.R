library(readr)

tokenize <- function(text){
  if (file.exists(text)) { #check if the file exists in your directory
    text.input <- read_file(text) #reads into a string
  }
  else{
    text.input <- text 
  }
  strsplit(text.input, split = "\\s+")[[1]] #input=string, will return a list. We will split using                                           '\\s+', which allows for multiple spaces
}

##Exercise 2 and unit testing for this

# lexicon_for_grep<-read_file('0068412.txt')
# vector_words<-tokenize(text.example)
# 
# lexicon_for_grep <-'she said jump and her sister said how high'
# vector_words<- c('and', 'flux', 'her', 'high', 'how', 'jump', 'observed', 'orange', 'said', 'she', 'sister')

makeBoW<-function(lexicon_for_grep, vector_words){
  vector_words<-unique(vector_words)
bag_of_words <- sapply(vector_words, function(x) {
  matches <- gregexpr(x, lexicon_for_grep)[[1]]
  if (matches[[1]] == -1) {
    0
  }else {
    length(matches)
    }
  })
  return(bag_of_words)
}


##TEST CASES ### 

library(testthat)

test_that("does not fail when lexicon (text) is missing", {
  expect_error(makeBoW('', vector_words), NA)
})

test_that("does not fail when tokenize output (vector_words) is missing", {
  expect_error(makeBoW(lexicon_for_grep, ''), NA)
})

test_that("counts duplicate words", {
  lexicon_for_grep <- 'she said jump and her sister said how high'
  vector_words <- c('and', 'flux', 'her', 'high', 'how', 'jump', 'observed', 'orange', 'said', 'she', 'sister')
  expect_equal(makeBoW(lexicon_for_grep, vector_words)[[9]], 2) #checking said (appears twice)
})

test_that("returns correct length when there are duplicate words in vector_words (tokenize output)", {
  lexicon_for_grep <- 'she said jump and her sister said how high'
  vector_words <- c('and', 'flux', 'her', 'high', 'how', 'jump', 'flux', 'observed', 'orange', 'said', 'she', 'sister')
  expect_equal(length(makeBoW(lexicon_for_grep, vector_words)), 11)
})

