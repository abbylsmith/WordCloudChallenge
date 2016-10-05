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
