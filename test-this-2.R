##Test-That 2
##Abby Smith

library(testthat)

test_that("length of output=n", {
  #note we did not need to set the seed here
  expect_equal(length(rnorm(45, 0,2.5)), 45)
  expect_equal(length(rnorm(0)), 0) #should not return error
  expect_error(length(rnorm(-4))) #should return an error
  expect_error(length(rnorm(NA))) #should return an error
  expect_warning(expect_error(length(rnorm("random characters")))) #shows an error AND warning
})

test_that("mean of output= mean", {
  #when looking at the actual output we need to set the seed
  expect_equal({set.seed(500)
    mean(rnorm(50, 25, 4))}, 25, tol = .05)
  expect_equal({set.seed(500)
    mean(rnorm(50,-4))}, -4, tol = .05)
  expect_equal({set.seed(500)
    mean(rnorm(50))}, 0, tol = .05)
  expect_warning(expect_equal(length(rnorm(30, NA)), 30)) #expect a warning but also recognizing that it will generate list of NAS, length=n
})

test_that("standard deviation of deviation of output=std", {
  expect_equal({set.seed(500)
    sd(rnorm(200))}, 1, tol=.05)
  expect_warning({set.seed(500)
    rnorm(40,0, -4)}) #NAs produced when std deviation is below 0
  expect_warning(expect_equal(length(rnorm(30, 2, NA)), 30)) #expect a warning but also recognizing that it will generate list of NAS, length=n
})