##Test-That 2
##Abby Smith

library(testthat)

test_that("invalid first argument for length", {
  #note we did not need to set the seed here
  expect_error(length(rnorm(-4))) #requires non-negative. should return an error
  expect_error(length(rnorm(NA))) #should return an error
  expect_warning(expect_error(length(rnorm("random characters")))) #shows an error AND warning
})

test_that("allows for there be a 0 length vectr", {
  expect_equal(length(rnorm(0)), 0) 
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

test_that("standard deviation of output=std", {
  expect_equal({set.seed(500)
    sd(rnorm(200))}, 1, tol=.05)
  expect_warning({set.seed(500)
    rnorm(40,0, -4)}) #NAs produced when std deviation is below 0
  expect_warning(expect_equal(length(rnorm(30, 2, NA)), 30)) #expect a warning but also recognizing that it will generate list of NAS, length=n
})

test_that("t distribution does not meet tests", {
  #shapiro test: if p value is less than alpha, we can reject the null hypothesis and conclude that 
  #there is evidence that the data is NOT from a normally distributed population
  expect_gt(shapiro.test(rnorm(30))$p.value, 0.05)
  expect_lt(shapiro.test(rt(3000,10))$p.value, 0.05) #increasing degrees of freedom -> closer to normal
})

