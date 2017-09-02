library(dplyr)
context("methods")

test_that('test_get', {
  fqc <- parse_fqc('sample_1.txt')
  fqc <- FastQC(fqc)

  expect_error(test_get(fqc))

  tst_dat <- test_get(fqc, test = 'Per base sequence quality')

  expect_true(is.factor(tst_dat$Base))
  expect_true(all(apply(tst_dat[, -1], 2, is.numeric)))
})
