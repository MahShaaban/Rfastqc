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

test_that('plot.FastQC', {
  fqc <- parse_fqc('sample_1.txt')
  fqc <- FastQC(fqc)
  nms <- names(fqc)
  expect_error(plot(fqc), 'User Should provied a test name.')
  expect_error(plot(fqc, test = nms[1]), 'Nothing to plot. Use test_get instead.')
  expect_error(plot(fqc, test = nms[10]), 'Nothing to plot. Use summary instead.')
  expect_silent(plot(fqc, test = nms[2]))
})

test_that('plot.multiFastQC', {
  fls <- list.files('.', pattern = '*.txt', full.names = TRUE)
  fqc <- parse_collection(fls)
  fqc <- multiFastQC(fqc)
  nms <- names(fqc)
  expect_error(plot(fqc), 'User should provide a test name.')
  expect_silent(plot(fqc, test = nms[2]))
})


# clean
unlink('*.pdf')
