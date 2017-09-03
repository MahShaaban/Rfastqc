context("objects")

test_that('FastQC', {
  fqc <- parse_fqc('sample_1.txt')
  fqc <- FastQC(fqc)
  expect_identical(class(fqc), 'FastQC')
  expect_equal(length(fqc), 10)
  expect_true(all(sapply(fqc, class) == 'data.frame'))
})

test_that('multiFastQC', {
  fls <- list.files('.', pattern = '.txt', full.names = TRUE)
  fqc <- parse_collection(fls)
  fqc <- multiFastQC(fqc)
  expect_identical(class(fqc), 'multiFastQC')
  expect_equal(length(fqc), 10)
  expect_true(all(sapply(fqc, class) == 'data.frame'))
})
