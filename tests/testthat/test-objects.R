context("objects")

test_that('FastQC', {
  fqc <- parse_fqc('sample_1.txt')
  fqc <- FastQC(fqc)
  expect_identical(class(fqc), 'FastQC')
  expect_equal(length(fqc), 10)
  expect_true(all(sapply(fqc, class) == 'data.frame'))
})
