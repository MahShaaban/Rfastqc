library(stringr)
context('test_import')

test_that('parse_test', {
  txt <- readLines('sample_1.txt')
  tst <- parse_test(txt)
  expect_true(is.data.frame(tst))
  expect_identical(names(tst), c('name', 'output'))
  expect_true(all(duplicated(tst$name)) == FALSE)
  expect_true(all(unique(tst$output) == c('pass', 'fail')))
})

test_that('parse_index', {
  txt <- readLines('sample_1.txt')
  tst <- parse_test(txt)
  ind <- parse_index(txt, tst)
  expect_true(is.data.frame(ind))
  expect_true(is.numeric(ind$start))
  expect_true(is.numeric(ind$end))
  expect_true(all((ind$end - ind$start) > 1))
})

test_that('parse_data', {
  txt <- readLines('sample_1.txt')
  tst <- parse_test(txt)
  ind <- parse_index(txt, tst)
  dat <- parse_data(txt, start = ind$start[1], ind$end[1])
  expect_true(is.data.frame(dat))
  expect_identical(names(dat), c('Measure', 'Value'))
  expect_equal(nrow(dat), 7)
})

test_that('parse_fqc', {
  fqc <- parse_fqc('sample_1.txt')
  expect_true(is.list(fqc))
  expect_true(all(sapply(fqc,function(x) is.data.frame(x))))
})

test_that('parse_collection', {
  fls <- paste('sample_', 1:3, '.txt', sep = '')
  coll <- parse_collection(fls)
  expect_true(is.list(coll))
  expect_equal(names(coll), fls)
})
