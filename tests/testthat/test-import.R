library(stringr)
context('test_import')

test_that('parse_test', {
    fl <- system.file("extdata", "sample_1.txt", package = "Rfastqc")
    txt <- readLines(fl)
    tst <- parse_test(txt)
    expect_true(is.data.frame(tst))
    expect_identical(names(tst), c('name', 'output'))
    expect_true(all(duplicated(tst$name)) == FALSE)
    expect_true(all(unique(tst$output) == c('pass', 'fail')))
})

test_that('parse_index', {
    fl <- system.file("extdata", "sample_1.txt", package = "Rfastqc")
    txt <- readLines(fl)
    tst <- parse_test(txt)
    ind <- parse_index(txt, tst)
    expect_true(is.data.frame(ind))
    expect_true(is.numeric(ind$start))
    expect_true(is.numeric(ind$end))
    expect_true(all((ind$end - ind$start) > 1))
})

test_that('parse_data', {
    fl <- system.file("extdata", "sample_1.txt", package = "Rfastqc")
    txt <- readLines(fl)
    tst <- parse_test(txt)
    ind <- parse_index(txt, tst)
    dat <- parse_data(txt, start = ind$start[1], ind$end[1])
    expect_true(is.data.frame(dat))
    expect_identical(names(dat), c('Measure', 'Value'))
    expect_equal(nrow(dat), 7)
})

test_that('parse_fqc', {
    fl <- system.file("extdata", "sample_1.txt", package = "Rfastqc")
    fqc <- parse_fqc(fl)
    expect_true(is.list(fqc))
    expect_true(all(sapply(fqc,function(x) is.data.frame(x))))
})

test_that('parse_collection', {
    path <- system.file('extdata', package = 'Rfastqc')
    fls <- list.files(path, pattern = '.txt', full.names = TRUE)
    coll <- parse_collection(fls)
    expect_true(is.list(coll))
    expect_equal(names(coll), fls)
})
