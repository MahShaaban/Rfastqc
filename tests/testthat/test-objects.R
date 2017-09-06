context("objects")

test_that('FastQC', {
    fl <- system.file("extdata", "sample_1.txt", package = "Rfastqc")
    fqc <- parse_fqc(fl)
    fqc <- FastQC(fqc)
    expect_identical(class(fqc), 'FastQC')
    expect_equal(length(fqc), 10)
    expect_true(all(sapply(fqc, class) == 'data.frame'))
})

test_that('multiFastQC', {
    path <- system.file('extdata', package = 'Rfastqc')
    fls <- list.files(path, pattern = '.txt', full.names = TRUE)
    fqc <- parse_collection(fls)
    fqc <- multiFastQC(fqc)
    expect_identical(class(fqc), 'multiFastQC')
    expect_equal(length(fqc), 10)
    expect_true(all(sapply(fqc, class) == 'data.frame'))
})
