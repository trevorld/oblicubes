test_that("splicing works", {
    expect_equal(splice4(c(1:4, 9:12), c(5:8, 13:16)), 1:16)
    expect_equal(splice4(c(1:4, 13:16), c(5:8, 17:20), c(9:12, 21:24)), 1:24)
})
