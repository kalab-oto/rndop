m_num <- ndop_download("mantis religiosa", num_rec_only = T)

test_that("search works", {
  expect_gt(m_num,1)
  expect_gt(ndop_download(family = "Mantidae", num_rec_only = T),1)
  expect_gt(ndop_download(group = "Polyneoptera", num_rec_only = T),1)
})

m <- ndop_download("mantis religiosa", locations = 2)
m_table <- m[[1]]

test_that("download table works", {
    expect_equal(dim(m_table)[2],45)
    expect_gt(dim(m_table)[1],0)
})

test_that("correct number of records in table", {
    expect_equal(dim(m_table)[1],m_num)
})

m_locs <- m[[2]]

test_that("download locations works", {
    expect_equal(length(m_locs), 3)
    expect_equal(class(m_locs[[1]]$geometry)[1], "sfc_POINT")
    expect_equal(class(m_locs[[2]]$geometry)[1], "sfc_MULTIPOLYGON")
    expect_equal(class(m_locs[[3]]$geometry)[1], "sfc_LINESTRING")
})
