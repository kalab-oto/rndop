test_that("search works", {
  expect_gt(ndop_download("mantis religiosa", num_rec_only = T),1)
  expect_gt(ndop_download(family = "Mantidae", num_rec_only = T),1)
  expect_gt(ndop_download(group = "Polyneoptera", num_rec_only = T),1)
})
