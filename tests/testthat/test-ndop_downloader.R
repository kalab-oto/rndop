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

test_that("correct number of records in tables", {
    expect_equal(dim(m_table)[1],m_num)
    expect_equal(dim(ndop_download("Lutra lutra"))[1],ndop_download("Lutra lutra", num_rec_only = T))
})

m_locs <- m[[2]]

test_that("download locations works", {
    expect_equal(length(m_locs), 3)
    expect_equal(class(m_locs[[1]]$geometry)[1], "sfc_POINT")
    expect_equal(class(m_locs[[2]]$geometry)[1], "sfc_MULTIPOLYGON")
    expect_equal(class(m_locs[[3]]$geometry)[1], "sfc_LINESTRING")
})

test_that("multiple species works", {
    expect_gt(ndop_download(c("Mycetobia pallipes", "Tipula subcunctans"), num_rec_only = T), 1)
})

test_that("species not found works", {
    expect_equal(is.na(ndop_download("no species", num_rec_only = T)),T)
    expect_gt(ndop_download(c("Mycetobia pallipes", "Tipula subcunctans", "Foo bar"), num_rec_only = T), 1)
    expect_equal(dim(ndop_download(c("Mycetobia pallipes", "Tipula subcunctans", "Foo bar")))[2], 45)
})

 
test_that("families not found works", {
    expect_equal(is.na(ndop_download(family = "no family", num_rec_only = T)),T)
    expect_gt(ndop_download(family = c("Foo bar", "Mantidae"), num_rec_only = T), 1)
})


test_that("groups not found works", {
    expect_equal(is.na(ndop_download(group = "no group", num_rec_only = T)),T)
    expect_gt(ndop_download(group = c("Foo bar", "Polyneoptera"), num_rec_only = T), 1)
})

pld <- set_search_payload(rfKvadrat = 6175, rfTaxon = c("Mycetobia pallipes", "mantis religiosa"))
pld_0 <- ndop_download(search_payload = pld)

pld_num <- set_search_payload(rfKvadrat = 6175, rfTaxon = "mantis religiosa")
pld_1 <- ndop_download(search_payload = pld_num, num_rec_only = T)

test_that("query multiple species while one of them have 0 records", {
    expect_equal(dim(pld_0),pld_1)
})
