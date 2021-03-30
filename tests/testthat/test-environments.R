test_that("query() finds data frame in global environment", {
  expect_error(
    {
      query("SELECT Species, COUNT(*) FROM iris WHERE Species = 'setosa' GROUP BY Species")
    },
    NA
  )
})

test_that("query() finds data frame in caller environment and global environment", {
  expect_error(
    {
      foo <- function(dat, sql) {
        query(sql)
      }
      foo(iris, "SELECT Species, COUNT(*) FROM dat WHERE Species = 'setosa' GROUP BY Species")
      foo(iris, "SELECT Species, COUNT(*) FROM iris WHERE Species = 'setosa' GROUP BY Species")
    },
    NA
  )
})

test_that("query() finds data frame in package namespace", {
  skip_if_not("nycflights13" %in% rownames(installed.packages()), message = "nycflights13 package not installed")
  expect_equal(
    query("SELECT COUNT(*) FROM nycflights13::flights"),
    nycflights13::flights %>% summarise(dplyr::n()) %>% ungroup()
  )
})

test_that("query() on bad data frame in namespace gives appropriate error message", {
  expect_error(
    query("SELECT COUNT(*) FROM osidfgsrgsdxzfgsdrg::omdfertgxfgdrg"),
    "No data frame exists with the name osidfgsrgsdxzfgsdrg::omdfertgxfgdrg",
    fixed = TRUE
  )
})
