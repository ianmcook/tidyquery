test_that("query() finds data frame in global environment", {
  expect_error({
    query("SELECT Species, COUNT(*) FROM iris WHERE Species = 'setosa' GROUP BY Species")
  }, NA)
})

test_that("query() finds data frame in caller environment and global environment", {
  expect_error({
    foo <- function(dat, sql) {
      query(sql)
    }
    foo(iris, "SELECT Species, COUNT(*) FROM dat WHERE Species = 'setosa' GROUP BY Species")
    foo(iris, "SELECT Species, COUNT(*) FROM iris WHERE Species = 'setosa' GROUP BY Species")
  }, NA)
})
