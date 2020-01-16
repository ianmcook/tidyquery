test_that("query() fails when no arguments passed", {
  expect_error(
    query(),
    "argument"
  )
})

test_that("query() fails when arguments are specified backward", {
  expect_error(
    "SELECT Species, COUNT(*) GROUP BY Species" %>% query(iris),
    "argument"
  )
})

test_that("query() fails when passed two queries", {
  expect_error(
    query(c("SELECT year FROM games", "SELECT inventor FROM games")),
    "argument"
  )
})

test_that("query() fails when data frame passed and FROM clause specified", {
  expect_error(
    iris %>% query("SELECT Species, COUNT(*) FROM iris GROUP BY Species"),
    "argument"
  )
})

test_that("query() fails when object named in FROM clause has unsupported type", {
  expect_error(
    query("SELECT * FROM letters"),
    "supported"
  )
})

test_that("query() fails when object in data argument has unsupported type", {
  expect_error(
    letters %>% query("SELECT *"),
    "Unexpected"
  )
})


test_that("query() fails when no data is specified in data argument or in FROM clause", {
  expect_error(
    query("SELECT *"),
    "data"
  )
})

test_that("query() fails when input data frame is grouped", {
  expect_error(
    iris %>% group_by(Species) %>% query("SELECT COUNT(*)"),
    "grouped"
  )
})

test_that("query() fails when same column alias is used more than once", {
  expect_error(
    query("SELECT Sepal.Length as sepal, Sepal.Width as sepal FROM iris"),
    "alias"
  )
})

test_that("query() fails when same column reference is used more than once with no alias", {
  expect_error(
    query("SELECT Sepal.Length, Sepal.Length FROM iris"),
    "identical"
  )
})

test_that("query() fails when a column reference with no alias is identical to an alias", {
  expect_error(
    query("SELECT Sepal.Length as Sepal.Width, Sepal.Width FROM iris"),
    "identical"
  )
})

test_that("query() fails when an expression with no alias is identical to an alias", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT year, 2020 AS year FROM games"),
    "identical"
  )
})

test_that("query() fails with two identical expressions with no aliases", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT upper(name), upper(name) FROM games"),
    "identical"
  )
})

test_that("query() fails on two very long expressions with no aliases", {
  expect_error(
    query("SELECT 1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1, 1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+2 FROM games"),
    "alias"
  )
})
