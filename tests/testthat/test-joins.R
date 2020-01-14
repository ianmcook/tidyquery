test_that("Inner join example query #1 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM toys JOIN makers ON toys.maker_id = makers.id"),
    toys %>%
      inner_join(makers, by = c(maker_id = "id"), suffix = c(".toys", ".makers")) %>%
      rename(toys.name = "name.toys", makers.name = "name.makers")
  )
})

test_that("Inner join example query #3 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM toys t JOIN makers m ON toys.maker_id = makers.id"),
    toys %>%
      inner_join(makers, by = c(maker_id = "id"), suffix = c(".t", ".m")) %>%
      rename(t.name = "name.t", m.name = "name.m")
  )
})

test_that("Inner join example query #3 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM toys t JOIN makers m ON t.maker_id = m.id"),
    toys %>%
      inner_join(makers, by = c(maker_id = "id"), suffix = c(".t", ".m")) %>%
      rename(t.name = "name.t", m.name = "name.m")
  )
})

test_that("Join fails when column names have suffixes matching table names or aliases", {
  expect_error(
    query("select * FROM iris JOIN iris AS Length ON Species = Species"),
    "Names"
  )
})
