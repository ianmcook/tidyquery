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

test_that("Join fails on misqualified column reference example #1", {
  expect_error(
    query("SELECT concat_ws(' ', 'Now boarding', a.name, 'flight', CAST(a.flight AS string)) FROM flights f JOIN airlines a USING (carrier)"),
    "a.flight"
  )
})

test_that("Join fails on misqualified column reference example #2", {
  expect_error(
    query("SELECT concat_ws(' ', 'Now boarding', airlines.name, 'flight', CAST(airlines.flight AS string)) FROM flights f JOIN airlines a USING (carrier)"),
    "airlines.flight"
  )
})

test_that("Join fails on ambiguous column reference example #1", {
  expect_error(
    query("SELECT name FROM toys t JOIN makers m ON toys.maker_id = makers.id"),
    "name"
  )
})
