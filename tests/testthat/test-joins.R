test_that("Inner join example query #1 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM toys JOIN makers ON toys.maker_id = makers.id"),
    toys %>% inner_join(makers, by = c(maker_id = "id"))
  )
})
