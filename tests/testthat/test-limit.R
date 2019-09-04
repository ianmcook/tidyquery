test_that("LIMIT clause example query #1 returns expected result", {
  skip_if_not(exists("flights"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM flights LIMIT 5;") %>% tally() %>% pull(1),
    5L
  )
})

test_that("LIMIT clause example query #2 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    crayons %>% query("SELECT * LIMIT 1e2;") %>% tally() %>% pull(1),
    100L
  )
})
