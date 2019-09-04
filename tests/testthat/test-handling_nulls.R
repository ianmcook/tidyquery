test_that("Handling NULLs example query #1 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory WHERE price IS NULL;"),
    inventory %>% filter(is.na(price))
  )
})

test_that("Handling NULLs example query #2 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory WHERE price IS NOT NULL;"),
    inventory %>% filter(!is.na(price))
  )
})

test_that("Handling NULLs example query #3 returns expected result", {
  skip_if_not(exists("offices"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM offices WHERE state_province IS DISTINCT FROM 'Illinois'"),
    offices %>% filter(is.na(state_province) | state_province != "Illinois")
  )
})
