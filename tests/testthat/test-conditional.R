test_that("if() function example query #1 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT shop, game, if(price IS NULL, 8.99, price) AS correct_price FROM inventory"),
    inventory %>% transmute(shop, game, correct_price = ifelse(is.na(price), 8.99, price))
  )
})

test_that("if() function example query #2 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT shop, game, price,
              if(price > 10, 'high price', 'low or missing price') AS price_category
            FROM inventory;"),
    inventory %>%
      transmute(
        shop, game, price,
        price_category = ifelse(price > 10, "high price", "low or missing price")
      )
  )
})
