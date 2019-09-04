test_that("ORDER BY clause example query #1 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM games ORDER BY id;"),
    games %>% arrange(id)
  )
})

test_that("ORDER BY clause example query #2 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    games %>% query("SELECT * ORDER BY max_players, list_price;"),
    games %>% arrange(max_players, list_price)
  )
})

test_that("ORDER BY clause example query #3 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT shop, SUM(qty)
             FROM inventory
             WHERE price IS NOT NULL
             GROUP BY shop ORDER BY shop;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    inventory %>%
      filter(!is.na(price)) %>%
      group_by(shop) %>%
      summarise(sum(qty, na.rm = TRUE)) %>%
      arrange(shop) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("ORDER BY clause example query #4 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM games ORDER BY list_price"),
    games %>% arrange(list_price)
  )
})

test_that("ORDER BY clause example query #5 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM games ORDER BY list_price DESC"),
    games %>% arrange(desc(list_price))
  )
})

test_that("ORDER BY clause example query #6 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM games ORDER BY list_price ASC"),
    games %>% arrange(list_price)
  )
})

test_that("ORDER BY clause example query #7 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT name, max_players, list_price FROM games ORDER BY max_players DESC, list_price ASC;"),
    games %>%
      arrange(desc(max_players), list_price) %>%
      select(name, max_players, list_price)
  )
})

test_that("ORDER BY clause example query #8 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    query("SELECT *
             FROM crayons
             ORDER BY (greatest(red, green, blue) - least(red, green, blue)) /
               greatest(red, green, blue) DESC;"),
    crayons %>%
      arrange(
        desc((pmax(red, green, blue) - pmin(red, green, blue)) /
               pmin(red, green, blue))
      )
  )
})

test_that("ORDER BY clause example query #9 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    query("SELECT (greatest(red, green, blue) - least(red, green, blue)) /
            greatest(red, green, blue)
          FROM crayons
          ORDER BY (greatest(red, green, blue) - least(red, green, blue)) /
            greatest(red, green, blue) DESC;
          ") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    crayons %>%
      arrange(
        desc((pmax(red, green, blue) - pmin(red, green, blue)) / pmax(red, green, blue))
      ) %>%
      transmute(
        (pmax(red, green, blue) - pmin(red, green, blue)) / pmax(red, green, blue)
      ) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("ORDER BY clause example query #10 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    query("SELECT *, (greatest(red, green, blue) - least(red, green, blue)) /
               greatest(red, green, blue) AS saturation
             FROM crayons
             ORDER BY saturation DESC;"),
    crayons %>%
      mutate(
        saturation = (pmax(red, green, blue) - pmin(red, green, blue)) /
               pmax(red, green, blue)
      ) %>%
      arrange(desc(saturation))
  )
})
