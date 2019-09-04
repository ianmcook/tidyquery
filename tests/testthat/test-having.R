test_that("HAVING clause example query #1 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT shop, SUM(price * qty) FROM inventory GROUP BY shop HAVING SUM(price * qty) > 300;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    inventory %>%
      group_by(shop) %>%
      filter(sum(price * qty, na.rm = TRUE) > 300) %>%
      summarise(sum(price * qty, na.rm = TRUE)) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("HAVING clause example query #2 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT shop, SUM(price * qty)
            FROM inventory
            GROUP BY shop
            HAVING SUM(price * qty) > 300 AND COUNT (*) >= 3") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    inventory %>%
      group_by(shop) %>%
      filter(sum(price * qty, na.rm = TRUE) > 300 & n() >= 3) %>%
      summarise(sum(price * qty, na.rm = TRUE)) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("HAVING clause example query #3 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT shop, COUNT(*) FROM inventory WHERE price < 20 GROUP BY shop HAVING COUNT(*) >= 2;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    inventory %>%
      filter(price < 20) %>%
      group_by(shop) %>%
      filter(n() >= 2) %>%
      summarise(n()) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("HAVING clause example query #4 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT shop FROM inventory GROUP BY shop HAVING SUM(price * qty) > 300;"),
    inventory %>%
      group_by(shop) %>%
      filter(sum(price * qty, na.rm = TRUE) > 300) %>%
      summarise()
  )
})

test_that("HAVING clause example query #5 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT shop, COUNT(*) FROM inventory GROUP BY shop HAVING SUM(price * qty) > 300;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    inventory %>%
      group_by(shop) %>%
      filter(sum(price * qty, na.rm = TRUE) > 300) %>%
      summarise(n()) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("HAVING clause example query #6 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT COUNT(*) FROM inventory GROUP BY shop HAVING SUM(price * qty) > 300;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    inventory %>%
      group_by(shop) %>%
      filter(sum(price * qty, na.rm = TRUE) > 300) %>%
      summarise(n()) %>%
      select(-shop) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("HAVING clause example query #7 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT shop, SUM(price * qty), MIN(price), MAX(price), COUNT(*)
             FROM inventory
             GROUP BY shop
             HAVING SUM(price * qty) > 300;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    inventory %>%
      group_by(shop) %>%
      filter(sum(price * qty, na.rm = TRUE) > 300) %>%
      summarise(
        sum(price * qty, na.rm = TRUE),
        min(price, na.rm = TRUE),
        max(price, na.rm = TRUE),
        n()
      ) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("HAVING clause example query #8 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT shop, SUM(price * qty) AS trv FROM inventory GROUP BY shop HAVING trv > 300;"),
    inventory %>%
      group_by(shop) %>%
      summarise(trv = sum(price * qty, na.rm = TRUE)) %>%
      filter(trv > 300)
  )
})
