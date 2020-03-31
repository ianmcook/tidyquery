test_that("Aggregate example query #1 variation A returns expected result", {
  skip_if_not(exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT COUNT(*) FROM employees;") %>% pull(1),
    employees %>% summarise(n()) %>% pull(1)
  )
})

test_that("Aggregate example query #1 variation B returns expected result", {
  skip_if_not(exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT SUM(1) FROM employees;") %>% pull(1),
    employees %>% summarise(n()) %>% pull(1)
  )
})

test_that("Aggregate example query #1 variation C returns expected result", {
  skip_if_not(exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT COUNT(1) FROM employees;") %>% pull(1),
    employees %>% summarise(n()) %>% pull(1)
  )
})

test_that("Aggregate example query #2 returns expected result", {
  skip_if_not(exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT COUNT(*) AS num_rows FROM employees;"),
    employees %>% summarise(num_rows = n())
  )
})

test_that("Aggregate example query #3 returns expected result", {
  skip_if_not(exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT SUM(salary) FROM employees;") %>% pull(1),
    employees %>% summarise(sum(salary, na.rm = TRUE)) %>% pull(1)
  )
})

test_that("Aggregate example query #4 returns expected result", {
  skip_if_not(exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT SUM(salary) AS salary_total FROM employees;"),
    employees %>% summarise(salary_total = sum(salary, na.rm = TRUE))
  )
})

test_that("Aggregate example query #5 returns expected result", {
  skip_if_not(exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT MIN(salary) AS lowest_salary, MAX(salary) AS highest_salary FROM employees;"),
    employees %>% summarise(
      lowest_salary = min(salary, na.rm = TRUE),
      highest_salary = max(salary, na.rm = TRUE)
    )
  )
})

test_that("Aggregate example query #5 returns expected result", {
  skip_if_not(exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT MAX(salary) - MIN(salary) AS salary_spread FROM employees;"),
    employees %>% summarise(salary_spread = max(salary, na.rm = TRUE) - min(salary, na.rm = TRUE))
  )
})

test_that("Aggregate example query #6 returns expected result", {
  skip_if_not(exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT round(SUM(salary) * 0.062) AS total_tax FROM employees;"),
    employees %>% summarise(total_tax = round(sum(salary, na.rm = TRUE) * 0.062))
  )
})

test_that("Aggregate example query #7 returns expected result", {
  skip_if_not(exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT SUM(round(salary * 0.062)) AS total_tax FROM employees;"),
    employees %>% summarise(total_tax = sum(round(salary * 0.062), na.rm = TRUE))
  )
})

test_that("Aggregate example query #8 returns expected result", {
  skip_if_not(exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT COUNT(*) FROM employees WHERE salary > 30000;") %>% pull(1),
    employees %>% filter(salary > 30000) %>% summarise(n()) %>% pull(1)
  )
})

test_that("Aggregate example query #9 returns expected result", {
  skip_if_not(exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT COUNT(*) FROM employees GROUP BY office_id;") %>% pull(1),
    employees %>%
      group_by(office_id) %>%
      summarise(n()) %>%
      select(-office_id) %>%
      pull(1)
  )
})

test_that("Aggregate example query #10 returns expected result", {
  skip_if_not(exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT office_id, COUNT(*) FROM employees GROUP BY office_id;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    employees %>%
      group_by(office_id) %>%
      summarise(n()) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("Aggregate example query #11 returns expected result", {
  skip_if_not(exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT office_id, COUNT(*) FROM employees WHERE salary > 35000 GROUP BY office_id;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    employees %>%
      filter(salary > 35000) %>%
      group_by(office_id) %>%
      summarise(n()) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("Aggregate example query #12 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT shop, COUNT(*) FROM inventory GROUP BY shop;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    inventory %>%
      group_by(shop) %>%
      summarise(n()) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("Aggregate example query #13 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT shop, SUM(qty) FROM inventory GROUP BY shop;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    inventory %>%
      group_by(shop) %>%
      summarise(sum(qty, na.rm = TRUE)) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("Aggregate example query #14 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT game, SUM(qty) AS total_qty FROM inventory GROUP BY game"),
    inventory %>% group_by(game) %>% summarise(total_qty = sum(qty, na.rm = TRUE))
  )
})

test_that("Aggregate example query #15 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT list_price < 10, COUNT(*) FROM games GROUP BY list_price < 10;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    games %>%
      group_by(list_price < 10) %>%
      summarise(n()) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("Aggregate example query #16 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT list_price < 10 AS low_price, COUNT(*) FROM games GROUP BY low_price;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    games %>%
      mutate(low_price = list_price < 10) %>%
      group_by(low_price) %>%
      summarise(n()) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("Aggregate example query #17 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT min_age, max_players, COUNT(*) FROM games GROUP BY min_age, max_players;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    games %>%
      group_by(min_age, max_players) %>%
      summarise(n()) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("Aggregate example query #18 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT min_age, list_price < 10, COUNT(*) FROM games GROUP BY min_age, list_price < 10;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    games %>%
      group_by(min_age, list_price < 10) %>%
      summarise(n()) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("Aggregate example query #19 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT min_age, list_price < 10 AS low_price, COUNT(*) FROM games GROUP BY min_age, low_price;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    games %>%
      mutate(low_price = list_price < 10) %>%
      group_by(min_age, low_price) %>%
      summarise(n()) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("Aggregate example query #20 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT list_price > 20 AS over_20, max_players, COUNT(*)
           FROM games
          GROUP BY over_20, max_players") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    games %>%
      mutate(over_20 = list_price > 20) %>%
      group_by(over_20, max_players) %>%
      summarise(n()) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("Aggregate example query #21 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT COUNT(*), MIN(list_price), MAX(list_price) FROM games;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    games %>%
      summarise(n(), min(list_price, na.rm = TRUE), max(list_price, na.rm = TRUE)) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("Aggregate example query #22 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT min_age FROM games GROUP BY min_age;"),
    games %>% group_by(min_age) %>% summarise()
  )
})

test_that("Aggregate example query #23 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT min_age, MAX(list_price) FROM games GROUP BY min_age;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    games %>%
      group_by(min_age) %>%
      summarise(max(list_price, na.rm = TRUE)) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("Aggregate example query #24 returns expected result", {
  skip("currently returns columns in wrong order")
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT min_age, round(AVG(list_price), 2) AS avg_list_price,
              0.21 AS tax_rate, round(AVG(list_price) * 1.21, 2) AS avg_list_price_with_tax
            FROM games
            GROUP BY min_age;"),
    games %>%
      group_by(min_age) %>%
      summarise(
        avg_list_price = round(mean(list_price, na.rm = TRUE), 2),
        tax_rate = 0.21,
        avg_list_price_with_tax = round(mean(list_price, na.rm = TRUE) * 1.21, 2)
      )
  )
})

test_that("Aggregate example query #25 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT COUNT(price) FROM inventory; ") %>% pull(1),
    inventory %>% summarise(sum(!is.na(price))) %>% pull(1)
  )
})

test_that("Aggregate example query #26 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT shop, COUNT(price) FROM inventory GROUP BY shop;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    inventory %>%
      group_by(shop) %>%
      summarise(sum(!is.na(price))) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})

test_that("Aggregate example query #27 returns expected result", {
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query("SELECT COUNT(DISTINCT aisle) FROM inventory;") %>% pull(1),
    inventory %>% summarise(n_distinct(aisle, na.rm = TRUE)) %>% pull(1)
  )
})

test_that("Aggregate example query #28 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    query("SELECT COUNT(DISTINCT red, green, blue) FROM crayons; ") %>% pull(1),
    crayons %>% summarise(n_distinct(red, green, blue, na.rm = TRUE)) %>% pull(1)
  )
})

test_that("Aggregate example query #29 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    query("SELECT COUNT(DISTINCT red), COUNT(DISTINCT green), COUNT(DISTINCT blue) FROM crayons;") %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname(),
    crayons %>%
      summarise(
        n_distinct(red, na.rm = TRUE),
        n_distinct(green, na.rm = TRUE),
        n_distinct(blue, na.rm = TRUE)
      ) %>%
      as.data.frame() %>% # need this because https://github.com/tidyverse/dplyr/issues/4552
      unname()
  )
})
