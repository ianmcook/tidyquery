test_that("SELECT DISTINCT example query #1 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT DISTINCT min_age FROM games"),
    games %>% distinct(min_age)
  )
})

test_that("SELECT DISTINCT example query #2 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT DISTINCT min_age, max_players FROM games"),
    games %>% distinct(min_age, max_players)
  )
})

test_that("SELECT DISTINCT example query #3 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT DISTINCT min_age ma, max_players mp FROM games"),
    games %>% distinct(ma = min_age, mp = max_players)
  )
})

test_that("SELECT DISTINCT example query #4 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT DISTINCT concat(substring(year, 1, 3), '0s') FROM games") %>% pull(1),
    games %>% distinct(decade = paste0(substring(year, 1, 3), '0s')) %>% pull(1)
  )
})

test_that("SELECT DISTINCT example query #5 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT DISTINCT concat(substring(year, 1, 3), '0s') AS decade FROM games"),
    games %>% distinct(decade = paste0(substring(year, 1, 3), '0s'))
  )
})
