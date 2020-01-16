test_that("Simple SELECT example query #1 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM games"),
    games
  )
})

test_that("Simple SELECT example query #1 returns expected result when sql argument is passed by name", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query(sql = "SELECT * FROM games"),
    games
  )
})

test_that("Simple SELECT example query #2 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT name, year, inventor FROM games"),
    games %>% select(name, year, inventor)
  )
})

test_that("Simple SELECT example query #3 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT name, year, inventor AS inventor_name FROM games"),
    games %>% select(name, year, inventor_name = inventor)
  )
})

test_that("Simple SELECT example query #4 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT name, list_price + 5 FROM games"),
    games %>% transmute(name, list_price + 5)
  )
})

test_that("Simple SELECT example query #5 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT name, round(list_price) FROM games;"),
    games %>% transmute(name, round(list_price))
  )
})

test_that("Simple SELECT example query #6 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT -list_price FROM games;"),
    games %>% transmute(-list_price)
  )
})

test_that("Simple SELECT example query #7 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT name, 5, list_price + 5 FROM games"),
    games %>% transmute(name, 5, list_price + 5)
  )
})

test_that("Simple SELECT example query #8 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT name, 5 AS shipping_fee, list_price + 5 AS price_with_shipping FROM games;"),
    games %>% transmute(name, shipping_fee = 5, price_with_shipping = list_price + 5)
  )
})

test_that("Simple SELECT example query #9 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT concat(name, ' is for players age ', min_age, ' or older') FROM games;") %>% pull(1),
    games %>% transmute(paste0(name, ' is for players age ', min_age, ' or older')) %>% pull(1)
  )
})

test_that("Simple SELECT example query #10 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT concat(name, ' is for players age ', cast(min_age AS STRING), ' or older') FROM games;") %>% pull(1),
    games %>% transmute(paste0(name, ' is for players age ', as.character(min_age), ' or older')) %>% pull(1)
  )
})

test_that("Simple SELECT example query #11 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT name AS name_of_game, inventor AS inventor_of_game,
              year AS year_game_invented, min_age AS youngest_player,
              min_players AS fewest_players, max_players AS most_players
            FROM games;"),
    games %>% transmute(name_of_game = name, inventor_of_game = inventor,
                        year_game_invented = year, youngest_player = min_age,
                        fewest_players = min_players, most_players = max_players)
  )
})

test_that("Simple SELECT example query #12 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    query("SELECT red, green, blue FROM crayons;"),
    crayons %>% select(red, green, blue)
  )
})

test_that("Simple SELECT example query #13 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    query("SELECT ALL red, green, blue FROM crayons;"),
    crayons %>% select(red, green, blue)
  )
})

test_that("Simple SELECT example query #14 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    query("SELECT crayons.red, crayons.green, crayons.blue FROM crayons;"),
    crayons %>% select(red, green, blue)
  )
})

test_that("Simple SELECT example query #15 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    query("SELECT c.red, c.green, c.blue FROM crayons c;"),
    crayons %>% select(red, green, blue)
  )
})
