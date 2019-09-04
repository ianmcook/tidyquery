test_that("SELECT with WHERE example query #1 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT name, list_price FROM games WHERE list_price < 10"),
    games %>% filter(list_price < 10) %>% select(name, list_price)
  )
})

test_that("SELECT with WHERE example query #2 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT name FROM games WHERE inventor = 'Elizabeth Magie';"),
    games %>% filter(inventor == 'Elizabeth Magie') %>% select(name)
  )
})

test_that("SELECT with WHERE example query #3 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    query("SELECT color, red + green + blue AS rgb_sum FROM crayons WHERE red + green + blue > 650;"),
    crayons %>% filter(red + green + blue > 650) %>% transmute(color, rgb_sum = red + green + blue)
  )
})

test_that("SELECT with WHERE example query #4 returns expected result", {
  skip_if_not(exists("crayons"), message = "Test data not loaded")
  expect_equal(
    query("SELECT color, red + green + blue > 650 AS light FROM crayons;"),
    crayons %>% transmute(color, light = red + green + blue > 650)
  )
})

test_that("SELECT with WHERE example query #5 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM games WHERE max_players >= 6 AND list_price <= 10;"),
    games %>% filter(max_players >= 6 & list_price <= 10)
  )
})

test_that("SELECT with WHERE example query #6 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM games WHERE list_price <= 10 OR name = 'Monopoly';"),
    games %>% filter(list_price <= 10 | name == "Monopoly")
  )
})

test_that("SELECT with WHERE example query #7 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM games WHERE NOT name = 'Candy Land' AND NOT name = 'Risk'"),
    games %>% filter(!name == "Candy Land" & !name == "Risk")
  )
})

test_that("SELECT with WHERE example query #8 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM games WHERE name != 'Candy Land' AND name != 'Risk';"),
    games %>% filter(name != "Candy Land" & name != "Risk")
  )
})

test_that("SELECT with WHERE example query #9 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM games WHERE NOT (name = 'Candy Land' OR name = 'Risk')"),
    games %>% filter(!(name == "Candy Land" | name == "Risk"))
  )
})

test_that("SELECT with WHERE example query #10 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM games WHERE (list_price <= 10 OR name = 'Monopoly') AND max_players >= 6;"),
    games %>% filter((list_price <= 10 | name == "Monopoly") & max_players >= 6)
  )
})
