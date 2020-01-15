test_that("Inner join example query #1 returns expected result", {
  skip_if_not(exists("toys") && exists("makers"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM toys JOIN makers ON toys.maker_id = makers.id"),
    toys %>%
      inner_join(makers, by = c(maker_id = "id"), suffix = c(".toys", ".makers")) %>%
      rename(toys.name = "name.toys", makers.name = "name.makers")
  )
})

test_that("Inner join example query #3 returns expected result", {
  skip_if_not(exists("toys") && exists("makers"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM toys t JOIN makers m ON toys.maker_id = makers.id"),
    toys %>%
      inner_join(makers, by = c(maker_id = "id"), suffix = c(".t", ".m")) %>%
      rename(t.name = "name.t", m.name = "name.m")
  )
})

test_that("Inner join example query #3 returns expected result", {
  skip_if_not(exists("toys") && exists("makers"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM toys t JOIN makers m ON t.maker_id = m.id"),
    toys %>%
      inner_join(makers, by = c(maker_id = "id"), suffix = c(".t", ".m")) %>%
      rename(t.name = "name.t", m.name = "name.m")
  )
})

test_that("Inner join example query #4 returns expected result", {
  skip_if_not(exists("flights") && exists("airlines"), message = "Test data not loaded")
  expect_equal(
    query("SELECT concat_ws(' ', 'Now boarding', name, 'flight', CAST(flight AS string))
             FROM flights f JOIN airlines a USING (carrier)"),
    flights %>%
      inner_join(airlines, by = "carrier") %>%
      transmute(stringr::str_c('Now boarding', name, 'flight', as.character(flight), sep = " "))
  )
})

test_that("Inner join example query #5 returns expected result", {
  skip_if_not(exists("flights") && exists("airlines"), message = "Test data not loaded")
  expect_equal(
    query("SELECT concat_ws(' ', 'Now boarding', airlines.name, 'flight', CAST(flight AS string))
             FROM flights f JOIN airlines a USING (carrier)"),
    flights %>%
      inner_join(airlines, by = "carrier") %>%
      transmute(stringr::str_c('Now boarding', name, 'flight', as.character(flight), sep = " "))
  )
})

test_that("Inner join example query #6 returns expected result", {
  skip_if_not(exists("flights") && exists("airlines"), message = "Test data not loaded")
  expect_equal(
    query("SELECT concat_ws(' ', 'Now boarding', a.name, 'flight', CAST(f.flight AS string))
             FROM flights f JOIN airlines a USING (carrier)"),
    flights %>%
      inner_join(airlines, by = "carrier") %>%
      transmute(stringr::str_c('Now boarding', name, 'flight', as.character(flight), sep = " "))
  )
})

test_that("Inner join example query #7 returns expected result", {
  skip_if_not(exists("flights") && exists("airlines"), message = "Test data not loaded")
  expect_equal(
    query("SELECT concat_ws(' ', 'Now boarding', airlines.name, 'flight', CAST(flights.flight AS string))
             FROM flights f JOIN airlines a USING (carrier)"),
    flights %>%
      inner_join(airlines, by = "carrier") %>%
      transmute(stringr::str_c('Now boarding', name, 'flight', as.character(flight), sep = " "))
  )
})

test_that("Join fails on misqualified column reference example #1", {
  skip_if_not(exists("flights") && exists("airlines"), message = "Test data not loaded")
  expect_error(
    query("SELECT concat_ws(' ', 'Now boarding', a.name, 'flight', CAST(a.flight AS string)) FROM flights f JOIN airlines a USING (carrier)"),
    "a.flight"
  )
})

test_that("Join fails when column names have suffixes matching table names or aliases", {
  expect_error(
    query("select * FROM iris JOIN iris AS Length ON Species = Species"),
    "Names"
  )
})

test_that("Join fails on misqualified column reference example #2", {
  skip_if_not(exists("flights") && exists("airlines"), message = "Test data not loaded")
  expect_error(
    query("SELECT concat_ws(' ', 'Now boarding', airlines.name, 'flight', CAST(airlines.flight AS string)) FROM flights f JOIN airlines a USING (carrier)"),
    "airlines.flight"
  )
})

test_that("Join fails on ambiguous column reference example #1", {
  skip_if_not(exists("toys") && exists("makers"), message = "Test data not loaded")
  expect_error(
    query("SELECT name FROM toys t JOIN makers m ON toys.maker_id = makers.id"),
    "name"
  )
})

test_that("Join alias/conditions example #1 variant A returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory JOIN games ON game = name"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant B returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON game = name"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant C returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory JOIN games ON name = game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant D returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON name = game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant E returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory JOIN games ON inventory.game = games.name"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})


test_that("Join alias/conditions example #1 variant F returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory JOIN games ON games.name = inventory.game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant G returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON i.game = g.name"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant H returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON g.name = i.game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant I returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON inventory.game = g.name"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant J returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON g.name = inventory.game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant K returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON i.game = games.name"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant L returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON games.name = i.game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant M returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games ON games.name = i.game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant N returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games ON name = i.game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant O returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games ON i.game = name"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant P returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory JOIN games g ON g.name = inventory.game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant Q returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    {
      games_with_col_renamed <<- games %>% rename(game = name)
      query("SELECT * FROM inventory JOIN games_with_col_renamed USING (game)")
    },
    inventory %>% inner_join(games %>% rename(game = name), by = c("game"))
  )
})

test_that("Join alias/conditions example #1 variant R returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    {
      games_with_col_renamed <<- games %>% rename(game = name)
      query("SELECT * FROM inventory i JOIN games_with_col_renamed g USING (game)")
    },
    inventory %>% inner_join(games %>% rename(game = name), by = c("game"))
  )
})

test_that("Inner join with two join conditions example query #1 returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory INNER JOIN games ON inventory.game = games.name AND inventory.price = games.list_price"),
    inventory %>% inner_join(games, by = c(game = "name", price = "list_price"))
  )
})

test_that("Inner join with two join conditions example query #2 returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i INNER JOIN games g ON i.game = g.name AND i.price = g.list_price"),
    inventory %>% inner_join(games, by = c(game = "name", price = "list_price"))
  )
})
