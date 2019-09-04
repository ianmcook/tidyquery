test_that("SELECT with IN example query returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM games WHERE name IN ('Monopoly','Clue','Risk');"),
    games %>% filter(name %in% c("Monopoly","Clue","Risk"))
  )
})

test_that("SELECT with BETWEEN example query returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM games WHERE min_age BETWEEN 8 AND 10;"),
    games %>% filter(between(min_age, 8, 10))
  )
})
