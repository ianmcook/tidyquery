test_that("show_dplyr() on simple SELECT example query #1 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    str2lang(paste(trimws(capture.output(
      show_dplyr("SELECT name, list_price FROM games WHERE max_players > 4")
    )), collapse = " ")),
    quote(games %>% filter(max_players > 4) %>% select(name, list_price))
  )
})
