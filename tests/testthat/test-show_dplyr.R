test_that("show_dplyr() on simple SELECT example query #1 returns expected result", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    str2lang(paste(trimws(capture.output(
      show_dplyr("SELECT name, list_price FROM games WHERE max_players > 4")
    )), collapse = " ")),
    quote(games %>% filter(max_players > 4) %>% select(name, list_price))
  )
})

test_that("show_dplyr() on SELECT example query #2 returns expected result", {
  skip_if_not(exists("planes"), message = "Test data not loaded")
  expect_equal(
    str2lang(paste(trimws(capture.output(
      show_dplyr(
        " SELECT manufacturer AS maker,
            COUNT(*) AS num_planes
          FROM planes
          WHERE engine = 'Turbo-fan'
          GROUP BY maker;"
      )
    )), collapse = " ")),
    quote(planes %>%
      filter(engine == "Turbo-fan") %>%
      group_by(manufacturer) %>%
      summarise(dplyr::n()) %>%
      ungroup() %>%
      mutate(maker = manufacturer, num_planes = `dplyr::n()`) %>%
      select(maker, num_planes))
  )
})

test_that("show_dplyr() on SELECT example query #3 returns expected result", {
  skip_if_not(exists("planes"), message = "Test data not loaded")
  expect_equal(
    str2lang(paste(trimws(capture.output(
      show_dplyr(
        " SELECT manufacturer,
            COUNT(*) AS num_planes
          FROM planes
          WHERE engine = 'Turbo-fan'
          GROUP BY manufacturer
          ORDER BY num_planes DESC;"
      )
    )), collapse = " ")),
    quote(planes %>%
      filter(engine == "Turbo-fan") %>%
      group_by(manufacturer) %>%
      summarise(num_planes = dplyr::n()) %>%
      ungroup() %>%
      arrange(dplyr::desc(num_planes)))
  )
})

test_that("show_dplyr() truncates function arguments after the fifth argument", {
  skip_if_not(exists("games"), message = "Test data not loaded")
  expect_equal(
    str2lang(paste(trimws(capture.output(
      show_dplyr("SELECT id, name, inventor, year, min_age, min_players, max_players, list_price FROM games")
    )), collapse = " ")),
    quote(games %>% select(id, name, inventor, year, min_age, ...))
  )
})
