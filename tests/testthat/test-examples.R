test_that("Full example #1 returns expected result", {
  skip_if_not(exists("flights"), message = "Test data not loaded")
  expect_equal(
    query(
      "SELECT origin, dest,
          COUNT(flight) AS num_flts,
          round(AVG(distance)) AS dist,
          round(AVG(arr_delay)) AS avg_delay
          FROM flights
        WHERE distance BETWEEN 200 AND 300
          AND air_time IS NOT NULL
        GROUP BY origin, dest
        HAVING num_flts > 3000
        ORDER BY num_flts DESC, avg_delay DESC
        LIMIT 100;"
    ),
    flights %>%
      filter(between(distance,200,300) & !is.na(air_time)) %>%
      group_by(origin, dest) %>%
      filter(sum(!is.na(flight)) > 3000) %>%
      summarise(
        num_flts = sum(!is.na(flight)),
        dist = round(mean(distance, na.rm = TRUE)),
        avg_delay = round(mean(arr_delay, na.rm = TRUE))
      ) %>%
      ungroup() %>%
      arrange(desc(num_flts), desc(avg_delay)) %>%
      head(100L)
  )
})

test_that("Full example #2 returns expected result", {
  skip_if_not(exists("airports"), message = "Test data not loaded")
  expect_equal(
    airports %>%
      query("SELECT name, lat, lon ORDER BY lat DESC LIMIT 5"),
    airports %>%
      select(name, lat, lon) %>%
      arrange(desc(lat)) %>%
      head(5)
  )
})

test_that("Full example #3 returns expected result", {
  skip_if_not(exists("flights"), message = "Test data not loaded")
  expect_equal(
    planes %>%
      filter(engine == "Turbo-fan") %>%
      query("SELECT manufacturer AS maker, COUNT(*) AS num_planes GROUP BY maker") %>%
      arrange(desc(num_planes)) %>%
      head(5),
    planes %>%
      filter(engine == "Turbo-fan") %>%
      transmute(maker = manufacturer) %>%
      group_by(maker) %>%
      summarise(num_planes = n()) %>%
      arrange(desc(num_planes)) %>%
      head(5)
  )
})
