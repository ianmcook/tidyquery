test_that("Simple SELECT example query #1 returns expected result on ArrowTabular", {
  skip_if_not_installed("arrow")
  iris_arrow <- arrow::arrow_table(iris)
  expect_equal(
    query(
      "SELECT Species, COUNT(*) AS n FROM iris_arrow GROUP BY Species"
    ) %>% collect(),
    iris_arrow %>%
      group_by(Species) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      collect()
  )
})

test_that("Full example #1 returns expected result on ArrowTabular", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("nycflights13")
  flights_arrow <- arrow::arrow_table(nycflights13::flights)
  expect_equal(
    suppressWarnings(query(
      "SELECT origin, dest,
          COUNT(flight) AS num_flts,
          round(AVG(distance)) AS dist,
          round(AVG(arr_delay)) AS avg_delay
          FROM flights_arrow
        WHERE distance BETWEEN 200 AND 300
          AND air_time IS NOT NULL
        GROUP BY origin, dest
        HAVING num_flts > 3000
        ORDER BY num_flts DESC, avg_delay DESC
        LIMIT 100;"
    ) %>% collect()),
    suppressWarnings(flights_arrow %>%
      filter(between(distance, 200, 300) & !is.na(air_time)) %>%
      group_by(origin, dest) %>%
      filter(sum(!is.na(flight)) > 3000) %>%
      summarise(
        num_flts = sum(!is.na(flight)),
        dist = round(mean(distance, na.rm = TRUE)),
        avg_delay = round(mean(arr_delay, na.rm = TRUE))
      ) %>%
      ungroup() %>%
      arrange(desc(num_flts), desc(avg_delay)) %>%
      head(100L) %>%
      collect())
  )
})

test_that("Simple SELECT example query #1 returns expected result on arrow Dataset", {
  skip_if_not_installed("arrow")
  skip_if_not(arrow::arrow_info()$capabilities["dataset"], message = "Arrow Datasets not available")

  tmp <- tempfile()
  on.exit(unlink(tmp))
  arrow::write_parquet(iris, tmp)
  iris_arrow <- arrow::open_dataset(tmp)
  expect_equal(
    query(
      "SELECT Species, COUNT(*) AS n FROM iris_arrow GROUP BY Species"
    ) %>% collect(),
    iris_arrow %>%
      group_by(Species) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      collect()
  )
})

test_that("query() fails when input ArrowTabular is grouped", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("nycflights13")
  expect_error(
    nycflights13::flights %>%
      group_by(month) %>%
      arrow::arrow_table() %>%
      query("SELECT COUNT(*)"),
    "grouped"
  )
})

test_that("query() fails when input arrow_dplyr_query is grouped", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("nycflights13")
  expect_error(
    nycflights13::flights %>%
      arrow::arrow_table() %>%
      group_by(month) %>%
      query("SELECT COUNT(*)"),
    "grouped"
  )
})
