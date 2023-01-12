test_that("Simple SELECT example query #1 returns expected result on tbl_sql", {
  skip_if_not(exists("iris_db"), message = "Test data not loaded")
  expect_equal(
    query("SELECT Species, COUNT(*) AS n FROM iris_db GROUP BY Species") %>%
      collect(),
    iris_db %>%
      group_by(Species) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      collect()
  )
})

test_that("Simple SELECT statement with backticks is unchanged after show_dplyr() then dbplyr::show_query()", {
  skip_if_not(exists("iris_db"), message = "Test data not loaded")
  expect_equal(
    paste(
      capture.output(
        show_query(
          eval(
            str2lang(
              paste0(
                capture.output(
                  show_dplyr("SELECT DISTINCT `Species` FROM `iris_db`")
                ),
                collapse = " "
              )
            )
          )
        )
      )[-1],
      collapse = " "
    ),
    "SELECT DISTINCT `Species` FROM `iris_db`"
  )
})

test_that("Full example #1 with join returns expected result on dtplyr_step", {
  skip("currently fails because dbplyr does not yet accept `multiple` argument to join functions")
  skip_if_not(exists("flights_db") && exists("planes_db"), message = "Test data not loaded")
  expect_equal(
    query(
      "SELECT origin, dest,
          COUNT(flight) AS num_flts,
          round(SUM(seats)) AS num_seats,
          round(AVG(arr_delay)) AS avg_delay
      FROM flights_db f LEFT OUTER JOIN planes_db p
        ON f.tailnum = p.tailnum
      WHERE distance BETWEEN 200 AND 300
      AND air_time IS NOT NULL
      GROUP BY origin, dest
      HAVING num_flts > 3000
      ORDER BY num_seats DESC, avg_delay ASC
      LIMIT 100;"
    ) %>% as.data.frame(),
    flights_db %>%
      left_join(planes_db, by = "tailnum", suffix = c(".f", ".p")) %>%
      rename(f.year = "year.f", p.year = "year.p") %>%
      filter(between(distance, 200, 300) & !is.na(air_time)) %>%
      group_by(origin, dest) %>%
      filter(sum(!is.na(flight), na.rm = TRUE) > 3000) %>%
      summarise(
        num_flts = sum(!is.na(flight), na.rm = TRUE),
        num_seats = round(sum(seats, na.rm = TRUE)),
        avg_delay = round(mean(arr_delay, na.rm = TRUE))
      ) %>%
      ungroup() %>%
      arrange(desc(num_seats), avg_delay) %>%
      head(100L) %>%
      as.data.frame()
  )
})
