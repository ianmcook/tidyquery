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

test_that("Full example #4 returns expected result", {
  skip_if(packageVersion("queryparser") < "0.1.1.9002", message = "Test requires queryparser 0.1.1.9002 or higher")
  skip_if_not(exists("inventory"), message = "Test data not loaded")
  expect_equal(
    query(
      "SELECT shop, game, price,
           CASE
             WHEN CAST(price AS double) IS NULL THEN CAST('missing price' AS string)
             WHEN price > 10 THEN 'high price'
             ELSE CAST('low price' AS string)
           END AS price_category
         FROM inventory;"
    ),
    inventory %>%
      transmute(
        shop, game, price,
        price_category = case_when(
          is.na(as.numeric(price)) ~ as.character("missing price"),
          price > 10 ~ "high price",
          TRUE ~ as.character("low price")
        )
      )
  )
})

test_that("Full example #5 returns expected result", {
  skip_if(packageVersion("queryparser") < "0.1.1.9002", message = "Test requires queryparser 0.1.1.9002 or higher")
  skip_if_not(exists("offices"), message = "Test data not loaded")
  expect_equal(
    query(
      "SELECT upper(city) AS city,
           CASE state_province
             WHEN \"Illinois\" THEN \"IL\"
           END AS st,
           upper(country) AS country
         FROM offices
         WHERE country = \"us\""
    ),
    offices %>%
      filter(country == "us") %>%
      transmute(
        city = toupper(city),
        st = case_when(state_province == "Illinois" ~ "IL"),
        country = toupper(country)
      )
  )
})

test_that("Full example #6 returns expected result", {
  skip_if(packageVersion("queryparser") < "0.1.1.9002", message = "Test requires queryparser 0.1.1.9002 or higher")
  skip_if_not(exists("offices"), message = "Test data not loaded")
  patterns <- eval(str2lang(paste0("list(",
    paste0("expr(state_province == '", state.name[state.name %in% offices$state_province], "' ~ '",
           state.abb[state.name %in% offices$state_province], "'", collapse = ", "),
    "))")))
  expect_equal(
    query(
      "SELECT upper(city) AS city,
           CASE state_province
             WHEN \"Alabama\" THEN \"AL\"
             WHEN \"Alaska\" THEN \"AK\"
             WHEN \"Arizona\" THEN \"AZ\"
             WHEN \"Arkansas\" THEN \"AR\"
             WHEN \"California\" THEN \"CA\"
             WHEN \"Colorado\" THEN \"CO\"
             WHEN \"Connecticut\" THEN \"CT\"
             WHEN \"Delaware\" THEN \"DE\"
             WHEN \"Florida\" THEN \"FL\"
             WHEN \"Georgia\" THEN \"GA\"
             WHEN \"Hawaii\" THEN \"HI\"
             WHEN \"Idaho\" THEN \"ID\"
             WHEN \"Illinois\" THEN \"IL\"
             WHEN \"Indiana\" THEN \"IN\"
             WHEN \"Iowa\" THEN \"IA\"
             WHEN \"Kansas\" THEN \"KS\"
             WHEN \"Kentucky\" THEN \"KY\"
             WHEN \"Louisiana\" THEN \"LA\"
             WHEN \"Maine\" THEN \"ME\"
             WHEN \"Maryland\" THEN \"MD\"
             WHEN \"Massachusetts\" THEN \"MA\"
             WHEN \"Michigan\" THEN \"MI\"
             WHEN \"Minnesota\" THEN \"MN\"
             WHEN \"Mississippi\" THEN \"MS\"
             WHEN \"Missouri\" THEN \"MO\"
             WHEN \"Montana\" THEN \"MT\"
             WHEN \"Nebraska\" THEN \"NE\"
             WHEN \"Nevada\" THEN \"NV\"
             WHEN \"New Hampshire\" THEN \"NH\"
             WHEN \"New Jersey\" THEN \"NJ\"
             WHEN \"New Mexico\" THEN \"NM\"
             WHEN \"New York\" THEN \"NY\"
             WHEN \"North Carolina\" THEN \"NC\"
             WHEN \"North Dakota\" THEN \"ND\"
             WHEN \"Ohio\" THEN \"OH\"
             WHEN \"Oklahoma\" THEN \"OK\"
             WHEN \"Oregon\" THEN \"OR\"
             WHEN \"Pennsylvania\" THEN \"PA\"
             WHEN \"Rhode Island\" THEN \"RI\"
             WHEN \"South Carolina\" THEN \"SC\"
             WHEN \"South Dakota\" THEN \"SD\"
             WHEN \"Tennessee\" THEN \"TN\"
             WHEN \"Texas\" THEN \"TX\"
             WHEN \"Utah\" THEN \"UT\"
             WHEN \"Vermont\" THEN \"VT\"
             WHEN \"Virginia\" THEN \"VA\"
             WHEN \"Washington\" THEN \"WA\"
             WHEN \"West Virginia\" THEN \"WV\"
             WHEN \"Wisconsin\" THEN \"WI\"
             WHEN \"Wyoming\" THEN \"WY\"
           END AS st,
           upper(country) AS country
         FROM offices
         WHERE country = \"us\""
    ),
    offices %>%
      filter(country == "us") %>%
      transmute(
        city = toupper(city),
        st = case_when(!!!patterns),
        country = toupper(country)
      )
  )
})
