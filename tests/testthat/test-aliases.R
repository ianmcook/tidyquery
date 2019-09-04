test_that("Alias test #1 returns consistent results", {
  skip_if_not(exists("flights"), message = "Test data not loaded")
  expect_equal(
    query("SELECT carrier AS airline, COUNT(*) AS num_flights FROM flights GROUP BY airline HAVING carrier = 'AS'"),
    query("SELECT carrier AS airline, COUNT(*) AS num_flights FROM flights GROUP BY airline HAVING airline = 'AS'")
  )
})

test_that("Alias test #2 returns consistent results", {
  skip_if_not(exists("flights"), message = "Test data not loaded")
  expect_equal(
    query("SELECT carrier AS airline, COUNT(*) AS num_flights FROM flights GROUP BY carrier HAVING carrier = 'AS'"),
    query("SELECT carrier AS airline, COUNT(*) AS num_flights FROM flights GROUP BY carrier HAVING airline = 'AS'")
  )
})

test_that("Alias test #3 returns consistent results", {
  skip_if_not(exists("flights"), message = "Test data not loaded")
  expect_equal(
    query("SELECT carrier, COUNT(*) AS num_flights FROM flights GROUP BY carrier HAVING COUNT(*) > 50000"),
    query("SELECT carrier, COUNT(*) AS num_flights FROM flights GROUP BY carrier HAVING num_flights > 50000")
  )
})

test_that("Alias test #4 returns consistent results", {
  skip_if_not(exists("flights"), message = "Test data not loaded")
  expect_equal(
    query("SELECT carrier AS airline FROM flights ORDER BY airline LIMIT 10"),
    query("SELECT carrier AS airline FROM flights ORDER BY carrier LIMIT 10")
  )
})

test_that("Alias test #5 returns consistent results", {
  skip_if_not(exists("flights"), message = "Test data not loaded")
  expect_equal(
    query("SELECT DISTINCT carrier AS airline FROM flights ORDER BY airline"),
    query("SELECT DISTINCT carrier AS airline FROM flights ORDER BY carrier")
  )
})

test_that("Alias test #6 returns consistent results", {
  skip_if_not(exists("flights"), message = "Test data not loaded")
  expect_equal(
    query("SELECT carrier AS airline, COUNT(*) AS num_flights FROM flights GROUP BY airline"),
    query("SELECT carrier AS airline, COUNT(*) AS num_flights FROM flights GROUP BY carrier")
  )
})

test_that("Alias test #7 returns consistent results", {
  skip_if_not(exists("flights"), message = "Test data not loaded")
  expect_equal(
    query("SELECT carrier AS airline, COUNT(*) AS num_flights FROM flights GROUP BY airline ORDER BY num_flights DESC"),
    query("SELECT carrier AS airline, COUNT(*) AS num_flights FROM flights GROUP BY airline ORDER BY COUNT(*) DESC")
  )
})

test_that("Alias test #8 returns consistent results", {
  skip_if_not(exists("flights"), message = "Test data not loaded")
  expect_equal(
    query("SELECT carrier AS airline, COUNT(*) AS num_flights FROM flights GROUP BY carrier ORDER BY num_flights DESC"),
    query("SELECT carrier AS airline, COUNT(*) AS num_flights FROM flights GROUP BY carrier ORDER BY COUNT(*) DESC")
  )
})

test_that("Alias test #9 returns consistent results", {
  skip_if_not(exists("flights"), message = "Test data not loaded")
  expect_equal(
    query("SELECT carrier, COUNT(*) AS num_flights FROM flights GROUP BY carrier ORDER BY AVG(arr_delay) DESC"),
    query("SELECT carrier, COUNT(*) AS num_flights FROM flights GROUP BY carrier ORDER BY -AVG(arr_delay)")
  )
})

test_that("Alias test #10 returns consistent results", {
  skip_if_not(exists("flights"), message = "Test data not loaded")
  expect_equal(
    query("SELECT carrier AS c, COUNT(*) AS num_flights FROM flights GROUP BY carrier HAVING AVG(arr_delay) > 15"),
    query("SELECT carrier c, COUNT(*) AS num_flights FROM flights GROUP BY c HAVING AVG(arr_delay) > 15")
  )
})

test_that("Alias test #11 returns consistent results", {
  skip_if_not(exists("flights"), message = "Test data not loaded")
  expect_equal(
    query("SELECT carrier AS airline, COUNT(*) AS num_flights FROM flights GROUP BY airline"),
    query("SELECT carrier AS airline, COUNT(*) AS num_flights FROM flights GROUP BY carrier")
  )
})
