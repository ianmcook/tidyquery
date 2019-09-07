
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyquery <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

<!-- badges: end -->

**tidyquery** runs SQL queries on R data frames.

It uses [queryparser](https://github.com/ianmcook/queryparser) to
translate SQL queries into R expressions, then it uses
[dplyr](https://dplyr.tidyverse.org) to evaluate these expressions and
return results. **tidyquery** does not load data frames into a database;
it queries them in place.

## Installation

Install the released version of **tidyquery** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tidyquery")
```

Or install the development version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("ianmcook/tidyquery")
```

## Usage

Call the function `query()`, passing a `SELECT` statement enclosed in
quotes as the first argument. The table name in the `FROM` clause should
match the name of a data frame in your current R session:

``` r
library(tidyquery)
library(nycflights13)

query(
" SELECT origin, dest,
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
)
#> # A tibble: 3 x 5
#>   origin dest  num_flts  dist avg_delay
#>   <chr>  <chr>    <int> <dbl>     <dbl>
#> 1 EWR    BOS       5247   200         5
#> 2 LGA    DCA       4468   214         6
#> 3 JFK    DCA       3076   213         8
```

Alternatively, you can pass a data frame as the first argument and a
`SELECT` statement as the second argument, omitting the `FROM` clause.
This allows `query()` to function like a dplyr verb:

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

airports %>%
  query("SELECT name, lat, lon ORDER BY lat DESC LIMIT 5")
#> # A tibble: 5 x 3
#>   name                                         lat    lon
#>   <chr>                                      <dbl>  <dbl>
#> 1 Dillant Hopkins Airport                     72.3   42.9
#> 2 Wiley Post Will Rogers Mem                  71.3 -157. 
#> 3 Wainwright Airport                          70.6 -160. 
#> 4 Wainwright As                               70.6 -160. 
#> 5 Atqasuk Edward Burnell Sr Memorial Airport  70.5 -157.
```

You can chain dplyr verbs before and after `query()`:

``` r
planes %>%
  filter(engine == "Turbo-fan") %>%
  query("SELECT manufacturer AS maker, COUNT(*) AS num_planes GROUP BY maker") %>%
  arrange(desc(num_planes)) %>%
  head(5)
#> # A tibble: 5 x 2
#>   maker            num_planes
#>   <chr>                 <int>
#> 1 BOEING                 1276
#> 2 BOMBARDIER INC          368
#> 3 AIRBUS                  331
#> 4 EMBRAER                 298
#> 5 AIRBUS INDUSTRIE        270
```

In the `SELECT` statement, the names of data frames and columns are
case-sensitive (like in R) and the names of keywords and function names
are case-insensitive (like in SQL).

It is possible to use `query()` with
[dbplyr](https://dbplyr.tidyverse.org) to query remote database tables
(`tbl_sql` objects), but this depends on which database and which
backend package (if any) you are using, so results may vary.

## Current Limitations

**tidyquery** is subject to the current limitations of the queryparser
package. Please see the **Current Limitations** section of the
queryparser README on
[CRAN](https://cran.r-project.org/package=queryparser/readme/README.html#current-limitations)
or
[GitHub](https://github.com/ianmcook/queryparser#current-limitations).
