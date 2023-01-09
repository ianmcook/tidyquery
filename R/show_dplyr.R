# Copyright 2023 Cloudera Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Show dplyr code equivalent to a SQL query
#'
#' @description \code{show_dplyr} takes a SQL \code{SELECT} statement and prints
#'   equivalent dplyr code
#'
#' @param data a data frame or data frame-like object (optional)
#' @param sql a character string containing a SQL \code{SELECT} statement
#' @details For more details, see \code{\link{query}}. Instead of running the
#'   dplyr code like \code{query} does, \code{show_dplyr} prints the dplyr code.
#'
#'   In function calls in the printed code, long lists of arguments may be
#'   truncated and appended with \code{...} if you have an older version of the
#'   rlang package installed. To fix this, update to a newer version of rlang.
#' @examples
#' library(dplyr)
#' library(nycflights13)
#'
#' query <- "SELECT origin, dest,
#'     COUNT(flight) AS num_flts,
#'     round(AVG(distance)) AS dist,
#'     round(AVG(arr_delay)) AS avg_delay
#'   FROM flights
#'   WHERE distance BETWEEN 200 AND 300
#'     AND air_time IS NOT NULL
#'   GROUP BY origin, dest
#'   HAVING num_flts > 5000
#'   ORDER BY num_flts DESC, avg_delay DESC
#'   LIMIT 100;"
#'
#' show_dplyr(query)
#' @seealso \code{\link{query}}
#' @export
show_dplyr <- function(data, sql) {
  query_(data, sql, FALSE)
}
