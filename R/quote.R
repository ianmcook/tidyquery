# Copyright 2020 Cloudera Inc.
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

#' @include compat.R
NULL

quote_column_in_expression <- function(expr, column) {
  if (deparse(expr) == column)  {
    expr <- as.symbol(deparse(expr))
  }
  if (length(expr) == 1) {
    return(expr)
  } else {
    return(as.call(lapply(expr, quote_column_in_expression, column)))
  }
}

quote_columns_in_expressions <- function(exprs, columns = NULL) {
  lapply(exprs, function(expr) {
    for (column in columns) {
      expr <- quote_column_in_expression(expr, column)
    }
    expr
  })
}

quote_full_expression <- function(expr) {
  if (is.call(expr)) {
    as.symbol(deparse(expr))
  } else {
    expr
  }
}

quote_full_expressions <- function(exprs) {
  lapply(exprs, quote_full_expression)
}
