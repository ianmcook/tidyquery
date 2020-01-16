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

replace_star_with_cols <- function(exprs, cols) {
  is_distinct <- isTRUE(attr(exprs, "distinct"))
  is_star <- vapply(exprs, deparse, "") == "dplyr::everything()"
  column_indices <- seq_len(length(exprs))
  if (any(is_star)) {
    for (star in which(is_star)) {
      exprs <- c(
        exprs[column_indices[column_indices < star]],
        lapply(cols, as.symbol),
        exprs[column_indices[column_indices > star]]
      )
    }
  }
  if (is_distinct) {
    attr(exprs, "distinct") <- TRUE
  }
  exprs
}

replace_alias_with_value <- function(expr, alias, value) {
  if (length(expr) == 1) {
    if (is.symbol(expr) && expr == alias) {
      expr <- value
    }
    return(expr)
  } else {
    return(as.call(lapply(expr, replace_alias_with_value, alias, value)))
  }
}

replace_aliases_with_values <- function(exprs, aliases, values) {
  lapply(exprs, function(expr) {
    for (i in seq_along(aliases)) {
      expr <- replace_alias_with_value(expr, aliases[i], values[[i]])
    }
    expr
  })
}

replace_empty_name_with_value <- function(name_, value) {
  if (!is.null(name_) && name_ != "") {
    as.name(name_)
  } else {
    value
  }
}

replace_empty_names_with_values <- function(names_, values) {
  if (is.null(names_)) {
    names_ <- rep("", length(values))
  }
  mapply(
    replace_empty_name_with_value,
    name_ = names_,
    value = values,
    USE.NAMES = FALSE
  )
}
