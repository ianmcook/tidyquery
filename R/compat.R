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

# for compatibility with R versions earlier than 3.6.0
if (!exists("str2lang")) {
  str2lang <- function(s) {
    parse(text = s, keep.source = FALSE)[[1]]
  }
}

# to avoid problems with expressions longer than about 60 characters
deparse <- function(expr, width.cutoff = 500, ...) {
  paste0(trimws(base::deparse(expr, width.cutoff, ...)), collapse = " ")
}

# to support data frame-like objects
is_supported_data_object <- function(obj) {
  inherits(
    obj,
    c(
      "data.frame",
      "tbl",
      "dtplyr_step",
      "disk.frame",
      "arrow_dplyr_query",
      "ArrowTabular"
    )) ||
    all(inherits(obj, c("Dataset", "ArrowObject"), which = TRUE) != 0L)
}
#' @importFrom dplyr group_vars
is_grouped_data_object <- function(obj) {
  inherits(obj, "grouped_disk.frame") || length(group_vars(obj))
}
data_object_uses_function_translations <- function(obj) {
  inherits(obj, c("tbl_sql", "dtplyr_step", "disk.frame"))
}
column_names <- function(obj) {
  if (inherits(obj, "dtplyr_step")) {
    obj$vars
  } else if (inherits(obj, "tbl")) {
    colnames(obj)
  } else {
    names(obj)
  }
}

# Export version number for use by queryparser
#' @export
#' @importFrom utils packageVersion
.tidyquery.version <- packageVersion("tidyquery")

# Use join_by() syntax for join conditions?
# This is introduced in dplyr 1.1.0, but as of early January 2023 it is not yet
# supported by some backends, so we default to not using it for compatibility.
.use_join_by <- function() getOption("tidyquery.use_join_by", default = FALSE)

# Use cross_join() function for cross joins?
# This is introduced in dplyr 1.1.0. Earlier versions of dplyr used the syntax
# full_join(..., by = character()) for cross joins.
#' @importFrom utils getFromNamespace
.use_cross_join <-  tryCatch({
  getFromNamespace("cross_join", "dplyr")
  TRUE
}, error = function(...) { FALSE })

# Temporary fix for https://github.com/tidyverse/dtplyr/issues/184
#' @export
.datatable.aware <- TRUE
