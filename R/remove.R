# Copyright 2021 Cloudera Inc.
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

remove_desc_from_expression <- function(expr) {
  if (is.call(expr) && deparse(expr[[1]]) %in% c("dplyr::desc", "desc")) {
    expr <- expr[[2]]
  }
  if (length(expr) == 1) {
    return(expr)
  } else {
    return(as.call(lapply(expr, remove_desc_from_expression)))
  }
}

remove_desc_from_expressions <- function(exprs) {
  if (is.null(exprs)) {
    return(NULL)
  }
  lapply(exprs, remove_desc_from_expression)
}
