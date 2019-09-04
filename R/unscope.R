# Copyright 2019 Cloudera Inc.
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

unscope_expression <- function(expr) {
  if (is.call(expr) && is.call(expr[[1]]) && deparse(expr[[1]][[1]]) %in% "::") {
    # package name is in expr[[1]][[2]]
    expr[[1]] <- expr[[1]][[3]]
  }
  if (length(expr) == 1) {
    return(expr)
  } else {
    return(as.call(lapply(expr, unscope_expression)))
  }
}

unscope_expressions <- function(exprs) {
  if (is.null(exprs)) {
    return(NULL)
  }
  lapply(exprs, unscope_expression)
}

unscope_all_expressions <- function(tree) {
  # capture attributes
  select_distinct_attr <- attr(tree$select, "distinct")
  select_aggregate_attr <- attr(tree$select, "aggregate")

  tree$where <- unscope_expressions(tree$where)
  tree$group_by <- unscope_expressions(tree$group_by)
  tree$having <- unscope_expressions(tree$having)
  tree$select <- unscope_expressions(tree$select)
  tree$order_by <- unscope_expressions(tree$order_by)

  # reassign attributes
  attr(tree$select, "distinct") <- select_distinct_attr
  attr(tree$select, "aggregate") <- select_aggregate_attr

  tree
}
