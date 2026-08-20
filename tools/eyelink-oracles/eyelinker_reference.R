#!/usr/bin/env Rscript

# Copyright 2026 canardlapin
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Independent ASC oracle adapter. This process calls the separately maintained
# CRAN eyelinker reader and serializes only that reader's returned tables. It
# neither loads nor reuses the eyes4s parser.

required_version <- "0.2.2"

fail <- function(message) {
  stop(message, call. = FALSE)
}

parse_args <- function(values) {
  if (length(values) %% 2 != 0) {
    fail("arguments must be --name value pairs")
  }
  out <- list()
  if (length(values) > 0) {
    for (index in seq(1, length(values), by = 2)) {
      key <- values[[index]]
      if (!startsWith(key, "--")) fail(paste0("invalid argument name: ", key))
      name <- substring(key, 3)
      if (!is.null(out[[name]])) fail(paste0("duplicate argument: ", key))
      out[[name]] <- values[[index + 1]]
    }
  }
  out
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
required_args <- c(
  "input", "output", "fixture-id", "oracle-id", "tool-digest",
  "adapter-digest", "input-digest"
)
missing_args <- required_args[vapply(required_args, function(name) {
  is.null(args[[name]]) || !nzchar(args[[name]])
}, logical(1))]
if (length(missing_args) > 0) {
  fail(paste0("missing required arguments: ", paste(missing_args, collapse = ",")))
}

validate_id <- function(name, value) {
  if (!grepl("^[a-z0-9][a-z0-9-]*$", value)) {
    fail(paste0(name, " must match [a-z0-9][a-z0-9-]*: ", value))
  }
}

validate_digest <- function(name, value) {
  if (!grepl("^[0-9a-f]{64}$", value)) {
    fail(paste0(name, " must be a lower-case SHA-256 digest: ", value))
  }
}

validate_id("fixture-id", args[["fixture-id"]])
validate_id("oracle-id", args[["oracle-id"]])
for (name in c("tool-digest", "adapter-digest", "input-digest")) {
  validate_digest(name, args[[name]])
}
if (!is.null(args[["converter-receipt-digest"]])) {
  validate_digest("converter-receipt-digest", args[["converter-receipt-digest"]])
}

if (!requireNamespace("eyelinker", quietly = TRUE)) {
  fail(paste0("eyelinker ", required_version, " is not installed in this R library"))
}
actual_version <- as.character(utils::packageVersion("eyelinker"))
if (!identical(actual_version, required_version)) {
  fail(paste0("eyelinker version mismatch: expected=", required_version,
              " actual=", actual_version))
}

encode <- function(value) {
  value <- gsub("%", "%25", value, fixed = TRUE)
  value <- gsub("\t", "%09", value, fixed = TRUE)
  value <- gsub("\r", "%0D", value, fixed = TRUE)
  gsub("\n", "%0A", value, fixed = TRUE)
}

canonical_value <- function(value) {
  if (inherits(value, "POSIXt")) {
    return(format(value, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"))
  }
  if (is.factor(value)) value <- as.character(value)
  if (is.logical(value)) return(ifelse(value, "true", "false"))
  if (is.integer(value)) return(as.character(value))
  if (is.numeric(value)) return(sprintf("%.17g", value))
  as.character(value)
}

field_name <- function(value) {
  normalized <- tolower(gsub("[^A-Za-z0-9_.-]", "-", value))
  if (!grepl("^[a-z]", normalized)) normalized <- paste0("field-", normalized)
  normalized
}

facts <- list()
record_ordinal <- 0L

append_fact <- function(record, field, kind, block, path, presence, value, detail) {
  facts[[length(facts) + 1L]] <<- c(
    as.character(record),
    as.character(field),
    kind,
    if (is.na(block)) "" else as.character(block),
    "",
    path,
    presence,
    value,
    detail
  )
}

append_table <- function(table_name, record_kind, table) {
  if (!is.data.frame(table) || nrow(table) == 0) return(invisible(NULL))
  for (row_index in seq_len(nrow(table))) {
    record_ordinal <<- record_ordinal + 1L
    block <- NA_integer_
    if ("block" %in% names(table) && !is.na(table[["block"]][[row_index]])) {
      block <- as.integer(table[["block"]][[row_index]])
    }
    field_ordinal <- 1L
    append_fact(
      record_ordinal,
      field_ordinal,
      record_kind,
      block,
      "oracle.source-order",
      "omitted",
      "",
      "eyelinker-0.2.2-does-not-retain-cross-table-source-order"
    )
    field_ordinal <- field_ordinal + 1L

    for (column in names(table)) {
      value <- table[[column]][[row_index]]
      path <- paste0("reader.", table_name, ".", field_name(column))
      if (length(value) == 0 || is.na(value)) {
        append_fact(
          record_ordinal,
          field_ordinal,
          record_kind,
          block,
          path,
          "missing",
          "",
          "eyelinker-0.2.2-native-na"
        )
      } else {
        append_fact(
          record_ordinal,
          field_ordinal,
          record_kind,
          block,
          path,
          "value",
          canonical_value(value),
          ""
        )
      }
      field_ordinal <- field_ordinal + 1L
    }

    if (identical(table_name, "msg")) {
      raw_time <- table[["time"]][[row_index]]
      payload <- table[["text"]][[row_index]]
      append_fact(
        record_ordinal,
        field_ordinal,
        record_kind,
        block,
        "message.raw-time",
        if (is.na(raw_time)) "missing" else "value",
        if (is.na(raw_time)) "" else canonical_value(raw_time),
        if (is.na(raw_time)) "eyelinker-0.2.2-native-na" else ""
      )
      field_ordinal <- field_ordinal + 1L
      append_fact(
        record_ordinal,
        field_ordinal,
        record_kind,
        block,
        "message.payload",
        if (is.na(payload)) "missing" else "value",
        if (is.na(payload)) "" else canonical_value(payload),
        if (is.na(payload)) "eyelinker-0.2.2-native-na" else ""
      )
      field_ordinal <- field_ordinal + 1L
      append_fact(
        record_ordinal,
        field_ordinal,
        record_kind,
        block,
        "message.offset",
        "omitted",
        "",
        "eyelinker-0.2.2-does-not-interpret-integration-message-offset"
      )
      field_ordinal <- field_ordinal + 1L
      append_fact(
        record_ordinal,
        field_ordinal,
        record_kind,
        block,
        "message.effective-time",
        "omitted",
        "",
        "eyelinker-0.2.2-does-not-interpret-integration-message-offset"
      )
    }
  }
  invisible(NULL)
}

observed <- eyelinker::read_asc(
  args[["input"]],
  samples = TRUE,
  events = TRUE,
  parse_all = FALSE
)

expected_tables <- c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info")
if (!identical(names(observed), expected_tables)) {
  fail(paste0(
    "eyelinker return schema changed: expected=",
    paste(expected_tables, collapse = ","),
    " actual=", paste(names(observed), collapse = ",")
  ))
}

append_table("raw", "sample", observed$raw)
append_table("sacc", "native-saccade", observed$sacc)
append_table("fix", "native-fixation", observed$fix)
append_table("blinks", "native-blink", observed$blinks)
append_table("msg", "message", observed$msg)
append_table("input", "input", observed$input)
append_table("button", "button", observed$button)
append_table("info", "recording-metadata", observed$info)

if (record_ordinal == 0L) fail("independent reader returned no records")

invocation <- "eyelinker::read_asc(samples=TRUE,events=TRUE,parse_all=FALSE)"
preamble <- c(
  "# eyes4s-eyelink-oracle",
  "1",
  args[["oracle-id"]],
  args[["fixture-id"]],
  "eyelinker",
  "asc",
  "unavailable",
  "CRAN eyelinker",
  required_version,
  args[["tool-digest"]],
  args[["adapter-digest"]],
  args[["input-digest"]],
  invocation,
  if (is.null(args[["converter-receipt-digest"]])) "" else args[["converter-receipt-digest"]]
)
header <- c(
  "record_ordinal", "field_ordinal", "record_kind", "block", "source_order",
  "field_path", "presence", "value", "detail"
)
lines <- c(
  paste(vapply(preamble, encode, character(1)), collapse = "\t"),
  paste(header, collapse = "\t"),
  vapply(facts, function(row) {
    paste(vapply(row, encode, character(1)), collapse = "\t")
  }, character(1))
)
writeLines(lines, con = args[["output"]], sep = "\n", useBytes = TRUE)
