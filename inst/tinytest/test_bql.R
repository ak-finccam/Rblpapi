
# Copyright (C) 2025  Dirk Eddelbuettel, Whit Armstrong and John Laing
#
# This file is part of Rblpapi.
#
# Rblpapi is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Rblpapi is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Rblpapi.  If not, see <http://www.gnu.org/licenses/>.

library(tinytest)

if (!requireNamespace("jsonlite", quietly=TRUE)) exit_file("Skipping as 'jsonlite' is missing")

library(Rblpapi)

.readFixture <- function(file) {
    paste(readLines(file.path("bql", file), warn=FALSE), collapse="\n")
}

## -- offline parsing tests (no Bloomberg connection required) --------------

## single-item query: one data.frame with declared column types
res <- Rblpapi:::.bqlParse(.readFixture("response_px_last.json"))
expect_true(inherits(res, "data.frame"), info = "single item simplifies to data.frame")
expect_equal(dim(res), c(3L, 4L), info = "three rows, four columns")
expect_equal(colnames(res), c("ID", "px_last", "DATE", "CURRENCY"), info = "column names")
expect_equal(unname(sapply(res, class)), c("character", "numeric", "Date", "character"),
             info = "column types follow declared JSON types")
expect_equal(res$px_last[1:2], c(229.33, 254.49), info = "numeric values")
expect_true(is.na(res$px_last[3]), info = "string 'NaN' becomes NA")
expect_true(is.na(res$DATE[3]) && is.na(res$CURRENCY[3]), info = "JSON null becomes NA")
expect_equal(res$DATE[1], as.Date("2024-12-17"), info = "DATE conversion")

## simplify=FALSE keeps the list shape
res <- Rblpapi:::.bqlParse(.readFixture("response_px_last.json"), simplify=FALSE)
expect_true(is.list(res) && length(res) == 1L && names(res) == "px_last",
            info = "simplify=FALSE returns named list")

## multi-item query: one data.frame per 'get' item
res <- Rblpapi:::.bqlParse(.readFixture("response_multi_item.json"))
expect_true(is.list(res) && !inherits(res, "data.frame"), info = "multi item returns list")
expect_equal(names(res), c("name", "pe_ratio"), info = "list named by data item")
expect_equal(colnames(res$name), c("ID", "name"), info = "no secondary columns")
expect_equal(colnames(res$pe_ratio),
             c("ID", "pe_ratio", "AS_OF_DATE", "PERIOD_END_DATE", "REVISION_COUNT"),
             info = "secondary columns appended")
expect_equal(class(res$pe_ratio$REVISION_COUNT), "integer", info = "INT maps to integer")
expect_equal(res$name$name[2], "Apple Inc", info = "string values")

## a document split across partial responses is row-bound per item
docs <- rep(.readFixture("response_px_last.json"), 2L)
res <- Rblpapi:::.bqlParse(docs)
expect_equal(nrow(res), 6L, info = "partial responses row-bound")

## BQL errors surface as R errors
expect_error(Rblpapi:::.bqlParse(.readFixture("response_syntax_error.json")),
             pattern = "Unable to parse request", info = "responseExceptions raise")

## -- live test (requires a Bloomberg connection) ----------------------------

.runThisTest <- Sys.getenv("RunRblpapiUnitTests") == "yes"
if (!.runThisTest) exit_file("Skipping live BQL test")

res <- bql("get(px_last) for(['IBM US Equity', 'AAPL US Equity'])")
expect_true(inherits(res, "data.frame"), info = "live query returns data.frame")
expect_equal(nrow(res), 2L, info = "one row per security")
expect_true(is.numeric(res$px_last), info = "px_last is numeric")
