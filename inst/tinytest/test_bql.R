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
library(Rblpapi)

## every JSON parser which is installed is tested, and all of them must give
## the very same result
.parsers <- Filter(function(p) requireNamespace(p, quietly=TRUE),
                   Rblpapi:::.bqlParsers)
if (length(.parsers) == 0L)
    exit_file("Skipping as no JSON parser is available")

.readFixture <- function(file) {
    paste(readLines(file.path("bql", file), warn=FALSE), collapse="\n")
}
.parse <- function(file, ...) Rblpapi:::.bqlParse(.readFixture(file), ...)

## -- offline parsing tests (no Bloomberg connection required) --------------

for (.p in .parsers) {

    .with <- function(txt) paste0(txt, " [", .p, "]")

    ## single-item query: one data.frame with declared column types
    res <- .parse("response_px_last.json", parser=.p)
    expect_true(inherits(res, "data.frame"), info = .with("single item simplifies to data.frame"))
    expect_equal(dim(res), c(3L, 4L), info = .with("three rows, four columns"))
    expect_equal(colnames(res), c("ID", "px_last", "DATE", "CURRENCY"), info = .with("column names"))
    expect_equal(unname(sapply(res, class)), c("character", "numeric", "Date", "character"),
                 info = .with("column types follow declared JSON types"))
    expect_equal(res$px_last[1:2], c(229.33, 254.49), info = .with("numeric values"))
    expect_true(is.na(res$px_last[3]), info = .with("string 'NaN' becomes NA"))
    expect_true(is.na(res$DATE[3]) && is.na(res$CURRENCY[3]), info = .with("JSON null becomes NA"))
    expect_equal(res$DATE[1], as.Date("2024-12-17"), info = .with("DATE conversion"))

    ## simplify=FALSE keeps the list shape
    res <- .parse("response_px_last.json", simplify=FALSE, parser=.p)
    expect_true(is.list(res) && length(res) == 1L && names(res) == "px_last",
                info = .with("simplify=FALSE returns named list"))

    ## multi-item query: one data.frame per 'get' item
    res <- .parse("response_multi_item.json", parser=.p)
    expect_true(is.list(res) && !inherits(res, "data.frame"), info = .with("multi item returns list"))
    expect_equal(names(res), c("name", "pe_ratio"), info = .with("list named by data item"))
    expect_equal(colnames(res$name), c("ID", "name"), info = .with("no secondary columns"))
    expect_equal(colnames(res$pe_ratio),
                 c("ID", "pe_ratio", "AS_OF_DATE", "PERIOD_END_DATE", "REVISION_COUNT"),
                 info = .with("secondary columns appended"))
    expect_equal(class(res$pe_ratio$REVISION_COUNT), "integer", info = .with("INT maps to integer"))
    expect_equal(res$name$name[2], "Apple Inc", info = .with("string values"))

    ## responses above 4 MiB arrive as fragments of one document, cut mid-token,
    ## and must give the same result as the document delivered in one message
    doc <- .readFixture("response_px_last.json")
    cut <- nchar(doc) %/% 3L
    fragments <- substring(doc, c(1L, cut + 1L, 2L * cut + 1L), c(cut, 2L * cut, nchar(doc)))
    expect_equal(Rblpapi:::.bqlParse(fragments, parser=.p),
                 Rblpapi:::.bqlParse(doc, parser=.p),
                 info = .with("fragmented response parses like the joined document"))

    ## BQL errors surface as R errors
    expect_error(.parse("response_syntax_error.json", parser=.p),
                 pattern = "Unable to parse request", info = .with("responseExceptions raise"))

    ## literal "NA" strings in STRING columns are preserved, not turned into NA
    res <- .parse("response_string_na.json", parser=.p)
    expect_equal(res$ticker[2], "NA", info = .with("literal 'NA' string value preserved"))
    expect_false(anyNA(res$ticker), info = .with("no spurious NAs in string column"))

    ## item-level responseExceptions surface as warnings, data is kept
    expect_warning(res <- .parse("response_item_error.json", parser=.p),
                   pattern = "Insufficient data", info = .with("item-level exceptions warn"))
    expect_equal(nrow(res), 1L, info = .with("partial data still returned"))

    ## grouped aggregation, e.g. let(#mv=sum(group(amt_outstanding(),
    ## by=[year(maturity()), industry_sector()]));): the ID column holds
    ## composite group labels, year() yields an INT column, and ORIG_IDS is
    ## null for multi-security groups but set for single-security groups
    ## (synthetic values; structure verified against a live response)
    res <- .parse("response_grouped.json", parser=.p)
    expect_equal(dim(res), c(6L, 8L), info = .with("grouped: dimensions"))
    expect_equal(colnames(res),
                 c("ID", "#mv", "CURRENCY_OF_ISSUE", "MULTIPLIER", "CURRENCY",
                   "ORIG_IDS", "YEAR(MATURITY())", "INDUSTRY_SECTOR()"),
                 info = .with("grouped: column names"))
    expect_equal(unname(sapply(res, function(x) class(x)[1])),
                 c("character", "numeric", "character", "numeric", "character",
                   "character", "integer", "character"),
                 info = .with("grouped: column types incl. INT from year()"))
    expect_equal(res$ID[1], "2027.0:Technology", info = .with("grouped: composite group id"))
    expect_equal(res[["#mv"]][1], 1500000000, info = .with("grouped: aggregated value"))
    expect_equal(res[["YEAR(MATURITY())"]][1], 2027L, info = .with("grouped: integer year"))
    expect_true(anyNA(res$ORIG_IDS) && !all(is.na(res$ORIG_IDS)),
                info = .with("grouped: ORIG_IDS null for groups, set for singletons"))
    expect_equal(res$CURRENCY_OF_ISSUE[1], "USD",
                 info = .with("grouped: undeclared types like ENUM fall back to character"))
}

## -- fragmented responses -------------------------------------------------

## The C++ layer returns one string per response message, and the service cuts
## a response above 4 MiB at a byte boundary, in the middle of a token, so the
## fragments form one document only once joined. Chunking a fixture into pieces
## far smaller than 4 MiB reproduces that without needing a 4 MiB response.
##
## The chunking is on bytes, not on characters, because that is what the
## service does: a boundary can fall inside a multi-byte UTF-8 character, which
## leaves that one fragment invalid UTF-8 on its own.
.chunkBytes <- function(txt, n) {
    b <- charToRaw(txt)
    i <- split(seq_along(b), ceiling(seq_along(b) / n))
    vapply(i, function(k) rawToChar(b[k]), character(1), USE.NAMES = FALSE)
}

for (.p in .parsers) {
    for (f in list.files("bql", pattern = "[.]json$")) {
        doc <- .readFixture(f)
        ref <- tryCatch(suppressWarnings(Rblpapi:::.bqlParse(doc, parser = .p)),
                        error = function(e) conditionMessage(e))
        for (n in c(1L, 7L, 64L, 1000L)) {
            frags <- .chunkBytes(doc, n)
            got <- tryCatch(suppressWarnings(Rblpapi:::.bqlParse(frags, parser = .p)),
                            error = function(e) conditionMessage(e))
            expect_equal(got, ref,
                         info = paste0(f, " in ", length(frags), " chunks of ", n,
                                       " bytes [", .p, "]"))
        }
    }

    ## a single fragment is not a document: the failure the joining prevents
    doc <- .readFixture("response_px_last.json")
    frags <- .chunkBytes(doc, nchar(doc, type = "bytes") %/% 2L)
    expect_true(length(frags) > 1L, info = "the fixture really was split")
    expect_error(Rblpapi:::.bqlParse(frags[1], parser = .p),
                 info = paste0("a lone fragment does not parse [", .p, "]"))

    ## a boundary inside a multi-byte UTF-8 character must still rejoin; the
    ## characters are written as escapes so that this file stays ASCII
    utf8doc <- paste0('{"results":{"name":{"name":"name","idColumn":{"name":"ID",',
                      '"type":"STRING","values":["X","Y"]},"valuesColumn":',
                      '{"name":"VALUE","type":"STRING","values":',
                      '["Nestl\u00e9 S\u00e9n\u00e9gal","\u00dcbermorgen"]},',
                      '"secondaryColumns":[]}},"responseExceptions":[]}')
    want <- c("Nestl\u00e9 S\u00e9n\u00e9gal", "\u00dcbermorgen")
    expect_equal(Rblpapi:::.bqlParse(utf8doc, parser = .p)$name, want,
                 info = paste0("multi-byte characters read correctly [", .p, "]"))
    for (n in 1L:8L) {
        frags <- .chunkBytes(utf8doc, n)
        expect_equal(Rblpapi:::.bqlParse(frags, parser = .p)$name, want,
                     info = paste0("multi-byte characters survive ", n,
                                   "-byte chunking [", .p, "]"))
    }
}

## -- the parsers must agree exactly ----------------------------------------

if (length(.parsers) > 1L) {
    for (f in list.files("bql", pattern="[.]json$")) {
        out <- lapply(.parsers, function(p)
            tryCatch(suppressWarnings(.parse(f, parser=p)),
                     error=function(e) conditionMessage(e)))
        expect_true(all(vapply(out[-1], identical, logical(1), out[[1]])),
                    info = paste("all parsers agree on", f))
    }
}

## -- parser selection ------------------------------------------------------

expect_true(Rblpapi:::.bqlParser() %in% .parsers, info = "default parser is installed")
local({
    old <- options(Rblpapi.bqlParser=.parsers[length(.parsers)])
    on.exit(options(old))
    expect_equal(Rblpapi:::.bqlParser(), .parsers[length(.parsers)],
                 info = "option selects the parser")
})
local({
    old <- options(Rblpapi.bqlParser="notAParser")
    on.exit(options(old))
    expect_error(Rblpapi:::.bqlParser(), info = "unknown parser name is rejected")
})

## -- column conversion -----------------------------------------------------

.col <- function(...) Rblpapi:::.bqlColumn(list(...))

## a column of only nulls must keep the type its declaration implies
expect_equal(.col(type="STRING", values=list(NULL, NULL)), c(NA_character_, NA_character_),
             info = "all-null STRING column stays character")
expect_equal(.col(type="DATE", values=list(NULL)), as.Date(NA),
             info = "all-null DATE column stays Date")
expect_equal(.col(type="DOUBLE", values=list(NULL)), NA_real_,
             info = "all-null DOUBLE column stays numeric")

## an empty or absent 'values' key gives a zero-length column, not an error
expect_equal(.col(type="DOUBLE", values=list()), numeric(0), info = "empty column")
expect_equal(.col(type="STRING"), character(0), info = "absent values key")

## JSON booleans and their string spellings both convert
expect_equal(.col(type="BOOLEAN", values=list(TRUE, FALSE, NULL)), c(TRUE, FALSE, NA),
             info = "JSON booleans")
expect_equal(.col(type="BOOLEAN", values=list("true", "FALSE", NULL)), c(TRUE, FALSE, NA),
             info = "boolean strings")

## numeric columns keep full double precision: the values do not go through
## character, which would keep only 15 significant digits
expect_identical(.col(type="DOUBLE", values=list(pi, 1/3)), c(pi, 1/3),
                 info = "DOUBLE column is lossless")

## a nested value would silently shift the rows of a column, so it must fail
expect_error(.col(name="X", type="STRING", values=list("a", list("b", "c"))),
             pattern = "non-scalar", info = "non-scalar values are rejected")

## -- live test (requires a Bloomberg connection) ----------------------------

.runThisTest <- Sys.getenv("RunRblpapiUnitTests") == "yes"
if (!.runThisTest) exit_file("Skipping live BQL test")

res <- bql("get(px_last) for(['IBM US Equity', 'AAPL US Equity'])")
expect_true(inherits(res, "data.frame"), info = "live query returns data.frame")
expect_equal(nrow(res), 2L, info = "one row per security")
expect_true(is.numeric(res$px_last), info = "px_last is numeric")
