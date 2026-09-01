
##  Copyright (C) 2025  Whit Armstrong and Dirk Eddelbuettel and John Laing
##
##  This file is part of Rblpapi
##
##  Rblpapi is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 2 of the License, or
##  (at your option) any later version.
##
##  Rblpapi is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with Rblpapi.  If not, see <http://www.gnu.org/licenses/>.


##' This function uses the Bloomberg API to execute 'BQL' (Bloomberg
##' Query Language) queries via the \sQuote{//blp/bqlsvc} service --
##' the same service used by the Excel \code{=BQL()} function.
##'
##' The service returns a single JSON document. Each queried data
##' item is self-describing: every column carries a declared type
##' (\sQuote{STRING}, \sQuote{DOUBLE}, \sQuote{INT}, \sQuote{DATE},
##' \sQuote{DATETIME}, \sQuote{BOOLEAN}) which is used to construct
##' properly-typed \code{data.frame} columns. Parsing requires either
##' the \CRANpkg{RcppSimdJson} or the \CRANpkg{jsonlite} package;
##' \CRANpkg{RcppSimdJson} is preferred when both are installed as it
##' is faster on the large documents BQL can return. Both give the same
##' result for the documents the service returns. Set
##' \code{parse=FALSE} to obtain the raw JSON string instead, e.g. for
##' queries whose shape the parser does not handle.
##'
##' Note that \sQuote{//blp/bqlsvc} is not part of the officially
##' documented public API; it is the service behind the Excel BQL
##' add-in and may change without notice.
##'
##' @title Run 'Bloomberg Query Language' (BQL) Queries
##' @param expression A character string with the BQL query, e.g.
##' \code{"get(px_last) for(['IBM US Equity'])"}.
##' @param parse A boolean indicating whether the JSON response should
##' be parsed into \code{data.frame} objects (requires either the
##' \CRANpkg{RcppSimdJson} or the \CRANpkg{jsonlite} package),
##' defaults to \sQuote{TRUE}. If \sQuote{FALSE} the raw JSON string
##' is returned. The option \code{Rblpapi.bqlParser} selects the
##' parser explicitly, e.g. \code{options(Rblpapi.bqlParser="jsonlite")}.
##' @param simplify A boolean indicating whether a query returning a
##' single data item should be returned directly as a \code{data.frame}
##' instead of a list of length one, defaults to \sQuote{TRUE}.
##' @param verbose A boolean indicating whether verbose operation is
##' desired, defaults to \sQuote{FALSE}.
##' @param con A connection object as created by a \code{blpConnect}
##' call, and retrieved via the internal function
##' \code{defaultConnection}.
##' @return If \code{parse} is \sQuote{TRUE}, a named list of
##' \code{data.frame} objects, one per data item in the query's
##' \code{get()} clause (or a single \code{data.frame} if
##' \code{simplify} is \sQuote{TRUE} and only one item was queried).
##' Each \code{data.frame} has an \sQuote{ID} column, a value column
##' named after the data item, and any secondary columns (such as
##' \sQuote{DATE} or \sQuote{CURRENCY}) the service returned. If
##' \code{parse} is \sQuote{FALSE}, a character string with the JSON
##' document.
##' @author Alexander Kammerer and Dirk Eddelbuettel
##' @examples
##' \dontrun{
##' con <- blpConnect()
##' bql("get(px_last) for(['IBM US Equity', 'AAPL US Equity'])")
##' bql("get(px_last, name) for(members('INDU Index'))", simplify=FALSE)
##' }
bql <- function(expression,
                parse=TRUE,
                simplify=TRUE,
                verbose=FALSE,
                con=defaultConnection()) {

    ## resolve the parser before the request so that a missing package does
    ## not discard a response which has already been retrieved
    parser <- if (parse) .bqlParser() else NULL
    res <- bql_Impl(con, expression, verbose)
    if (!parse) return(.bqlJoin(res))
    .bqlParse(res, simplify=simplify, parser=parser)
}

## The service delivers responses larger than 4 MiB in several messages, cutting
## the JSON mid-token: the fragments form one document only once joined
.bqlJoin <- function(fragments) paste0(fragments, collapse="")

## Supported JSON parsers, in order of preference
.bqlParsers <- c("RcppSimdJson", "jsonlite")

## Select the JSON parser: RcppSimdJson is preferred as it is faster on the
## large documents BQL can return, jsonlite is the fallback. The option
## 'Rblpapi.bqlParser' forces one, which also lets the tests exercise both.
.bqlParser <- function() {
    ## validated here rather than with match.arg(), which would accept an
    ## abbreviation and would silently drop an unknown name given alongside a
    ## known one
    want <- getOption("Rblpapi.bqlParser", .bqlParsers)
    if (!is.character(want) || length(want) == 0L || anyNA(want) ||
        !all(want %in% .bqlParsers))
        stop("Option 'Rblpapi.bqlParser' must be one or more of ",
             paste0("'", .bqlParsers, "'", collapse=", "), call.=FALSE)
    for (p in want) if (requireNamespace(p, quietly=TRUE)) return(p)
    ## name only what was actually asked for, which may be a single parser
    stop("Parsing BQL responses requires ",
         paste0("'", want, "'", collapse=" or "),
         "; install it or call bql(..., parse=FALSE) for the raw JSON.",
         call.=FALSE)
}

## Parse one JSON document into nested lists. Both parsers are asked not to
## simplify at all so that they return the very same structure: the typing is
## done from the declared BQL column types in .bqlColumn. The two 'empty'
## arguments make RcppSimdJson agree with jsonlite on '[]' and '{}', which it
## maps to NULL by default.
.bqlFromJSON <- function(txt, parser=.bqlParser()) {
    switch(parser,
           "RcppSimdJson" =
               RcppSimdJson::fparse(txt,
                                    max_simplify_lvl="list",
                                    empty_array=list(),
                                    empty_object=structure(list(),
                                                           names=character())),
           "jsonlite" =
               jsonlite::fromJSON(txt, simplifyVector=FALSE),
           ## without this a wrong name would return NULL, and the caller
           ## would see an empty result rather than a diagnosis
           stop("Unknown BQL JSON parser '", parser, "'", call.=FALSE))
}

## Parse a raw BQL JSON response into a named list of data.frames
.bqlParse <- function(json, simplify=TRUE, parser=.bqlParser()) {
    parsed <- .bqlFromJSON(.bqlJoin(json), parser)
    .bqlCheckExceptions(parsed)
    tables <- list()
    for (item in parsed[["results"]]) {
        nm <- if (is.null(item[["name"]])) "" else item[["name"]]
        msgs <- .bqlExceptionMessages(item[["responseExceptions"]])
        if (length(msgs))
            warning("BQL error for item '", nm, "': ",
                    paste(msgs, collapse="; "), call.=FALSE)
        tables[[nm]] <- .bqlItemToDataFrame(item)
    }
    if (simplify && length(tables) == 1L) return(tables[[1L]])
    tables
}

## Raise an R error for any top-level 'responseExceptions' the service reported
.bqlCheckExceptions <- function(parsed) {
    msgs <- .bqlExceptionMessages(parsed[["responseExceptions"]])
    if (length(msgs))
        stop("BQL error: ", paste(msgs, collapse="; "), call.=FALSE)
    invisible(NULL)
}

.bqlExceptionMessages <- function(excs) {
    if (is.null(excs) || length(excs) == 0L) return(character())
    vapply(excs, function(e) {
        msg <- e[["message"]]
        if (is.null(msg) || !nzchar(msg)) msg <- e[["internalMessage"]]
        if (is.null(msg) || !nzchar(msg)) msg <- "unknown BQL error"
        msg
    }, character(1))
}

## Convert one entry of 'results' into a data.frame using the declared
## column types; the value column is named after the data item itself
.bqlItemToDataFrame <- function(item) {
    cols <- list()
    idcol <- item[["idColumn"]]
    if (!is.null(idcol))
        cols[[.bqlColName(idcol, "ID")]] <- .bqlColumn(idcol)
    valcol <- item[["valuesColumn"]]
    if (!is.null(valcol)) {
        nm <- if (is.null(item[["name"]]) || !nzchar(item[["name"]]))
                  .bqlColName(valcol, "VALUE") else item[["name"]]
        cols[[nm]] <- .bqlColumn(valcol)
    }
    for (sec in item[["secondaryColumns"]])
        cols[[.bqlColName(sec, "V")]] <- .bqlColumn(sec)
    names(cols) <- make.unique(names(cols))
    ## avoid data.frame() name mangling and rownames
    structure(cols,
              class="data.frame",
              row.names=if (length(cols)) seq_along(cols[[1L]]) else integer())
}

.bqlColName <- function(col, fallback) {
    nm <- col[["name"]]
    if (is.null(nm) || !nzchar(nm)) fallback else nm
}

## Convert a BQL column (list with 'type' and 'values') to a typed R vector.
## The values arrive as a list of scalars, one element per row. They are
## flattened with vectorised primitives rather than one element at a time:
## 'lengths()' finds the JSON nulls without a call per element, and unlist()
## does the rest in one step.
##
## JSON null maps to NA for every type; the string placeholders "NaN" and
## "NA" additionally map to NA for numeric columns only, as string columns
## may legitimately contain them (e.g. the ticker of 'NA US Equity').
##
## A numeric column stays numeric throughout and so keeps the values exactly
## as the service sent them, rather than losing the last digits to a detour
## through character. Bloomberg sends float-derived prices such as
## 230.66000366210938, which as.character() would truncate to
## 230.66000366210901.
##
## One consequence of letting unlist() pick the type does remain: it coerces a
## logical before a string, so a JSON boolean sharing an array with a JSON
## number becomes 1 or 0 rather than "TRUE" or "FALSE". BQL declares one type
## per column and does not mix the two, and avoiding this would need a call
## per element for every column, which is the cost this function exists to
## avoid.
.bqlColumn <- function(col) {
    type <- if (is.null(col[["type"]])) "STRING" else col[["type"]]
    values <- col[["values"]]
    n <- length(values)
    values[lengths(values) == 0L] <- NA
    values <- unlist(values, use.names=FALSE)
    ## unlist() flattens a nested value instead of failing, unlike the vapply()
    ## this replaces. This catches a value which flattens to more than one
    ## element; one which flattens to exactly one is kept, as it was before.
    if (length(values) != n)
        stop("BQL column '", .bqlColName(col, "?"),
             "' has non-scalar values", call.=FALSE)
    ## Blank the "NaN", "NA" and "" placeholders which stand for a missing
    ## number. Doing it in the list, before flattening, is what lets unlist()
    ## keep the column numeric: a single such string would otherwise promote
    ## the whole column to character and send every number back through its 15
    ## significant digit form. Only a numeric column is treated this way, as a
    ## string column may legitimately hold those spellings. Any other string is
    ## a number written as a string, which as.numeric() below still converts.
    ## This costs one pass per element, and only for a numeric column which
    ## really does contain a placeholder.
    if (is.character(values) && type %in% c("DOUBLE", "INT")) {
        vals <- col[["values"]]
        isph <- vapply(vals, is.character, NA)
        isph[isph] <- unlist(vals[isph], use.names=FALSE) %in% c("NaN", "NA", "")
        if (any(isph)) {
            vals[isph | lengths(vals) == 0L] <- NA
            values <- unlist(vals, use.names=FALSE)
        }
    }
    switch(type,
           "DOUBLE"   = as.numeric(values),
           "INT"      = as.integer(values),
           "BOOLEAN"  = if (is.logical(values)) values
                        else as.logical(toupper(.bqlChar(values))),
           "DATE"     = .bqlByUnique(substr(.bqlChar(values), 1L, 10L), as.Date),
           "DATETIME" = .bqlByUnique(.bqlChar(values), function(u)
                            as.POSIXct(u, format="%Y-%m-%dT%H:%M:%OS", tz="UTC")),
           .bqlChar(values))
}

## A column of only nulls flattens to a logical vector, so the character types
## still need the conversion; for an actual character vector this is a no-op
.bqlChar <- function(v) if (is.character(v)) v else as.character(v)

## Parsing a date string costs far more per value than a hash lookup, and BQL
## date columns repeat heavily (one date per period, the same date for many
## securities), so convert only the distinct strings
.bqlByUnique <- function(v, fun) {
    u <- unique(v)
    fun(u)[match(v, u)]
}
