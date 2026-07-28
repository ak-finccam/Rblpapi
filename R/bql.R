
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
##' The service returns one or more JSON documents. Each queried data
##' item is self-describing: every column carries a declared type
##' (\sQuote{STRING}, \sQuote{DOUBLE}, \sQuote{INT}, \sQuote{DATE},
##' \sQuote{DATETIME}, \sQuote{BOOLEAN}) which is used to construct
##' properly-typed \code{data.frame} columns. Parsing requires the
##' \CRANpkg{jsonlite} package; set \code{parse=FALSE} to obtain the
##' raw JSON string(s) instead, e.g. for queries whose shape the
##' parser does not handle.
##'
##' Note that \sQuote{//blp/bqlsvc} is not part of the officially
##' documented public API; it is the service behind the Excel BQL
##' add-in and may change without notice.
##'
##' @title Run 'Bloomberg Query Language' (BQL) Queries
##' @param expression A character string with the BQL query, e.g.
##' \code{"get(px_last) for(['IBM US Equity'])"}.
##' @param parse A boolean indicating whether the JSON response should
##' be parsed into \code{data.frame} objects (requires the
##' \CRANpkg{jsonlite} package), defaults to \sQuote{TRUE}. If
##' \sQuote{FALSE} the raw JSON string(s) are returned.
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
##' \code{parse} is \sQuote{FALSE}, a character vector of JSON
##' documents.
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

    res <- bql_Impl(con, expression, verbose)
    if (!parse) return(res)
    if (!requireNamespace("jsonlite", quietly=TRUE))
        stop("The 'jsonlite' package is required to parse BQL responses; ",
             "install it or call bql(..., parse=FALSE) for the raw JSON.",
             call.=FALSE)
    .bqlParse(res, simplify=simplify)
}

## Parse one or more raw BQL JSON documents into a named list of data.frames
.bqlParse <- function(json, simplify=TRUE) {
    tables <- list()
    for (doc in json) {
        parsed <- jsonlite::fromJSON(doc, simplifyVector=FALSE)
        .bqlCheckExceptions(parsed)
        for (item in parsed[["results"]]) {
            df <- .bqlItemToDataFrame(item)
            nm <- if (is.null(item[["name"]])) "" else item[["name"]]
            if (!is.null(tables[[nm]])) {        # same item split across partial responses
                tables[[nm]] <- rbind(tables[[nm]], df)
            } else {
                tables[[nm]] <- df
            }
        }
    }
    if (simplify && length(tables) == 1L) return(tables[[1L]])
    tables
}

## Raise an R error for any 'responseExceptions' the service reported
.bqlCheckExceptions <- function(parsed) {
    excs <- parsed[["responseExceptions"]]
    if (is.null(excs) || length(excs) == 0L) return(invisible(NULL))
    msgs <- vapply(excs, function(e) {
        msg <- e[["message"]]
        if (is.null(msg) || !nzchar(msg)) msg <- e[["internalMessage"]]
        if (is.null(msg) || !nzchar(msg)) msg <- "unknown BQL error"
        msg
    }, character(1))
    stop("BQL error: ", paste(msgs, collapse="; "), call.=FALSE)
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
## JSON null and string "NaN" placeholders both map to NA.
.bqlColumn <- function(col) {
    values <- col[["values"]]
    type <- if (is.null(col[["type"]])) "STRING" else col[["type"]]
    values <- vapply(values, function(v) {
        if (is.null(v) || (is.character(v) && v %in% c("NaN", "NA"))) NA_character_
        else as.character(v)
    }, character(1))
    switch(type,
           "DOUBLE"   = as.numeric(values),
           "INT"      = as.integer(values),
           "BOOLEAN"  = as.logical(toupper(values)),
           "DATE"     = as.Date(substr(values, 1L, 10L)),
           "DATETIME" = as.POSIXct(values, format="%Y-%m-%dT%H:%M:%OS", tz="UTC"),
           values)
}
