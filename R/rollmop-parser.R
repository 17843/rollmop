#' Execute a command
#'
#' @param cmd A command of class \code{OlapCommand}
#' @return Boolean TRUE if executed successfully.
#' @export
#' @examples
#' parser <- olapExecute(cmd)
olapExecute <- function(cmd) {
  exec <- function(clrCmd) {
    cmd@.cache$parser <- rClr::clrNew("rollmop.RollmopParser", clrCmd)
    rClr::clrCall(cmd@.cache$parser, "executeAndParse")
  }
  if (exists("parser", envir = cmd@.cache) && class(cmd@.cache$parser) == "cobjRef") {
    return(TRUE)
  } else {
    clrCmd <- as(cmd, "cobjRef")
    if (try(exec(clrCmd), silent = TRUE) != TRUE) {
      msg <- capture.output(clrTraceback())
      e   <- simpleError(gsub("Message: ", "", msg[2], fixed = TRUE))
      stop(e)      
    } else {
      return(TRUE)
    }
  }
}

parseCells <- function(clrParser, split = TRUE, resolveClasses = split) {
  cols  <- rClr::clrGet(clrParser, "colCount")
  cells <- rClr::clrGet(clrParser, "cells")
  cells <- if (is.numeric(as.numeric(cells)) && resolveClasses == FALSE) {
    matrix(as.numeric(cells), ncol  = cols, byrow = TRUE)
  } else {
    matrix(cells, ncol = cols, byrow = TRUE)
  }
  if (split == TRUE) {
    cells <- split(cells, col(cells))
    if (resolveClasses == TRUE) {
      xsi <- rClr::clrGet(clrParser, "cellClasses")
      xsi[xsi == ""] <- NA
      classes <- getColumnClasses(matrix(xsi, ncol = cols, byrow = TRUE))
      for(i in seq(1:length(cells))) {
        cells[[i]] <- as(cells[[i]], classes[i])
      }
    }    
  }
  return(cells)
}

parseColumnNames <- function(clrParser, crossjoinSep = ".X.", appendHierarchies = TRUE) {
  captions <- rClr::clrGet(clrParser, "colLabels")
  cols     <- rClr::clrGet(clrParser, "colCount")
  if (length(captions) > cols) {
    captions <- matrix(captions, ncol = cols, byrow = FALSE)        
    captions <- apply(captions, 2, paste, collapse = crossjoinSep)
  }
  if (appendHierarchies) {
    captions <- c(rClr::clrGet(clrParser, "levels"), captions)
  }
  return(captions)
}

parseRowNames <- function(clrParser, crossjoinSep = ".X.") {
  captions <- rClr::clrGet(clrParser, "rowLabels")   
  rows     <- rClr::clrGet(clrParser, "rowCount")
  captions <- matrix(captions, ncol = length(captions) / rows, byrow = TRUE)
  rowNames <- split(captions, col(captions))
  return(rowNames)
}

parseAsList <- function(clrParser, resolveClasses = TRUE) {
  dim <- c(rows = rClr::clrGet(clrParser, "rowCount"),
           cols = rClr::clrGet(clrParser, "colCount"))
  
  if (dim["rows"] > 1) {
    result <- c(
               parseRowNames(clrParser),
               parseCells(clrParser, resolveClasses = resolveClasses))
  } else {
    result <- parseCells(clrParser, resolveClasses = resolveClasses)
  }
  names(result) <- parseColumnNames(clrParser)
  return(result)
}


getColumnClasses <- function(m, threshold = 0.75) {
  data.types <- list(
    "xsd:double" = "numeric",
    "xsd:int"    = "integer",
    "xsd:short"  = "integer",
    "xsd:string" = "character"
  )
  
  m <- apply(m, c(1, 2), function(t) ifelse(!is.na(t), t <- data.types[[t]], NA))
  
  if(dim(unique(na.omit(m)))[1] == 1) {
    colClasses <- as.vector(unique(na.omit(m)))
  } else {
    colClasses <- vector(mode = "character")
    for(i in seq(1:dim(m)[2])) {
      cls <- unique(na.omit(m[,i]))
      if(length(cls) == 1) {
        colClasses[i] <- cls
      } else if (identical(sort(cls), c("integer", "numeric"))) {
        colClasses[i] <- "numeric"
      } else {
        freq <- ftable(cls) / length(cls)
        if(max(freq) >= threshold) {
          rank <- sort(freq, decreasing = TRUE)
          colClasses[i] <- names(rank[1])
        } else {
          colClasses[i] <- "character"
        }
      }
    }
  }
  return(colClasses)
}
