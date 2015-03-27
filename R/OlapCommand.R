#' @export
setClass("OlapCommand", 
         contains = "cobjRef",
         representation(.cache = "environment",
                        Connection  = "OlapConnection",
                        CommandText = "character"))


setMethod("initialize", "OlapCommand", 
          function(.Object, ...) { 
            # more init stuff
            .Object@.cache <- new.env()
            callNextMethod(.Object, ...) 
          })

#' Create a new command
#'
#' @return An \code{OlapCommand} object. 
#' @param cmd A valid MDX command, either as a length one character vector or a reference to a file.
#' @param conn  An \code{OlapConnection} object.
#' @export
#' @examples
#' conn <- olapConnection("Data Source=localhost;")
#' open(conn)
#' cmd <- olapCommand("SELECT [Some].[Hierarchy].Members ON 0 FROM [Table] WHERE [Some].[Measure]", conn)
#' df <- as.data.frame(cmd)
olapCommand <- function (cmdText = NULL, conn = NULL) {
  if(!is.null(cmdText) && file.exists(cmdText)) {
    # read MDX
    cmdText <- readChar(cmdText, file.info(cmdText)$size)
  }
  if(!is.null(conn)) conn <- as(conn, "cobjRef")
  if(!is.null(cmdText) && !is.null(conn)) {
    cmd <- as(rClr::clrNew("Microsoft.AnalysisServices.AdomdClient.AdomdCommand", cmdText, conn), "OlapCommand")
    cmd@CommandText <- gsub("[\r\n]", "", cmdText)
    cmd@Connection  <- as(conn, "OlapConnection")
  } else if (!is.null(cmdText)) {
    cmd <- as(rClr::clrNew("Microsoft.AnalysisServices.AdomdClient.AdomdCommand", cmdText), "OlapCommand")
    cmd@CommandText <- gsub("[\r\n]", "", cmdText)
  } else {
    cmd <- as(rClr::clrNew("Microsoft.AnalysisServices.AdomdClient.AdomdCommand"), "OlapCommand")                
  }
  return(cmd)
}

#' @export
setMethod("as.data.frame", "OlapCommand", 
          function(x, row.names = NULL, optional = FALSE, ..., stringsAsFactors = default.stringsAsFactors(), resolveClasses = TRUE) {
            if (exists("parser", envir = x@.cache) && class(x@.cache$parser) == "cobjRef") {
              result <- parseAsList(x@.cache$parser, resolveClasses = resolveClasses) # add this list to cache?
            } else {
              parser <- olapExecute(x)
              result <- parseAsList(parser, resolveClasses = resolveClasses)
            }
             as.data.frame(result,
                          row.names = row.names,
                          optional = optional,
                          ..., 
                          stringsAsFactors = stringsAsFactors)            
          }
)

#' @export
olapCommand2Df <- function(x, row.names = NULL, optional = FALSE, ..., stringsAsFactors = default.stringsAsFactors(), resolveClasses = TRUE) {
  if (exists("parser", envir = x@.cache) && class(x@.cache$parser) == "cobjRef") {
    result <- parseAsList(x@.cache$parser, resolveClasses = resolveClasses) # add this list to cache?
  } else {
    parser <- olapExecute(x)
    result <- parseAsList(parser, resolveClasses = resolveClasses)
  }
  as.data.frame(result,
                row.names = row.names,
                optional = optional,
                ..., 
                stringsAsFactors = stringsAsFactors)            
}


#' @export
setMethod("as.list", "OlapCommand", 
          function(x, ..., resolveClasses = TRUE) {
            if (exists("parser", envir = x@.cache) && class(x@.cache$parser) == "cobjRef") {
              result <- parseAsList(x@.cache$parser, split = TRUE, resolveClasses = resolveClasses) # add this list to cache?
            } else {
              parser <- olapExecute(x)
              result <- parseAsList(parser)
            }
          }
)

#' @export
setMethod("as.matrix", "OlapCommand", 
          function(x, ..., resolveClasses = FALSE) {
            if (exists("parser", envir = x@.cache) && class(x@.cache$parser) == "cobjRef") {
              parser <- x@.cache$parser
              result <- parseCells(parser, split = FALSE, resolveClasses = resolveClasses) # add this list to cache?
            } else {
              parser <- olapExecute(x)
              result <- parseCells(parser, split = FALSE, resolveClasses = resolveClasses)
            }
            colnames(result) <- parseColumnNames(parser, appendHierarchies = FALSE)
            return(result)
          }
)