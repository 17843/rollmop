#' @export
setClass("OlapConnection", 
         contains = "cobjRef",
         representation(dataSource = "character", 
                        sessionID  = "character"))

#' Create a connection
#'
#' @return An \code{OlapConnection} object. 
#' @export
#' @examples
#' conn <- olapConnection()
setGeneric("olapConnection", 
           function (x = NULL) {
                   conn <- rClr::clrNew("Microsoft.AnalysisServices.AdomdClient.AdomdConnection")
                   conn <- as(conn, "OlapConnection")
           })

#' @export
#' @describeIn olapConnection Connect using a connection string. See \url{http://msdn.microsoft.com/en-us/library/dn140245.aspx}
setMethod("olapConnection", "character", 
          function(x) {
                  conn <- rClr::clrNew("Microsoft.AnalysisServices.AdomdClient.AdomdConnection", x)
                  conn <- as(conn, "OlapConnection")
          })

#' @export
#' @describeIn olapConnection Connect using a template \code{\link{OlapConnection}}
setMethod("olapConnection", "OlapConnection", 
          function(x) {
                  template  <- as(x, "cobjRef")
                  conn <- rClr::clrNew("Microsoft.AnalysisServices.AdomdClient.AdomdConnection", template)
                  conn <- as(conn, "OlapConnection")
          })



setMethod("show", "OlapConnection", function(object) {
        conn <- as(object, "cobjRef")
        state <- rClr::clrGet(conn, "State")
        cat("Connection state: ", ifelse(state == 1, "Open", "Closed"), "\n")
        if (state == 1) {
          cat("Session ID:       ", rClr::clrGet(conn, "SessionID"), "\n")
          cat("Client version:   ", rClr::clrGet(conn, "ClientVersion"), "\n")
          cat("Server version:   ", rClr::clrGet(conn, "ServerVersion"), "\n")
        }
})

#' Open a connection
#'
#' @param An \code{OlapConnection} object
#' @export
#' @examples
#' open(conn)
setMethod("open", "OlapConnection", function(con, newSession = TRUE, quietly = TRUE, ...) {
  conn <- as(con, "cobjRef")
  if(rClr::clrGet(conn, "State") == 1) {
    warning("The connection is already open.")
    return()
  }
  if(newSession == FALSE) {
    sessionID <- rClr::clrGet(conn, "SessionID")
    rClr::clrSet(conn, "SessionID", sessionID)
  }
  rClr::clrCall(conn, "Open")
  if(quietly == FALSE) {
    if(rClr::clrGet(conn, "State") == 1) {
      message("Connection to the analytical data source established.")
    } else {
      warning("Failed to open the connection.")
    }
  }
})

#' Close a connection
#'
#' @param An \code{OlapConnection} object
#' @export
#' @examples
#' close(conn)
setMethod("close", "OlapConnection", function(con, endSession = TRUE, quietly = TRUE, ...) {
  conn <- as(con, "cobjRef")
  rClr::clrCall(conn, "Close", endSession)
  if(quietly == FALSE) {
    if(rClr::clrGet(conn, "State") == 0) {
      message("Connection to the analytical data source closed.")
    } else {
      warning("Failed to close the connection.")
    }
  }
})


# Connection properties
olapGetSessionID <- function (conn) {
        rClr::clrGet(as(conn, "cobjRef"), "SessionID")
}

olapGetServerVersion <- function (conn) {
        rClr::clrGet(as(conn, "cobjRef"), "ServerVersion")
}
