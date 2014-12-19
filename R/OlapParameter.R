# http://msdn.microsoft.com/en-us/library/microsoft.analysisservices.adomdclient.adomdparameter.aspx
#' @export
setClass("OlapParameter", 
         contains = "cobjRef") # add a 'set' slot...

#' Create a new parameter
#'
#' @return An \code{OlapParameter} object. 
#' @param name Parameter name
#' @param value Parameter value
#' @export
#' @examples
#' conn <- olapConnection("Data Source=localhost;")
#' open(conn)
#' cmd <- olapCommand("SELECT StrToSet(@@Hierarchy) ON 0 FROM [Table] WHERE [Some].[Measure]", conn)
#' param <- olapParameter("Hierarchy", "[Some].[Hierarchy].Members")
#' olapAddParameters(param, cmd)
#' df <- as.data.frame(cmd)
olapParameter <- function (name = NULL, value = NULL) {
  if (is.null(name) || is.null(value)) {
    param <- as(clrNew("Microsoft.AnalysisServices.AdomdClient.AdomdParameter"), "OlapParameter")
  } else {
    param <- as(clrNew("Microsoft.AnalysisServices.AdomdClient.AdomdParameter", name, value), "OlapParameter")
  }
  return(param)
}

#' @export
setClass("OlapParameterCollection", 
         contains = "cobjRef")

#' @export
olapGetParameters <- function (cmd) {
        cmd <- as(cmd, "cobjRef")
        params <- as(clrCall(cmd, "get_Parameters"), "OlapParameterCollection") # as list?
}

#' Add parameter(s) to command
#'
#' @export
#' @examples
#' conn <- olapConnection("Data Source=localhost;")
#' open(conn)
#' cmd <- olapCommand("SELECT StrToSet(@@Hierarchy) ON 0 FROM [Table] WHERE [Some].[Measure]", conn)
#' param <- olapParameter("Hierarchy", "[Some].[Hierarchy].Members")
#' olapAddParameters(param, cmd)
#' df <- as.data.frame(cmd)
setGeneric("olapAddParameters", 
           function (x, params, ...) {
             standardGeneric("olapAddParameters")
           })

#' @export
#' @describeIn olapAddParameters
setMethod("olapAddParameters", signature(x = "OlapParameterCollection", params = "OlapParameter"),
           function (x, params, ...) {
             col <- as(x, "cobjRef")
             clrCall(col, "Add", as(params, "cobjRef"))
           })

#' @export
#' @describeIn olapAddParameters 
setMethod("olapAddParameters", signature(x = "OlapCommand", params = "OlapParameter"), 
          function(x, params, ...) {
            cmd <- as(x, "cobjRef")
            pc <- clrCall(cmd, "get_Parameters")
            clrCall(pc, "Add", as(params, "cobjRef"))
          })

#' @export
#' @describeIn olapAddParameters 
setMethod("olapAddParameters", signature(x = "OlapCommand", params = "list"), 
          function(x, params, ...) {
            cmd <- as(x, "cobjRef")
            pc <- clrCall(cmd, "get_Parameters")
            for(i in seq(1:length(params))) {
              clrCall(pc, "Add", as(olapParameter(names(params)[i], params[[i]]), "cobjRef"))  
            }
          })

