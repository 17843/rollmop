.onLoad <- function(libname = "~/R", pkgname = "rollmop") {
  require(rClr)
  adomd   <- file.path(getNativeLibsPath("rClr"), "Microsoft.AnalysisServices.AdomdClient.dll")
  rollmop <- file.path(getNativeLibsPath("rClr"), "rollmop.dll")
  clrLoadAssembly(adomd); clrLoadAssembly(rollmop)
}