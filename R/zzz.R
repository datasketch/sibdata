
.onLoad <- function(libname, pkgname){
  con <- sib_create_connection()
  package_env <- parent.env(environment())
  dbcooper::dbc_init(con, "sibdata", env = package_env)
}

.onUnload <- function(libpath){
  dbcooper::dbc_clear_connections()
}


