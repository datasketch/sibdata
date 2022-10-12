
.onLoad <- function(libname, pkgname){
  # con <- sib_create_connection()
  # package_env <- parent.env(environment())
  # dbcooper::dbc_init(con, "sibdata", env = package_env)

  con <- getOption("con")
  if(!is.null(con)){
    DBI::dbDisconnect(con,shutdown=TRUE)
    duckdb::duckdb_shutdown(duckdb::duckdb())
  }

  con <- DBI::dbConnect(duckdb::duckdb(), sys_file("db/sibdata.duckdb"),
                        read_only = TRUE)
  options(con = con)
}

.onUnload <- function(libpath){
  con <- getOption("con")
  DBI::dbDisconnect(con)
  #duckdb::duckdb_shutdown()
  #dbcooper::dbc_clear_connections()

}


