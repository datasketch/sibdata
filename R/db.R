

sib_create_connection <- function(){
  # sqlite_file <- NULL
  # if(file.exists("sibdata.sqlite"))
  #   sqlite_file <- "sibdata.sqlite"
  sqlite_file <- sqlite_file %||% sys_file("db/sib.sqlite")
  DBI::dbConnect(RSQLite::SQLite(), sqlite_file)
}



