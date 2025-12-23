library(sibdata)
library(tictoc)
devtools::load_all()
here::dr_here()
here::set_here("./..")
setwd("../")
here::dr_here()
tic()

dir_exist <- dir.exists("static/data")
if (!dir_exist) dir.create("static/data", recursive = TRUE)


con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"),
                      read_only = TRUE)


## Generate info pages
save_info_page("static/data", con)

DBI::dbDisconnect(con)


