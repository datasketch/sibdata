library(sibdata)
library(lfltmagic)

devtools::load_all()


library(tictoc)

here::dr_here()
#here::set_here("./..")
setwd("../")
here::dr_here()
tic()

con <- DBI::dbConnect(RSQLite::SQLite(), sys_file("db/sibdata.sqlite"),
                      read_only = TRUE)


## Generate info pages
save_info_page("static/data", con)

dbDisconnect(con)


