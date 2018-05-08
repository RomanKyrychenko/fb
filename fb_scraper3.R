#!/usr/bin/env Rscript
setwd("~/fb/")
source("fb_common.R")

#remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4446L, browserName = "firefox")
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4447L, browserName = "firefox")
remDr$open(silent = T)
remDr$navigate("http://www.facebook.com")

fb_login(id = "+380631725580", pass = "8938006230")

for (i in 1:50000) {
  tryCatch({
    if(check_sql(con)) con <- connect_sql()
    jj <- exezekutor(persons[370:554])$result #205
    jj <- jj[!is.na(jj)]
    new_post <- unique(jj[!str_detect(jj, "l.facebook.com")])
    con <- connect_sql()
    old <- query(con) %>% pull(post_link)
    new_post <- new_post[!new_post %in% old]
    for(i in new_post) {
      tryCatch({
        request <- get_post_info(i)
        if(!is.na(request$user)) {
          interruptor(FUN = wtable, args = con, time.limit = 6,ALTFUN = wtable)
        }
        post = post + 1
      }, error = function(e) NULL)
    }
    cat(paste0(Sys.time(),": ", post, " posts added"))
    lapply(c("p", "act", "datr", "lu", "fr", "presence", "csm", "pl", "sb"), remDr$deleteCookieNamed)
  }, error = function(e) NULL)
}
