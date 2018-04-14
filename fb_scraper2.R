#!/usr/bin/env Rscript
setwd("~/fb/")
source("fb_common.R")

remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4446L, browserName = "firefox")
remDr$open(silent = T)
remDr$navigate("http://www.facebook.com")

fb_login(id = "melnykeo94@gmail.com")
#jj <- exezekutor(persons[203:404])

post <- 0

for (i in 1:50000) {
  tryCatch({
    if(check_sql(con)) con <- connect_sql()
    old <- query(con) %>% pull(post_link)
    #old <- read_csv("facebook.csv", col_names = F)$X4
    jj <- exezekutor(persons[278:554])$result #205
    jj <- jj[!is.na(jj)]
    new_post <- unique(jj[!str_detect(jj, "l.facebook.com")])
    #new_post <- jj[!jj %in% jj[duplicated(jj)]]
    new_post <- new_post[!new_post %in% old]
    for(i in new_post) {
      tryCatch({
        request <- get_post_info(i)
        if(!is.na(request$user)) {
          if(check_sql(con)) con <- connect_sql()
          wtable(con)
          #readr::write_excel_csv(request, "facebook.csv", append = T)
        }
        post = post + 1
      }, error = function(e) NULL)
    }
    
    #request <- purrr::map_dfr(new_post, get_post_info)
    #request <- request %>% filter(!is.na(user))
    #readr::write_excel_csv(request, "facebook.csv", append = T)
    #post <- sum(post, nrow(request))
    cat(paste0(Sys.time(),": ", post, " posts added"))
    #jj <- unique(jj)
    lapply(c("p", "act", "datr", "lu", "fr", "presence", "csm", "pl", "sb"), remDr$deleteCookieNamed)
  }, error = function(e) NULL)
}
