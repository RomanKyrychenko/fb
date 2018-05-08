Sys.setlocale("LC_ALL", "Ukrainian")

options(warn = -1)

package.list <- c("RSelenium", "dplyr", "httr", "purrr", "rvest", "stringr", "RPostgres")

#for(i in package.list) {
#  if (!require(i)) install.packages(i)
#}

suppressPackageStartupMessages({
  require(methods)
  require(RSelenium)
  require(dplyr)
  require(httr)
  require(purrr)
  require(rvest)
  require(readr)
  require(stringr)
  require(RPostgres)
  require(R.utils)
})

download.file(url = "https://github.com/RomanKyrychenko/fb/raw/master/persons.rds", destfile = "persons.rds")
persons <- readRDS("persons.rds")

fb_login <- function(id = "+380935855724", passw = "v1579~v1363_09") {
  user <- remDr$findElement(using = "id", "email")
  user$sendKeysToElement(list(id))
  pass <- remDr$findElement(using = "id", value = "pass")
  pass$sendKeysToElement(list(passw))
  login <- remDr$findElement(using = "css selector", value = ".uiButton.uiButtonConfirm")
  login$clickElement()
}

alerts_facebook <- function(fb_page, .pb = NULL) {
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  remDr$navigate(fb_page)
  for (i in 1:15) {
    webElem <- remDr$findElement("css", "body")
    Sys.sleep(runif(1,0.1,2))
    webElem$sendKeysToElement(list(key = "end"))
  }
  Sys.sleep(runif(1,15,25))
  page <- remDr$getPageSource()[[1]]
  new_post <- (page %>% read_html() %>% html_nodes(css = "._5pcq") %>% html_attr("href")) %>% unlist() %>% unname()
  ifelse(stringr::str_detect(new_post, "https://www.facebook.com"), new_post, paste0("https://www.facebook.com", new_post))
}

alerts_facebook <- safely(alerts_facebook)

exezekutor <- function(lgd) {
  pb <- progress_estimated(length(lgd))
  af <- function(x) alerts_facebook(x, .pb = pb)$result
  purrr::map(lgd, af) %>% purrr::flatten_chr()
}

exezekutor <- purrr::safely(exezekutor)

get_post_info <- function(post_link) {
  remDr$navigate(post_link)
  Sys.sleep(runif(1,15,30))
  ps <- read_html(remDr$getPageSource()[[1]])
  photos <- NULL
  post <- NULL
  post <- ps %>% html_node("._5pbx.userContent") %>% html_text() %>% str_replace_all(pattern = "Показать перевод","")
  if (is_empty(post)) post <- NA
  if (post == "" | is.na(post)) post <- ps %>% html_node(".mvl._52jv") %>% html_text()
  if (is.na(post)) post <- NA
  
  photos <- (ps %>% html_nodes("._5dec._xcx") %>% html_attr("href"))[1]
  #if (is_empty(photos) | photos == "" | is.null(photos)) photos <- NA
  like <- 0
  love <- 0 
  wow <- 0
  haha <- 0
  sad <- 0
  angry <- 0
  likes <- ps %>% html_nodes("a._3emk") %>% html_attr("aria-label")
  if (!is_empty(likes)) {
    lk <- likes[map_lgl(likes, function(x) str_detect(x, "Нрав"))] %>% str_replace_all(pattern = ",", ".")
    like <- parse_number(lk) * ifelse(str_detect(lk, "тыс"), 1000, 1)
    love <- likes[map_lgl(likes, function(x) str_detect(x, "Супе"))] %>% parse_number()
    wow <- likes[map_lgl(likes, function(x) str_detect(x, "Ух"))] %>% parse_number()
    haha <- likes[map_lgl(likes, function(x) str_detect(x, "Ха-ха"))] %>% substr(5, 10) %>% parse_number()
    sad <- likes[map_lgl(likes, function(x) str_detect(x, "Сочувствую"))] %>% parse_number()
    angry <- likes[map_lgl(likes, function(x) str_detect(x, "Возмутите"))] %>% substr(5, 10) %>% parse_number()
  }
  comments <- (ps %>% html_nodes("._4bl7") %>% html_text())[4] %>% parse_number()
  shares <- ps %>% html_node(".UFIShareLink") %>% html_text() %>% str_replace_all(pattern = ",", ".")
  shares <- parse_number(shares) * ifelse(str_detect(shares, "тыс"), 1000, 1)
  
  data_frame(
    user = (ps %>% html_node("span.fwn.fcg") %>% html_text() %>% word(start = 1, end = 2) %>% str_replace_all("[:punct:]", ""))[1],
    user_link = (ps %>% html_node("._5pb8._8o._8s.lfloat._ohe") %>% html_attr("href") %>% str_replace_all("\\?fref=nf", ""))[1],
    post = post[1],
    post_link = post_link[1],
    type = case_when(str_detect(post_link, "post") ~ "post", str_detect(post_link, "photo") ~ "photo", str_detect(post_link, "video") ~ "video")[1],
    time = (ps %>% html_node("abbr._5ptz") %>% html_attr("data-utime") %>% as.numeric() %>% as.POSIXct(origin = "1970-01-01", tz = "Europe/Kiev"))[1],
    location = (ps %>% html_nodes("._5pcq") %>% html_text())[2],
    privacy = (ps %>% html_node("._6a._29ee._4f-9._43_1") %>% html_attr("data-tooltip-content"))[1],
    photos = photos[1],
    likes_count = sum(like[1], love[1], wow[1], haha[1], sad[1], angry[1], na.rm = T),
    like = like[1],
    love = love[1],
    wow = wow[1],
    haha = haha[1],
    sad = sad[1],
    angry = angry[1],
    comments = comments[1],
    shares = shares[1],
    attr_text = (ps %>% html_node("span.fcg") %>% html_text())[1],
    link_in_post = (ps %>% html_node("a._52c6") %>% html_attr("href"))[1] %>% str_replace_all("%3A",":") %>% str_replace_all("%2F","/") %>% str_replace_all("%3F","?") %>% str_replace_all("%3D","&") %>% substr(32,1000),
    image_link = (ps %>% html_node("._4-eo._2t9n") %>% html_attr("data-ploi"))[1],
    app_name = (ps %>% html_node("a._5pcq._20y0") %>% html_text())[1],
    app_link = (ps %>% html_node("a._5pcq._20y0") %>% html_attr("href"))[1],
    repost_text = (ps %>% html_node(".mtm._5pco") %>% html_text() %>% str_replace_all(pattern = "Показать перевод", ""))[1],
    repost_author = (ps %>% html_node("._1nb_.fwn.fcg") %>% html_text())[1],
    repost_time = (ps %>% html_nodes("abbr._5ptz") %>% html_attr("data-utime") %>% as.numeric() %>% as.POSIXct(origin = "1970-01-01"))[2],
    download_time = Sys.time()
  )
}

# get_post_info <- purrr::safely(get_post_info)

get_like_post <- function(post_link) {
  remDr$navigate(post_link)
  ps <- read_html(remDr$getPageSource()[[1]])
  # likes
  likes <- ps %>% html_nodes("a._3emk") %>% html_attr("aria-label")
  like <- if (!is_empty(likes)) {
    lk <- likes[sapply(likes, function(x) grepl("Нрав", x))]
    lk <- gsub(pattern = ",", ".", lk)
    coef <- ifelse(grepl("тыс", lk), 1000, 1)
    lk <- lk %>% parse_number()
    lk * coef
  } 
  if (is_empty(like) | is_empty(likes)) like <- 0
  love <- if (!is_empty(likes)) {
    likes[sapply(likes, function(x) grepl("Супе", x))] %>% parse_number()
  }
  if (is_empty(love) | is_empty(likes)) love <- 0
  wow <- if (!is_empty(likes)) {
    likes[sapply(likes, function(x) grepl("Ух", x))] %>% parse_number()
  }
  if (is_empty(wow) | is_empty(likes)) wow <- 0
  haha <- if (!is_empty(likes)) {
    likes[sapply(likes, function(x) grepl("Ха-ха", x))] %>% substr(5, 10) %>% parse_number()
  }
  if (is_empty(haha) | is_empty(likes)) haha <- 0
  sad <- if (!is_empty(likes)) {
    likes[sapply(likes, function(x) grepl("Сочувствую", x))] %>% parse_number()
  }
  if (is_empty(sad) | is_empty(likes)) sad <- 0
  angry <- if (!is_empty(likes)) {
    likes[sapply(likes, function(x) grepl("Возмутите", x))] %>% substr(5, 10) %>% parse_number()
  }
  if (is_empty(angry) | is_empty(likes)) angry <- 0
  # comments
  comments <- (ps %>% html_nodes("._4bl7") %>% html_text())[4] %>% parse_number()
  # shares
  shares <- ps %>% html_node(".UFIShareLink") %>% html_text()
  shares <- gsub(pattern = ",", ".", shares)
  coef <- ifelse(grepl("тыс", shares), 1000, 1)
  shares <- shares %>% parse_number()
  shares <- shares * coef
  df <- data_frame(post_link, like, love, wow, haha, sad, angry, comments, shares)
  print(likes)
  df[!duplicated(df$post_link), ]
}

get_count <- function(userlink) {
  remDr$navigate(userlink)
  ps <- read_html(remDr$getPageSource()[[1]])
  user <- ps %>% html_node(css = "._2nlw._2nlv") %>% html_text()
  subscribers <- ps %>% html_node(css = ".uiList._3-8x._2pic._4kg") %>% html_text() %>% gsub(pattern = ".*Подписчики:", replacement = "") %>% stringr::str_replace_all("[[:blank:]]", "") %>% readr::parse_number()
  # friends <- ps %>% html_nodes(css = ".clearfix._3-8t._2pi4") %>% html_text() %>% paste(collapse = " ") %>% stringr::str_replace_all("[[:blank:]]","") %>% readr::parse_number()
  cat(user)
  data_frame(user, userlink, subscribers)
}

psconnect <- safely(DBI::dbConnect)

connect_sql <- function() {
  con <- psconnect(RPostgres::Postgres(),
                   host   = "fb.cen4gigyi8eb.us-east-2.rds.amazonaws.com",
                   dbname = "fb",
                   user      = "romankyrychenko",
                   password      = "?Usosrskvs29",
                   port     = 5555)
  
  while(is.null(con$result)) {
    con <- psconnect(RPostgres::Postgres(),
                     host   = "fb.cen4gigyi8eb.us-east-2.rds.amazonaws.com",
                     dbname = "fb",
                     user      = "romankyrychenko",
                     password      = "?Usosrskvs29",
                     port     = 5555)
  }
  
  con$result
}

con <- connect_sql()

dbGetInfo <- safely(dbGetInfo)

check_sql <- function(con) {
  is.null(dbGetInfo(con)$result)
}

fet <- safely(RPostgres::dbFetch)

query <- function(con) {
  res <- fet(RPostgres::dbSendQuery(con, "SELECT post_link FROM fb"))
  
  while(is.null(res$result)) {
    if(check_sql(con)) con <- connect_sql()
    res <- fet(RPostgres::dbSendQuery(con, "SELECT post_link FROM fb"))
  }
  res$result
}

wt <- safely(RPostgres::dbWriteTable)

wtable <- function(con){
  con <- connect_sql()
  res <- wt(con, "fb", request, append = T)
  while(is.null(res$result)) {
    con <- connect_sql()
    res <- wt(con, "fb", request, append = T)
  }
}


interruptor = function(FUN,args, time.limit, ALTFUN){
  results <- NULL
  results <- withTimeout({FUN(args)},timeout=time.limit,onTimeout="silent")
  if(is.null(results)){
    results <- ALTFUN(args)
  }
  return(results)
}   

post <- 0

#withTimeout({table(5)},timeout=5,onTimeout="silent")

