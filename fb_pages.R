suppressPackageStartupMessages(require(Rfacebook))
suppressPackageStartupMessages(require(purrr))
suppressPackageStartupMessages(require(dplyr))

pages <- c(
  "gennadykernes","Nikolay.Azarov","yatsenyuk.arseniy","SaakashviliMikheil","gordondmitry","YaShevchenkoUa","komarovskiynet","Dmitriy.Chekalkin",
  "viktoriya.voytsitska","o.danyliuk","Iryna.Friz","andriy.sadovyi","OleksandrVilkul","andriipyvovarskyi","ulanasuprun","petroporoshenko","volodymyrgroysman",
  "PavloPetrenko.official","president.ukraine","merkieva","YuliaTymoshenko","d.karpachoff"
)

token <- "EAACEdEose0cBALclfPzhSVoKj3oImDz0oZCyZCbuU4xIigVMhxaZCQZCwIPyyu0iBZB8FpjFRO2fuP7j7wubiNSm3qwZAGRRQVZA5L5evygpAQ7ByvhayHlaeEGcYAz2uIBXChdq8sbrstZALoNZAsasWeVZCsNiT7HtWaw3D6zygSQQal3gu1kmyhmnf3vxxEsXwZD"

users <- map_dfr(pages, function(x) getPage(x, token = token, since = strftime(Sys.Date()-60, format = "%Y/%m/%d"), until = strftime(Sys.Date(), format = "%Y/%m/%d"), n = 50000))

xlsx::write.xlsx(users, "fb_02032018.xlsx")

get_count("https://www.facebook.com/yevgeniy.murayev")
user_info <- purrr::map_dfr(persons, get_count)

pages_df <- users %>% as_data_frame() %>% mutate(
  user = from_name,  
  user_link = paste0("https://www.facebook.com/", from_id),
  post = message,
  post_link = paste0("https://www.facebook.com/", id),
  type = type,
  time = lubridate::ymd_hms(created_time, tz = "Europe/Kiev"), 
  location = NA,
  privacy = "Public",
  photos = ifelse(stringr::str_detect(link, "photo"), link, NA),
  likes_count = likes_count,
  like  = likes_count,
  love = 0,
  wow = 0,    
  haha = 0,     
  sad = 0,    
  angry = 0,   
  comments = comments_count,   
  shares = shares_count,
  attr_text = story,
  link_in_post = ifelse(stringr::str_detect(link, "photo"), NA, link),
  image_link = NA,
  app_name = NA,
  app_link = NA,
  repost_text = NA,
  repost_author = NA,
  repost_time = NA,
  download_time = Sys.time()
) %>% select(user, user_link, post, post_link, type, time, location, privacy, photos, likes_count, like,
              love,
              wow,
              haha,
              sad,
              angry ,
              comments,
              shares,
              attr_text,
              link_in_post, 
              image_link ,
              app_name,
              app_link,
              repost_text,
              repost_author,
              repost_time,
              download_time)

