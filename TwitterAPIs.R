install.packages("rtweet")
install.packages("httpuv")
install.packages("reactable")
install.packages("glue")
install.packages("stringr")



library(rtweet)
library(httpuv)

getwd()

x = create_token(consumer_key = , 
                 consumer_secret = ,
                 access_token = ,
                 access_secret = 
                   )

twitter_tokens <- c(
  create_token(app = "aaa", consumer_key = "xxx1", consumer_secret = "yyy1"),
  create_token(app = "bbb", consumer_key = "xxx2", consumer_secret = "yyy2"),
  create_token(app = "ccc", consumer_key = "xxx3", consumer_secret = "yyy3"),
  create_token(app = "ddd", consumer_key = "xxx4", consumer_secret = "yyy4"))