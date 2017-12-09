library(httr)

key <- "7761ca6c1a3b6881407f"
secret <- "16acd176ba8cc0bae165802476e7f12b10ba5549"

myapp <- oauth_app("github",
                   key = key,
                   secret = secret)

github_token <- oauth2.0_token(oauth_endpoints("github"),myapp)

gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)

json1 = content(req)
json2 = jsonlite::fromJSON(jsonlite::toJSON(json1))
json2$created_at[json2$name == "datasharing"]

#Q5
tmp <- read.fwf("Q5.for",widths = c(15,4,4,5,4,4,5,4,4,5,4,4),skip = 4)