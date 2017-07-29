
# LIBRARIES
library ("httr")
library ("purrr")
library ("dplyr")

# CREDENTIALS: yelp login credentials
yelp = "https://api.yelp.com"
grant.type = "client_credentials"
client.id  = "zHibzyg_jQFOS7qDAEMGZg"
client.secret = "sWWdsGIfK2By69WCvBkvhkv5mIXA8DWtcUVcM4HDHGS7pGHefgZqiHfXXAUC12z1"

# AUTHENTICATION
yelp.res = POST("https://api.yelp.com/oauth2/token",
            body = list(grant_type = grant.type,
                        client_id = client.id,
                        client_secret = client.secret))

token = content(yelp.res)$access_token


# QUERY
term = "coffee"
location = "Vancouver, BC"
limit = 3
(url =
    modify_url (yelp, path = c("v3", "businesses", "search"),
               query = list(term = term, location = location, limit = limit)))
res = GET (url, add_headers('Authorization' = paste("bearer", token)))

# show request status
http_status(res)

# get response content
ct = content(res)

# show just name and phone
ct$businesses %>% 
  map_df(`[`, c("name", "phone"))



