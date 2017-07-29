
# LIBRARIES
library (httr)
library (purrr)
library (dplyr)
# https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
# library (jsonlite)

# API DOCO: https://www.yelp.com/developers/documentation/v3/business_search

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

function(postcode, country, term) {
  limit <- 10
  location <- paste0(postcode,", ",country)
  url <- modify_url(yelp, path=c("v3","businesses","search"),
                    query=list(term=term,location=location,limit=limit))
  response <- GET (url, add_headers('Authorization' = paste("bearer", token)))
  
  # show request status
  http_status(response)
  
  # get response content in list of lists (converted from JSON)
  # result is a list of list, use length in a while loop to get individual elements,
  # or map_df as seen below to get individual attributes from elements.
  ct <- content(response)
  
  return(ct)
  
}


