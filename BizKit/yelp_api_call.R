
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

function(postcode, country, term="", limit=50) {
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
  
  output <- data.table(name=character(),
                       category=list(),
                       lat=numeric(),
                       long=numeric(),
                       price=character())
  
  ## Extract names
  name.data <- as.character(ct$businesses %>% map("name"))
  category.data <- vector("list", length(ct$businesses))
  
  for (i in 1:length(ct$businesses)) {
    category.data[[i]] <- ct$businesses[[i]]$categories %>% map("title")
  }
  
  lat.data <- as.numeric(ct$businesses %>% map("coordinates") %>% map("latitude"))
  long.data <- as.numeric(ct$businesses %>% map("coordinates") %>% map("longitude")) 
  price.data <- as.character(ct$businesses %>% map("price"))
  rating.data <- as.numeric(ct$businesses %>% map("rating"))
  
  output <- data.table(name=name.data,
                       category=c(category.data),
                       lat=lat.data,
                       long=long.data,
                       price=price.data,
                       rating=rating.data)
  
  return(output)
  
}
