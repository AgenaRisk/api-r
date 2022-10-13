library(httr)

###### LOGIN AND AUTHENTICATION (POST credentials and refresh auth token)


#test account
username = "erhanpisirir@gmail.com"
password = "erhan_test"

login <- function(username, password){
  
  auth_endpoint <- "https://auth.agena.ai/realms/cloud/protocol/openid-connect/token"
  body <- list(client_id = "agenarisk-cloud",
               username = username,
               password = password,
               grant_type = "password")
  
  response <- POST(auth_endpoint, body = body, encode = "form")
  
  #if status == 200
  access_token <- content(response)$access_token
  refresh_token <- content(response)$refresh_token
  access_expire <- content(response)$expires_in
  refresh_expire <- content(response)$refresh_expires_in

 }


###### CALCULATION (POST model json and GET results)

