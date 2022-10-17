library(httr)


###### LOGIN AND AUTHENTICATION (POST credentials and refresh auth token)

login <- function(username, password){
  
  auth_endpoint <- "https://auth.agena.ai/realms/cloud/protocol/openid-connect/token"
  body <- list(client_id = "agenarisk-cloud",
               username = username,
               password = password,
               grant_type = "password")
  
  response <- POST(auth_endpoint, body = body, encode = "form")
  login_time <- as.integer(Sys.time())
  
  return(list(response,login_time))

 }


check_auth <- function(login) {
  
  #if status == 200
  
  # access_token <- content(login[[1]])$access_token
  # refresh_token <- content(login[[1]])$refresh_token
  
  login_time <- login[[2]]
  access_duration <- content(login[[1]])$expires_in
  access_expire <- login_time + access_duration
  
  refresh_duration <- content(login[[1]])$refresh_expires_in
  refresh_expire <- login_time + refresh_duration
  
  if (as.integer(Sys.time()) < access_expire){
    status_check <- 0
  } 
  if (as.integer(Sys.time()) > access_expire && as.integer(Sys.time()) < refresh_expire) {
    status_check <- 1
  } 
  if (as.integer(Sys.time() > refresh_expire)){
    status_check <- 2
  }
  
  return(status_check)
  
}

refresh_auth <- function(cur_login){
  #keep in mind refresh token will change now
  
  auth_endpoint <- "https://auth.agena.ai/realms/cloud/protocol/openid-connect/token"
  cur_refresh_token <- content(cur_login[[1]])$refresh_token
  body <- list(client_id = "agenarisk-cloud",
               refresh_token = cur_refresh_token,
               grant_type = "refresh_token")
  
  response <- POST(auth_endpoint, body = body, encode = "form")
  login_time <- cur_login[[2]]
  #refresh_time <- as.integer(Sys.time())
  
  return(list(response,login_time))
}

###### CALCULATION (POST model json and GET results)

calc_model <- function(input_model, cur_login){
  model_to_send <- generate_cmpx(input_model)
  
  calculate_endpoint <- "https://api.agena.ai/public/v1/calculate"
  body <- list("sync-wait" = "true",
               "model" = model_to_send$model)
  access_token <- content(cur_login[[1]])$access_token
  
  
  response <- POST(calculate_endpoint, body = body,
                   add_headers("Authorization" = paste("Bearer",access_token)),
                   encode = "json", accept_json())
}

calculate <- function(input_model, login) {
  
  if (check_auth(login) == 2){
    cat("Authentication expired, please log in again")
    break
  }
  if (check_auth(login) == 1){
    new_login <- refresh_auth(login)
    response <- calc_model(input_model, new_login)
  }
  if (check_auth(login) == 0){
    response <- calc_model(input_model, login)
  }
  
    return(response)
}





