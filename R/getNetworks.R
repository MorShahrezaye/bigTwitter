#' @export
getFollowerIDs <- function(screen_name_list = NULL, user_id_list = NULL, auth_df, sleepTime = 30, verbose = F){
  # require(itertools)
  require(iterators)
  # require(utils)
  options(scipen=999)

  if(is.null(screen_name_list) & is.null(user_id_list))
    stop("Either of screen_name_list or user_id_list parameter should be set.")
  if(is.null(screen_name_list)){
    items <- user_id_list
    use_screen_name <- F
  } else {
    items <- screen_name_list
    use_screen_name <- T
  }

  iterKeys <- itertools::recycle(t(auth_df), 10^6)
  key <- iterators::nextElem(iterKeys)
  auth <- key2auth(key)

  url <- "https://api.twitter.com/1.1/followers/ids.json"

  pb <- utils::txtProgressBar(min = 0, max = length(items), style = 3)
  counter <- 0
  results <- list()

  for(user in items){
    if(verbose)
      message("starting ", user)
    if(use_screen_name){
      q <- c(cursor=-1, screen_name=user, count=5000);
    } else {
      q <- c(cursor=-1, user_id=user, count=5000);
    }

    RESPONSE <- list(response = NULL, next_cursor_str = "-1")
    while(RESPONSE$next_cursor_str != "0"){
      q["cursor"] <- RESPONSE$next_cursor_str
      if(verbose)
        message("cursor at ", q["cursor"])
      continue <- TRUE
      while(continue){
        current <- twitter_api_call(url, q, auth)
        if(grepl("Rate limit exceeded", current)){
          if(verbose)
            message("Rate limit exceeded. Sleeping for ", sleepTime, " seconds")
          Sys.sleep(sleepTime)
          if(verbose)
            message("Activating next key.... ")
          key <- iterators::nextElem(iterKeys)
          auth <- key2auth(key)
        } else {
          continue <- FALSE
        }
        if(grepl("Sorry, that page does not exist.|Not authorized|error", current)){
          if(verbose)
            message(current)
          continue <- -1
          RESPONSE$next_cursor_str <- "0"
          break
        }
      }
      if(!continue<0){
        current <- RJSONIO::fromJSON(current)
        RESPONSE$response <- c(RESPONSE$response, as.character(current$ids))
        RESPONSE$next_cursor_str <- as.character(current$next_cursor)
      }
    }
    if(continue<0){
      if(verbose)
        message("failed ", user, "\n")
    } else {
      results[user] <- list(RESPONSE$response)
      if(verbose)
        message("finished ", user, "\n")
    }
    counter <- counter+1
    setTxtProgressBar(pb, counter)
  }
  close(pb)
  message("out of ", length(items), ", ", length(results), " got downloaded")
  return(results)
}

#' @export
getFriendIDs <- function(screen_name_list = NULL, user_id_list = NULL, auth_df, sleepTime = 30, verbose = F){
  # require(itertools)
  # require(iterators)
  # require(utils)
  options(scipen=999)

  if(is.null(screen_name_list) & is.null(user_id_list))
    stop("Either of screen_name_list or user_id_list parameter should be set.")
  if(is.null(screen_name_list)){
    items <- user_id_list
    use_screen_name <- F
  } else {
    items <- screen_name_list
    use_screen_name <- T
  }

  iterKeys <- itertools::recycle(t(auth_df), 10^6)
  key <- iterators::nextElem(iterKeys)
  auth <- key2auth(key)

  url <- "https://api.twitter.com/1.1/friends/ids.json"

  pb <- utils::txtProgressBar(min = 0, max = length(items), style = 3)
  counter <- 0
  results <- list()

  for(user in items){
    if(verbose)
      message("starting ", user)
    if(use_screen_name){
      q <- c(cursor=-1, screen_name=user, count=5000);
    } else {
      q <- c(cursor=-1, user_id=user, count=5000);
    }

    RESPONSE <- list(response = NULL, next_cursor_str = "-1")
    while(RESPONSE$next_cursor_str != "0"){
      q["cursor"] <- RESPONSE$next_cursor_str
      if(verbose)
        message("cursor at ", q["cursor"])
      continue <- TRUE
      while(continue){
        current <- twitter_api_call(url, q, auth)
        if(grepl("Rate limit exceeded", current)){
          if(verbose)
            message("Rate limit exceeded. Sleeping for ", sleepTime, " seconds")
          Sys.sleep(sleepTime)
          if(verbose)
            message("Activating next key.... ")
          key <- iterators::nextElem(iterKeys)
          auth <- key2auth(key)
        } else {
          continue <- FALSE
        }
        if(grepl("Sorry, that page does not exist.|Not authorized|error", current)){
          if(verbose)
            message(current)
          continue <- -1
          RESPONSE$next_cursor_str <- "0"
          break
        }
      }
      if(!continue<0){
        current <- RJSONIO::fromJSON(current)
        RESPONSE$response <- c(RESPONSE$response, as.character(current$ids))
        RESPONSE$next_cursor_str <- as.character(current$next_cursor)
      }
    }
    if(continue<0){
      if(verbose)
        message("failed ", user, "\n")
    } else {
      results[user] <- list(RESPONSE$response)
      if(verbose)
        message("finished ", user, "\n")
    }
    counter <- counter+1
    setTxtProgressBar(pb, counter)
  }
  close(pb)
  message("out of ", length(items), ", ", length(results), " got downloaded")
  return(results)
}




