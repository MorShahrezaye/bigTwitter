#' Downloading the follower ids of a list of users
#'
#' This function takes a single or a list of users (user id or user screen name) and downloads the followers of each user.
#' Either of screen_name_list or user_id_list must be provided
#'
#' @param screen_name_list A list of Twitter screen names
#' @param user_id_list A list of Twitter numerical ids (as character)
#' @param auth_df The dataframe containing the Twitter keys (check read.keys function)
#' @param sleepTime Sleep time (in seconds) when activating next key
#' @param max_per_user Max number of followers to download per user
#' @param verbose Monitor the process (recommended for debuging!)
#' @param jsonPath Where to save the json files
#' @return A list of users and their followers
#' @export
getFollowerIDs <- function(screen_name_list = NULL, user_id_list = NULL, auth_df, sleepTime = 30,
                           max_per_user = 15000, verbose = F, jsonPath = NULL, namePattern = "fol-",
                           return_results = T){
  require(itertools)
  require(iterators)
  require(utils)
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
      message("\nstarting ", user)
    if(!file.exists(paste0(jsonPath, "/", namePattern, user, ".json"))){
      if(use_screen_name){
        q <- c(cursor=-1, screen_name=user, count=5000);
      } else {
        q <- c(cursor=-1, user_id=user, count=5000);
      }

      RESPONSE <- list(response = NULL, next_cursor_str = "-1")
      while(RESPONSE$next_cursor_str != "0" & (length(RESPONSE$response)+1)<=max_per_user){
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
          if(grepl("Sorry, that page does not exist.|Not authorized", current)){
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
          if(is.null(current$next_cursor))
            RESPONSE$next_cursor_str <- "0"
        }
      }
      if(continue<0){
        if(verbose)
          message("failed ", user, "\n")
      } else {
        if(!is.null(jsonPath)){
          l <- list(as.character(RESPONSE$response))
          names(l) <- user
          writeLines(rjson::toJSON(l), paste0(jsonPath, "/", namePattern, user, ".json"))
          if(return_results)
            results[user] <- list(RESPONSE$response)
        } else {
          results[user] <- list(RESPONSE$response)
        }
        if(verbose)
          message("finished ", user, "\n")
      }
    } else {
      if(verbose)
        message(user, " file already exists\n")
    }
    counter <- counter+1
    setTxtProgressBar(pb, counter)
  }
  close(pb)
  message("out of ", length(items), ", ", length(results), " got downloaded")
  if(is.null(jsonPath)){
    return(results)
  } else {
    if(return_results)
      return(results)
    return(T)
  }
}

#' Downloading the friends ids of a list of users
#'
#' This function takes a single or a list of users (user id or user screen name) and downloads the friends of each user.
#' Either of screen_name_list or user_id_list must be provided
#'
#' @param screen_name_list A list of Twitter screen names
#' @param user_id_list A list of Twitter numerical ids (as character)
#' @param auth_df The dataframe containing the Twitter keys (check read.keys function)
#' @param sleepTime Sleep time (in seconds) when activating next key
#' @param max_per_user Max number of friends to download per user
#' @param verbose Monitor the process (recommended for debuging!)
#' @param jsonPath Where to save the json files
#' @return A list of users and their friends
#' @export
getFriendIDs <- function(screen_name_list = NULL, user_id_list = NULL, auth_df, sleepTime = 30,
                         max_per_user = 15000, verbose = F, jsonPath = NULL, namePattern = "fri-",
                         return_results = T){
  require(itertools)
  require(iterators)
  require(utils)
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
      message("\nstarting ", user)
    try({
      if(!file.exists(paste0(jsonPath, "/", namePattern, user, ".json"))){
        if(use_screen_name){
          q <- c(cursor=-1, screen_name=user, count=5000);
        } else {
          q <- c(cursor=-1, user_id=user, count=5000);
        }

        RESPONSE <- list(response = NULL, next_cursor_str = "-1")
        while(RESPONSE$next_cursor_str != "0" & (length(RESPONSE$response)+1)<=max_per_user){
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
            if(grepl("Sorry, that page does not exist.|Not authorized", current)){
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
            if(is.null(current$next_cursor))
              RESPONSE$next_cursor_str <- "0"
          }
        }
        if(continue<0){
          if(verbose)
            message("failed ", user, "\n")
        } else {
          if(!is.null(jsonPath)){
            l <- list(as.character(RESPONSE$response))
            names(l) <- user
            writeLines(rjson::toJSON(l), paste0(jsonPath, "/", namePattern, user, ".json"))
            if(return_results)
              results[user] <- list(RESPONSE$response)
          } else {
            results[user] <- list(RESPONSE$response)
          }
          if(verbose)
            message("finished ", user, "\n")
        }
      } else {
        if(verbose)
          message(user, " file already exists\n")
      }
    })
    counter <- counter+1
    setTxtProgressBar(pb, counter)
  }
  close(pb)
  message("out of ", length(items), ", ", length(results), " got downloaded")
  if(is.null(jsonPath)){
    return(results)
  } else {
    if(return_results)
      return(results)
    return(T)
  }
}

#' Downloading the friends' and folowers' ids of a list of users
#'
#' This function takes a single or a list of users (user id or user screen name) and downloads the friends of each user.
#' Either of screen_name_list or user_id_list must be provided
#'
#' @param screen_name_list A list of Twitter screen names
#' @param user_id_list A list of Twitter numerical ids (as character)
#' @param auth_df The dataframe containing the Twitter keys (check read.keys function)
#' @param sleepTime Sleep time (in seconds) when activating next key
#' @param max_per_user Max number of friends to download per user
#' @param verbose Monitor the process (recommended for debuging!)
#' @param jsonPath Where to save the json files
#' @return A list of users and their friends
#' @export
getFriendFollowersIDs <- function(screen_name_list = NULL, user_id_list = NULL, auth_df, sleepTime = 30,
                                  max_per_user = 15000, verbose = F, jsonPath = NULL, namePatternFriends = "fri-",
                                  namePatternFollowers = "fol-"){
  require(itertools)
  require(iterators)
  require(utils)
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
  if(is.null(jsonPath))
    stop("json path must be set!")

  iterKeys <- itertools::recycle(t(auth_df), 10^6)
  key <- iterators::nextElem(iterKeys)
  auth <- key2auth(key)

  friends_url <- "https://api.twitter.com/1.1/friends/ids.json"
  followers_url <- "https://api.twitter.com/1.1/followers/ids.json"

  pb <- utils::txtProgressBar(min = 0, max = length(items), style = 3)
  counter <- 0
  results <- list()

  for(user in items){
    if(verbose)
      message("\nstarting ", user)
    try({
      RESPONSE_friends <- list(response = NULL, next_cursor_str = "-1")
      if(!file.exists(paste0(jsonPath, "/", namePatternFriends, user, ".json"))){
        if(use_screen_name){
          q <- c(cursor=-1, screen_name=user, count=5000);
        } else {
          q <- c(cursor=-1, user_id=user, count=5000);
        }
        while(RESPONSE_friends$next_cursor_str != "0" & (length(RESPONSE_friends$response)+1)<=max_per_user){
          q["cursor"] <- RESPONSE_friends$next_cursor_str
          if(verbose)
            message("cursor at ", q["cursor"])
          continue <- TRUE
          while(continue){
            current <- twitter_api_call(friends_url, q, auth)
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
            if(grepl("Sorry, that page does not exist.|Not authorized", current)){
              if(verbose)
                message(current)
              continue <- -1
              RESPONSE_friends$next_cursor_str <- "0"
              break
            }
          }
          if(!continue<0){
            current <- RJSONIO::fromJSON(current)
            RESPONSE_friends$response <- c(RESPONSE_friends$response, as.character(current$ids))
            RESPONSE_friends$next_cursor_str <- as.character(current$next_cursor)
            if(is.null(current$next_cursor))
              RESPONSE_friends$next_cursor_str <- "0"
          }
        }
        if(continue<0){
          if(verbose)
            message("failed ", user, "\n")
        } else {
          if(!is.null(jsonPath)){
            l <- list(as.character(RESPONSE_friends$response))
            names(l) <- user
            writeLines(rjson::toJSON(l), paste0(jsonPath, "/",namePatternFriends, user, ".json"))
          } else {
            results[user] <- list(RESPONSE_friends$response)
          }
          if(verbose)
            message("finished ", user, "\n")
        }
      } else {
        if(verbose)
          message(user, " file already exists\n")
      }
      #downloading followers
      RESPONSE_followers <- list(response = NULL, next_cursor_str = "-1")
      if(!file.exists(paste0(jsonPath, "/", namePatternFollowers, user, ".json"))){
        if(use_screen_name){
          q <- c(cursor=-1, screen_name=user, count=5000);
        } else {
          q <- c(cursor=-1, user_id=user, count=5000);
        }
        while(RESPONSE_followers$next_cursor_str != "0" & (length(RESPONSE_followers$response)+1)<=max_per_user){
          q["cursor"] <- RESPONSE_followers$next_cursor_str
          if(verbose)
            message("cursor at ", q["cursor"])
          continue <- TRUE
          while(continue){
            current <- twitter_api_call(followers_url, q, auth)
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
            if(grepl("Sorry, that page does not exist.|Not authorized", current)){
              if(verbose)
                message(current)
              continue <- -1
              RESPONSE_followers$next_cursor_str <- "0"
              break
            }
          }
          if(!continue<0){
            current <- RJSONIO::fromJSON(current)
            RESPONSE_followers$response <- c(RESPONSE_followers$response, as.character(current$ids))
            RESPONSE_followers$next_cursor_str <- as.character(current$next_cursor)
            if(is.null(current$next_cursor))
              RESPONSE_followers$next_cursor_str <- "0"
          }
        }
        if(continue<0){
          if(verbose)
            message("failed ", user, "\n")
        } else {
          if(!is.null(jsonPath)){
            l <- list(as.character(RESPONSE_followers$response))
            names(l) <- user
            writeLines(rjson::toJSON(l), paste0(jsonPath, "/", namePatternFollowers, user, ".json"))
          } else {
            results[user] <- list(RESPONSE_followers$response)
          }
          if(verbose)
            message("finished ", user, "\n")
        }
      } else {
        if(verbose)
          message(user, " file already exists\n")
      }
    }) -> TRY
    if(class(TRY)=="try-error")
      message("try-error")
    #dowsnloading friends
    counter <- counter+1
    setTxtProgressBar(pb, counter)
  }
  close(pb)
  message("out of ", length(items), ", ", length(results), " got downloaded")
  return(T)
}
