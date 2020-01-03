#' Downloading a single user or alist of users
#'
#' This function takes a single or a list of users (user id or user screen name) and downloads the last Tweet and informations on the user.
#' Either of screen_name_list or user_id_list must be provided
#'
#' @param screen_name_list A list of Twitter screen names
#' @param user_id_list A list of Twitter numerical ids (as character)
#' @param auth_df The dataframe containing the Twitter keys (check read.keys function)
#' @param sleepTime Sleep time (in seconds) when activating next key
#' @param verbose Monitor the process (recommended for debuging!)
#' @return A data.table of last Tweets of each user (all the columns are returned!)
#' @export
showUsers <- function(screen_name_list = NULL, user_id_list = NULL, include_entities = T , auth_df, sleepTime = 30, verbose = F){
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
  if(include_entities){
    it <- 'true'
  }else{
    it <- 'false'
  }

  iterKeys <- itertools::recycle(t(auth_df), 10^6)
  key <- iterators::nextElem(iterKeys)
  auth <- key2auth(key)

  url <- "https://api.twitter.com/1.1/users/show.json"

  pb <- utils::txtProgressBar(min = 0, max = length(items), style = 3)
  counter <- 0
  results <- list()

  for(user in items){
    if(verbose)
      message("starting ", user)
    if(use_screen_name){
      q <- c(screen_name=user, include_entities=it);
    } else {
      q <- c(user_id=user, include_entities=it);
    }

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
        break
      }
    }
    if(continue<0){
      if(verbose)
        message("failed ", user, "\n")
    } else {
      results[[user]] <- current
      if(verbose)
        message("finished ", user, "\n")
    }
    counter <- counter+1
    setTxtProgressBar(pb, counter)
  }
  close(pb)
  message("out of ", length(items), ", ", length(results), " got downloaded")
  results.df <- lapply(results, function(x) data.table::as.data.table(t(unlist(RJSONIO::fromJSON(x)))))
  results.df <- data.table::rbindlist(results.df, fill = T)
  return(results.df)
}

#' @export
searchUsers <- function(query = NULL, include_entities = T , count = 100, auth_df, sleepTime = 30, verbose = F){
  require(itertools)
  require(iterators)
  require(utils)
  options(scipen=999)

  if(is.null(query))
    stop("the query parameter should be set.")
  if(include_entities){
    it <- 'true'
  }else{
    it <- 'false'
  }

  iterKeys <- itertools::recycle(t(auth_df), 10^6)
  key <- iterators::nextElem(iterKeys)
  auth <- key2auth(key)

  url <- "https://api.twitter.com/1.1/users/search.json"

  pages <- ceiling(count/20)
  pb <- utils::txtProgressBar(min = 0, max = pages, style = 3)
  counter <- 0
  results <- list()

  for(page in 1:pages){
    if(verbose)
      message("starting ", page)
    q <- c(q=query, include_entities=it, count=20, page=page);

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
        break
      }
    }
    if(continue<0){
      if(verbose)
        message("failed ", user, "\n")
    } else {
      results[[user]] <- current
      if(verbose)
        message("finished ", user, "\n")
    }
    counter <- counter+1
    setTxtProgressBar(pb, counter)
  }
  close(pb)
  message("out of ", length(items), ", ", length(results), " got downloaded")
  results.df <- lapply(results, function(x) data.table::as.data.table(t(unlist(RJSONIO::fromJSON(x)))))
  results.df <- data.table::rbindlist(results.df, fill = T)
  return(results.df)
}

