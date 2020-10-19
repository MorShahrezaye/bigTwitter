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

# source("R/shared.R")
# auth_df <- read.keys("/mnt/nvme0/Dropbox/USG/research/corona/python/keys.csv")
# screen_name_list <- c("stbrandner")
# include_entities = T ;sleepTime = 1; verbose = T; user_id_list = NULL; jsonpath = "./"
#' @export
getTweets <- function(screen_name_list = NULL, user_id_list = NULL, include_entities = T , auth_df, sleepTime = 30, verbose = F, jsonpath = "./"){
  require(itertools)
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

  url <- "https://api.twitter.com/1.1/statuses/user_timeline.json"

  pb <- utils::txtProgressBar(min = 0, max = length(items), style = 3)
  counter <- 0
  results <- list()

  for(user in items){
    if(verbose)
      message("\nstarting ", user)
    if(use_screen_name){
      q <- c(screen_name=user, include_entities=it, count=200, include_rts="true");
    } else {
      q <- c(user_id=user, include_entities=it, count=200, include_rts="true");
    }

    RESPONSE <- list(max_id = "-1")
    while(RESPONSE$max_id != "0"){
      # q["cursor"] <- RESPONSE$next_cursor_str
      if(verbose)
        message("cursor at ", RESPONSE["max_id"])
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
          RESPONSE$max_id <- "0"
          break
        }
      }
      if(current!="[]"){
        if(!continue<0){
          TRY <- try({
            current_list <- RJSONIO::fromJSON(iconv(current, "latin1", "ASCII", "byte"))
            nextID <- min(unlist(lapply(current_list, function(x) x$id)))
          }, silent = TRUE)
          if(class(TRY)=="try-error"){
            nextID = min(stringr::str_extract(stringr::str_extract_all(current, '[^"retweeted_status":]\\{\"created_at\".*?,\"id_str\"')[[1]], '(?<=id\":).*?(?=,\"id_str\")'))
          }
          saveRDS(current, paste0(jsonpath, "/", user,"_",nextID,".rds"))
          if(is.na(q["max_id"])) q["max_id"]<- "1"
          if(q["max_id"]==nextID){
            RESPONSE$max_id <- "0"
          } else {
            q["max_id"] <- nextID
            RESPONSE$max_id <- nextID
          }
        }
      } else {
        RESPONSE$max_id <- "0"
      }
    }
    if(verbose){
      if(continue<0){
        message("failed ", user, "\n")
      } else {
        message("finished ", user, "\n")
      }
    }
    counter <- counter+1
    setTxtProgressBar(pb, counter)
  }
  close(pb)
}

# source("R/shared.R")
# auth_df <- read.keys("/mnt/nvme0/Dropbox/USG/research/corona/python/keys.csv")
# ids = c("1009054456778981376", "378780881545998336", "1256288574313553922", "1250091976487317507"); sleepTime = 1; verbose = T
#' @export
getRetweets <- function(ids= NULL, auth_df, sleepTime = 30, verbose = F){
  require(itertools)
  require(utils)
  options(scipen=999)

  if(is.null(ids))
    stop("A list of Tweet ids are required.")

  iterKeys <- itertools::recycle(t(auth_df), 10^6)
  key <- iterators::nextElem(iterKeys)
  auth <- key2auth(key)

  url <- "https://api.twitter.com/1.1/statuses/retweeters/ids.json"

  pb <- utils::txtProgressBar(min = 0, max = length(ids), style = 3)
  counter <- 0
  results <- list()

  for(id in ids){
    if(verbose)
      message("\nstarting ", id)
    RESPONSE <- list(cursor = -1)
    results[id] = c()
    while(RESPONSE$cursor != "-2" & RESPONSE$cursor != "0"){
      q <- c(id=id, count=100, cursor=RESPONSE$cursor)
      if(verbose)
        message("cursor at ", RESPONSE["cursor"])
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
          RESPONSE$max_id <- "0"
          break
        }
      }
      if(current!="[]" & !grepl('"ids\":\\[\\]', current)){
        if(!continue<0){
          current_list <- RJSONIO::fromJSON(current)
          results[[id]] <- c(results[[id]], unlist(current_list$ids))
          RESPONSE$cursor <- current_list$next_cursor_str
        }
      } else {
        RESPONSE$cursor <- "-2"
      }
    }
    if(verbose){
      if(continue<0){
        message("failed ", id, "\n")
      } else {
        message("finished ", id, "\n")
      }
    }
    counter <- counter+1
    setTxtProgressBar(pb, counter)
  }
  close(pb)
  return(results)
}

# source("R/shared.R")
# auth_df <- read.keys("/mnt/nvme0/Dropbox/USG/research/corona/python/keys.csv")
# ids = c("1219545223593975809", "1220793070477881345", "1221022394179080192", "1221081288251039746", "1221139383761457153"); sleepTime = 1; verbose = T
#' @export
showTweets <- function(ids = NULL , auth_df, sleepTime = 30, verbose = F, jsonpath = "./"){
  require(itertools)
  require(utils)
  options(scipen=999)

  if(is.null(ids))
    stop("ids should be set.")
  items <- ids
  items <- split(items, ceiling(seq_along(items)/100))

  iterKeys <- itertools::recycle(t(auth_df), 10^6)
  key <- iterators::nextElem(iterKeys)
  auth <- key2auth(key)

  url <- "https://api.twitter.com/1.1/statuses/lookup.json"

  pb <- utils::txtProgressBar(min = 0, max = length(items), style = 3)
  counter <- 0
  results <- list()

  for(tweet in items){
    if(verbose)
      message("starting next batch")
    ids_str <- paste0(tweet, collapse = ",")
    q <- c(id=ids_str, include_entities="true", map="true")

    continue <- TRUE
    while(continue){
      current <- twitter_api_call(url, q, auth)
      if(grepl("Rate limit exceeded|code\":32,\"message\":\"Could not authenticate you.", current)){
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
        message("failed ", tweet, "\n")
    } else {
      saveRDS(current, paste0(jsonpath, "/", as.numeric(Sys.time()),".rds"))
    }
    counter <- counter+1
    setTxtProgressBar(pb, counter)
  }
  close(pb)
  return(NULL)
}
