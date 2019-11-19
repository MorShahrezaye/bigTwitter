#' @export
read.keys <- function(file){
  if(!file.exists(file))
    stop("File does not exist")
  if(!grepl("csv", file))
    stop("The file format must be csv. Check the folloing link for more information.\nhttps://en.wikipedia.org/wiki/Comma-separated_values#Example")
  message("Make sure the csv file has no header (no column names) and the order of the columns should be as following:\noauth_consumer_key, consumer_secret, oauth_token, oauth_token_secret")
  keys <- read.csv(file)
  if(ncol(keys)!=4)
    stop("It seems the csv file does not contain 4 columns")
  colnames(keys) <- c("oauth_consumer_key", "consumer_secret", "oauth_token", "oauth_token_secret")
  return(keys)
}

twitter_api_call <- function (url, api, params, print_result = FALSE, use_cygwin = FALSE,
                              cygwin_bash = "c:\\cygwin64\\bin\\bash.exe", print_cmd = FALSE,
                              test = FALSE)
{
  # library(jsonlite)
  # library(RCurl)
  if (is.na(params["oauth_timestamp"])) {
    params["oauth_timestamp"] <- as.character(as.integer(Sys.time()))
  }
  if (is.na(params["oauth_nonce"])) {
    params["oauth_nonce"] <- sprintf("%d%s", as.integer(Sys.time()),
                                     paste(floor(runif(6) * 10^6), collapse = ""))
  }
  if (test) {
    test_data <- oauth1_signature(method = "GET", url, api,
                                  params, test = TRUE)
    params["oauth_signature"] <- test_data[["signature_escaped"]]
  }
  else {
    params["oauth_signature"] <- oauth1_signature(method = "GET",
                                                  url, api, params, test = FALSE)
  }
  httpheader <- c(Authorization = sprintf(paste(c("OAuth oauth_consumer_key=\"%s\", oauth_nonce=\"%s\", oauth_signature=\"%s\", ",
                                                  "oauth_signature_method=\"%s\", oauth_timestamp=\"%s\", oauth_token=\"%s\", oauth_version=\"1.0\""),
                                                collapse = ""), params["oauth_consumer_key"], params["oauth_nonce"],
                                          params["oauth_signature"], params["oauth_signature_method"],
                                          params["oauth_timestamp"], params["oauth_token"], params["oauth_version"]))
  q <- paste(paste(names(api), api, sep = "="), collapse = "&")
  urlq <- paste(url, q, sep = "?")
  if (!test) {
    if (!use_cygwin) {
      result <- getURL(urlq, httpheader = httpheader)
    }
    else {
      httpheader_escaped <- sprintf("Authorization: %s",
                                    gsub("\"", "\"", httpheader["Authorization"]))
      cmd <- sprintf("%s -c \"/usr/bin/curl --silent --get '%s' --data '%s' --header '%s'\"",
                     cygwin_bash, url, q, httpheader_escaped)
      if (print_cmd) {
        cat(cmd)
      }
      result <- system(cmd, intern = TRUE)
    }
  }
  else {
    result <- "{}"
  }
  if (print_result) {
    cat(prettify(result))
  }
  if (test) {
    test_data[["httpheader"]] <- httpheader
    test_data[["q"]] <- q
    test_data[["urlq"]] <- urlq
    return(test_data)
  }
  return(result)
}

key2auth <- function(key){
  auth <- c(
    "oauth_consumer_key"     = key[1],
    "oauth_nonce"            = NA,
    "oauth_signature_method" = "HMAC-SHA1",
    "oauth_timestamp"        = NA,
    "oauth_token"            = key[3],
    "oauth_version"          = "1.0",
    "consumer_secret"        = key[2],
    "oauth_token_secret"     = key[4]
  )
  return(auth)
}

oauth1_signature <- function(method, url, api, params, test=FALSE) {
  # library(RCurl);
  # library(digest);
  # library(base64enc);

  # http://oauth.net/core/1.0/#encoding_parameters

  if(curlEscape(".") == ".") {
    # that's how we want it!
  } else if(curlEscape(".") == "%2E") {
    curlEscape <- function(str) {
      esc <- RCurl::curlEscape(str)
      esc <- gsub("%2E",".",esc)
      esc <- gsub("%2D","-",esc)
      esc <- gsub("%5F","_",esc)
      esc <- gsub("%7E","~",esc)
    }
  } else {
    stop("curlEscape('.') is supposed to be either '.' or '%2E'")
  }

  secrets <- params[c("consumer_secret","oauth_token_secret")];

  # exclude *_secret
  params <- params[! names(params) %in% c("consumer_secret","oauth_token_secret")]

  params <- c(api, params);
  params_sorted <- params[sort(names(params))];
  params_escaped <- sapply(params_sorted, curlEscape);
  pstr <- paste(paste(names(params_escaped),"=",params_escaped,sep=""),collapse="&");

  # it is important to use curlEscape instead of URLencode because the latter encodes
  # using lower case letters but we need upper case letters. Otherwise the resulting
  # signature will be different from what Twitter/OAuth expects/calculates.
  final <- sprintf("%s&%s&%s",toupper(method), curlEscape(url), curlEscape(pstr));

  sig <- sprintf("%s&%s",curlEscape(secrets["consumer_secret"]),curlEscape(secrets["oauth_token_secret"]));

  hmac <- hmac(sig,final,algo="sha1",raw=TRUE)
  signature <- base64encode(hmac);
  signature_escaped <- curlEscape(signature);

  if(test) {
    return(
      list(
        params_escaped = params_escaped,
        pstr = pstr,
        final = final,
        sig = sig,
        hmac = hmac,
        signature = signature,
        signature_escaped = signature_escaped
      )
    )
  }

  return(signature_escaped);
}
