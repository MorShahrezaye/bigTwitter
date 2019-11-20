<h1>Description</h1>
This is a simple R package that is developed to enable researchers downloading data from Twitter API using multiple keys. This package is developed for research and teaching purposes.

The main advantages of this package over the other R packages are:
<ul>
  <li>Capability of using multiple Twitter keys! If you have 10 keys then you can download the data 10 times faster (using one line of code)!</li>
  <li>The functions return the complete output as a datatable object (other packages trim the data!)</li>
</ul>

There is no gurantee that the functions perform as intended and there might be bugs in the code! 

<h1>Sample use</h1>
Downloading the complete list of friends of a Twitter user

```{r, echo = FALSE}
#install and load the package
devtools::install_github("MorShahrezaye/bigTwitter", force = TRUE)
library(bigTwitteR)

#read the keys from the csv file
auth_df <- read.keys("keys.csv")

#define the query user
queryUser <- "MorShahrezaye"

#download the friendsof the query user
friends <- getFriendIDs(screen_name_list = queryUser, auth_df = auth_df, sleepTime = 1, verbose = T)

#downliad the friends of each friend of the query user (max 50k friends per user)
friendsLevel2 <- getFriendIDs(user_id_list = friends[[1]], auth_df = auth_df, sleepTime = 1, verbose = F, max_per_user = 50000)

#form the friendship network
g <- userEdgeList2graph(friends, friendsLevel2, friendsLevel3, directed = F)
```

<h1>Recommended urls</h1>
1- <b>Twitter API documentation</b>: https://developer.twitter.com/en/docs</br>
2- <b>A tutorial to <i>rtweet</i> package</b>: https://mkearney.github.io/nicar_tworkshop/#1</br>
3- <b>A tool to visualize huge networks efficiently</b>: https://gephi.org/users/download/
