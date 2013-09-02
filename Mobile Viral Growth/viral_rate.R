require(RPostgreSQL)
library(chron)
library(plyr)
library(forecast)
library(inline)
library(plyr)
library(reshape2)
library(ggplot2)
#fetchQuery('GRANT ALL PRIVILEGES ON table_name to egregory', user = 'gploader', password = 'gploader')
#' @param user your username for the greenplum database
#' @param password your password
#' @param dbnam, default "prod"
#' @param host default "gp.tagged.com"
#' @param port port of the database on the host
#' @param driver the type of database driver to use
makeCxn <- function(user = "egregory", password = "egregory",
                    dbname = "prod", host = "gp.tagged.com", 
                    port = 5432, driver = "PostgreSQL") {
  drv <- dbDriver(driver)
  cxn <- dbConnect(drv, user = user, password = password,
                   dbname = dbname, host = host, port = port)
  cxn
}
#' @param query the query you want to make to the SQL connection you've specified
#' @param user your username for the greenplum database
#' @param password your password
#' @param dbnam, default "prod"
#' @param host default "gp.tagged.com"
#' @param port port of the database on the host
#' @param driver the type of database driver to use
fetchQuery <- function(query, user = "egregory", password = "egregory",
                       dbname = "prod", host = "gp.tagged.com", 
                       port = 5432, n = -1, verbose = TRUE,
                       split = TRUE) {
  res <- list()
  cxn <- makeCxn(user = user, password = password,
                 dbname = dbname, host = host, port = port)
  t1 <- Sys.time()
  queries <- query
  if (split == TRUE) {
    queries <- strsplit(query, ";", fixed = TRUE)[[1]] # Split the query into components
  }
  for (item in queries) {
    if(verbose) {
      cat(paste(item, '\n'))
    }
    tmp <- try(dbSendQuery(cxn, query)) # send the query
    if ('try-error' %in% class(tmp)) {
      res[[item]] <- dbGetException(cxn)
      next
    }
    type <- tolower(substring(gsub(" ", "", item), 0, 6)) # identify if select, insert, delete
    if (type == "select" | grepl("with..", type) | grepl('EXPLAI|explai', type) | !split) {
      res[[item]] <- try(fetch(tmp, n))
    }
    else {
      res[[item]] <- dbGetRowsAffected(tmp)
      cat(res[[item]])
    }
    if (verbose) {
      print(Sys.time() - t1)
      if (!is.null(dim(res))) {
        print(dim(res))
      }
    }
    dbClearResult(tmp)
  }
  dbDisconnect(cxn)
  if (length(res) == 1) {
    res <- res[[1]]
  }
  res
}


queryByDateRange <- function(query, min.date = as.character(Sys.Date() - 1), max.date = NULL) {
  if (is.null(max.date)) {
    max.date <- min.date
  }
  dates <- as.character(seq(as.Date(min.date), as.Date(max.date) + 1, 'day'))
  N <- length(dates)
  res <- list()
  for (i in 1:(N - 1)) {
    str.start <- paste("DATE('", dates[i], "') ", sep = "")
    str.end <- paste("DATE('", dates[i + 1], "') ", sep = "")
    tmp.query <- gsub(":start_date", str.start, query, fixed = TRUE)
    tmp.query <- gsub(":end_date", str.end, tmp.query, fixed = TRUE)
    res[[dates[i]]] <- fetchQuery(tmp.query, split = FALSE)
  }
  res
}


# Table to store number of messages sent on mobile, per day, per user
"CREATE TABLE sandbox.eg_mobile_messagers (
dt timestamp,
user_id bigint,
messages_sent int
)
" -> query.create_messagers
fetchQuery(query.create_messagers)


# Make sure this actually fills mobile messagers and the number of messages they sent, 1 day at a time
"
INSERT INTO sandbox.eg_mobile_messagers 
SELECT DATE(m.dt) as dt, u.user_id, COUNT(u.user_id) as messages_sent
FROM userdata_light u, login_detail_log l, message_log m
WHERE u.user_id = l.user_id
AND l.user_id = m.from_user_id
AND l.session_id = m.session_id
AND m.sub_type = 'send'
AND l.ua LIKE 'Tagged%'
AND l.dt >= :start_date
AND l.dt < :end_date
AND m.dt >= :start_date
AND m.dt < :end_date
GROUP BY m.dt, u.user_id
" -> query.fill_mobile_messagers

queryByDateRange(query.fill_mobile_messagers, "2013-04-01", "2013-06-01") # Fill messages sent per day, per user, on the mobile platform


# Table to represent users who satisfy your criteria, by day
"CREATE TABLE sandbox.eg_simon_mobile_criteria (
user_id bigint, 
dt date
)" -> query.create_mobile_criteria
fetchQuery(query.create_mobile_criteria)


# Make sure this really represents users who satisfy your criteria on day "n"
"INSERT INTO sandbox.eg_simon_mobile_criteria (
SELECT user_id, dt
FROM (
SELECT user_id, :start_date as dt, sum(messages_sent) as messages_sent, COUNT(DISTINCT dt) as days_messaged
FROM sandbox.eg_mobile_messagers mm
WHERE mm.dt >= :start_date - INTERVAL '29 days'
AND mm.dt < :start_date
AND user_id IN (SELECT user_id FROM sandbox.eg_mobile_messagers WHERE dt = :start_date)
GROUP BY user_id, :start_date
) a
WHERE messages_sent >= 10
AND days_messaged >= 5
)" -> query.fill_mobile_criteria
queryByDateRange(query.fill_mobile_criteria, "2013-05-01", "2013-06-01")


users_per_day <- fetchQuery("
                            WITH user_ranks AS (
                            SELECT user_id, dt, row_number() OVER (PARTITION BY user_id ORDER BY dt ASC) as rank
                            FROM sandbox.eg_simon_mobile_criteria
                            ) SELECT dt, COUNT(*) 
                            FROM user_ranks
                            WHERE rank = 1
                            GROUP BY dt 
                            ")

p <- ggplot(users_per_day) + geom_line(aes( x = dt, y = count))
print(p)