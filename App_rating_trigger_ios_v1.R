### This file has a series of utility functions, followed by the solution to ticket 24257 ###
### Annotated by a bunch of asterisks ###
### By Erik Gregory ###
### 2013-04-11 ###

library(multicore)
require(RPostgreSQL)
library(chron)
library(plyr)
library(forecast)
library(inline)
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

#'@author Rober McGehee
#'@source https://stat.ethz.ch/pipermail/r-sig-db/2010q3/000868.html
dbInsert <- function(con, name, value, row.names = TRUE, ...) {
  xx <- dbSendQuery(con, paste("select * from", name, "LIMIT 1;"))
  cols <- dbColumnInfo(xx)$name
  dbClearResult(xx)
  if (row.names) {
    if (!"row_names" %in% cols) stop("row_names column missing from
                                     ", sQuote(name))
    value[["row_names", exact=TRUE]] <- rownames(value)
  }
  if (length(setdiff(names(value), cols)))
    stop("names of 'value' do not match columns of ", sQuote(name))
  
  cdt  <- which(sapply(value, inherits, c("Date", "POSIXt")))
  ctxt <- which(sapply(value, postgresqlDataType)=="text")
  for (i in cdt)
    value[[i]] <- ifelse(is.na(value[[i]]), "NULL",
                         sQuote(format(value[[i]])))
  for (i in setdiff(ctxt, cdt))
    value[[i]] <- ifelse(is.na(value[[i]]), "NULL",
                         sQuote(value[[i]]))
  
  m <- as.matrix(value)
  class(m) <- "character"
  m[is.na(m)] <- "NULL"
  
  q1 <- paste("BEGIN; INSERT INTO", name, "(", paste(names(value),
                                                     collapse=", "), ") VALUES")
  q2 <- apply(m, 1, function(x) paste("(", paste(x, collapse=","),
                                      ")", sep=""))
  q3 <- "; COMMIT;"
  qry <- paste(q1, paste(q2, collapse=","), q3)
  dbGetQuery(con, qry)
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
                       port = 5432, n = -1, verbose = TRUE) {
  res <- NA
  cxn <- makeCxn(user = user, password = password,
                 dbname = dbname, host = host, port = port)
  t1 <- Sys.time()
  tmp <- try(dbSendQuery(cxn, query))
  if (!('try-error' %in% class(tmp))) {
    res <- try(fetch(tmp, n))
    if (verbose) {
      print(Sys.time() - t1)
      print(dim(res))
    }
  }
  dbDisconnect(cxn)
  res
}

#'@example {
#'a <- "CREATE TABLE sandbox.test_r_insert(user_id numeric(15, 0), dt date)"
#'tst <- fetchQuery(a)
#'datas <- data.frame(user_id = 0:10, dt = seq(as.Date('2012-01-01'), as.Date('2012-01-10'), "days"))}
insertData <- function(name, value, user = "egregory", password = "egregory",
                       dbname = "prod", host = "gp.tagged.com", 
                       port = 5432, n = -1, verbose = TRUE) {
  cxn <- makeCxn(user = user, password = password,
                 dbname = dbname, host = host, port = port)
  t1 <- Sys.time()
  res <- try(dbWriteTable(conn = cxn, name = name, value = value, row.names = FALSE, overwrite = TRUE))
  if (verbose) {
    print(Sys.time() - t1)
    print(dim(res))
  }
  dbDisconnect(cxn)
  res
}
getTables <- function(user = "egregory", password = "egregory",
                      dbname = "prod", host = "gp.tagged.com", 
                      port = 5432) {
  a <- makeCxn(user = user, password = password,
               dbname = dbname, host = host, port = port)
  tbls <- dbListTables(a)
  dbDisconnect(a)
  tbls
}
getFields <- function(tables, user = "egregory", password = "egregory",
                      dbname = "prod", host = "gp.tagged.com", 
                      port = 5432) {
  a <- makeCxn(user = user, password = password,
               dbname = dbname, host = host, port = port)
  tbls <- lapply(tables, function(i) try(dbListFields(a, i)))
  names(tbls) <- tables
  dbDisconnect(a)
  tbls
}

searchTables <- function(str) {
  all.tbls[grep(str, all.tbls)]
}

searchTbls <- function(str) {
  tbls[grep(str, tbls)]
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
    cat(tmp.query)
    res[[dates[i]]] <- fetchQuery(tmp.query)
  }
  res
}

addColumns <- function(df, outlier.thresh = 0.10, time.unit = "mins", min_dt = NULL) {
  if (!("POSIXct" %in% class(df$dt))) {
    df$dt <- as.POSIXct(df$dt)
  }
  df$weekday <- factor(weekdays(df$dt))
  df$minute <- factor(minutes(df$dt))
  df$hour <- factor((hours(df$dt) - 8) %% 24)
  df$minute_of_day <- factor(as.numeric(as.character(df$hour))*60 + as.numeric(as.character(df$minute)))
  df <- df[order(df$dt), ]
  if (df$dt[1] - df$dt[2] == structure(-1, tzone = "", units = "mins", class = "difftime")) {
    if ("median.value" %in% names(df)) { # If we already have median values
      
    }
    df <- ddply(df, c('weekday', 'hour', 'minute'), transform,
                median.value = median(value)) # Add medians for each weekday/minute combo
  }
  else {
    df <- ddply(df, c('weekday', 'hour'), transform,
                median.value = median(value)) # Add medians for each weekday/hour combo
  }
  df <- transform(df, dev = (value - median.value)/median.value)
  df$outlier <- ifelse(abs(df$dev) > outlier.thresh, TRUE, FALSE)
  if (is.null(min_dt)) {
    min_dt <- min(df$dt)
  }
  df$time_id <- round(difftime(df$dt, min_dt, unit = time.unit))
  df <- df[order(df$dt), ]
  df
}
# Oracle port number 1521
# dbname taganalysis
# user taganalysis
# password $taganalysis$
# host 10.15.40.120
# ana_metrics table
all.tbls <- getTables()
tbls <- gsub('[0-9]+', '', all.tbls)
tbls <- unique(tbls)

####### Ticket 24257 ######

# Bottom and top bounds on your time period of interest in which you want to look at user behavior
min_date <- "2013-04-7"
max_date <- "2013-04-14"
days_back <- 14 # Time period previous to the days above in which you want to consider iphone user messaging behavior
min_messages_sent <- 10 # Number of messages the user must have sent in the defined time period
min_messages_received <- 5 # Number of messages the user must have received in the defined time period
min_days_messages_sent <- 5 # Number of days the user must have sent at least one message during the defined time period


bottom_date <- as.character(as.Date(min_date) - days_back) # The absolute minimum date we want to pull users from

# Get user_ids of people who used iPhone during the week of interest.
query.drop.iphone_uid <- "DROP TABLE sandbox.iphone_uid"
fetchQuery(query.drop.iphone_uid) # Make sure there is a place to store iphone uids
query.create.iphone_uid <- "CREATE TABLE sandbox.iphone_uid (
                             user_id numeric(15, 0)
                            )"
fetchQuery(query.create.iphone_uid) # Create table

query.insert.iphone_uid <- "INSERT INTO sandbox.iphone_uid
                            SELECT DISTINCT m.user_id
                            FROM mobile_api_log m
                            WHERE NOT (m.mobile_type LIKE 'Tagged/%/an%' 
                              OR m.mobile_type LIKE 'Hi5/%/an%' 
                              OR m.mobile_type = 'Tagged/2.0+CFNetwork/459 Darwin/10.0.0d3') -- Remove all Android UAs
                              AND m.dt >= :start_date
                              AND m.dt < :end_date
                              AND NOT EXISTS (
                                SELECT 1 FROM sandbox.iphone_uid siu
                                WHERE siu.user_id = m.user_id
                              ) -- avoid adding duplicate entries 
                           AND (m.mobile_type LIKE '%Tagged%'
                           OR m.mobile_type LIKE '%Hi5%') -- UAs for mobile applications
                           "
# More efficient: Left outer join to make sure uid does not already exist
# LEFT OUTER JOIN sandbox.iphone_uid WHERE alias.user_id is null

queryByDateRange(query.insert.iphone_uid, min_date, max_date) # Fill Table


# Get all iphone app session ids, within days_back days, of the users who used the iphone app during the sample week
query.drop.iphone_app_sessions <- "DROP TABLE sandbox.iphone_app_sessions"
fetchQuery(query.drop.iphone_app_sessions)
query.create.iphone_app_sessions <- "CREATE TABLE sandbox.iphone_app_sessions (
                                      session_id varchar(32)
                                    )"
fetchQuery(query.create.iphone_app_sessions) # Create table

query.insert.iphone_app_sessions <- "INSERT INTO sandbox.iphone_app_sessions
                                     SELECT DISTINCT l.session_id
                                     FROM mobile_api_log l, sandbox.iphone_uid iu
                                     WHERE iu.user_id = l.user_id -- We only care about known iphone users during the week in question
                                      AND  l.dt >= :start_date
                                      AND l.dt < :end_date
                                      AND NOT (l.mobile_type LIKE 'Tagged/%/an%' 
                                        OR l.mobile_type LIKE 'Hi5/%/an%' 
                                        OR l.mobile_type = 'Tagged/2.0+CFNetwork/459 Darwin/10.0.0d3') -- Not android user
                                      AND (l.mobile_type LIKE '%Tagged%'
                                        OR l.mobile_type LIKE '%Hi5%') -- is mobile app user
                                      AND l.session_id NOT IN (
                                        SELECT session_id FROM sandbox.iphone_app_sessions
                                      ) -- avoid duplicate rows"
queryByDateRange(query.insert.iphone_app_sessions, bottom_date, max_date) # populate table

# Count users 
query.count.iphone_uid <- "SELECT COUNT(user_id) 
                           FROM sandbox.iphone_uid"
count.iphone_uid <- fetchQuery(query.count.iphone_uid)$count # Count the number of users who used iphone in the input time period
print(count.iphone_uid)

# Keep track of the messages sent, by day, by iphone user who was active during the sample time period
query.drop.daily_messages_sent <- "DROP TABLE sandbox.daily_messages_sent"
fetchQuery(query.drop.daily_messages_sent) # Drop old table
query.create.daily_messages_sent <- "CREATE TABLE sandbox.daily_messages_sent (
                                      user_id numeric(15, 0),
                                      messages_sent numeric(8, 0),
                                      dt date
                                     )"
fetchQuery(query.create.daily_messages_sent) # Create table

query.insert.daily_messages_sent <- "INSERT INTO sandbox.daily_messages_sent
                                     SELECT ml.from_user_id as user_id, COUNT(DISTINCT ml.msg_id) as messages_sent, 
                                      DATE(ml.dt) as dt
                                     FROM message_log ml, sandbox.iphone_app_sessions ias
                                     WHERE ias.session_id = ml.session_id -- Must be an iphone app session, as pre-determined
                                      AND ml.sub_type = 'send' -- has to be a message the user sent
                                      AND ml.dt >= :start_date
                                      AND ml.dt < :end_date
                                     GROUP BY ml.from_user_id, DATE(ml.dt)"
queryByDateRange(query.insert.daily_messages_sent, bottom_date, max_date) # Fill table


# Daily messages received, by iphone user, by day, for iphone users active during the sample time period
query.drop.daily_messages_received <- "DROP TABLE sandbox.daily_messages_received"
fetchQuery(query.drop.daily_messages_received) # Drop table
query.create.daily_messages_received <- "CREATE TABLE sandbox.daily_messages_received (
                                      user_id numeric(15, 0),
                                      messages_received numeric(8, 0),
                                      dt date
                                     )"
fetchQuery(query.create.daily_messages_received) # Create new version of table

query.insert.daily_messages_received <- "INSERT INTO sandbox.daily_messages_received
                                         SELECT ml.to_user_id as user_id, COUNT(DISTINCT ml.msg_id) AS messages_received,
                                         DATE(ml.dt) AS dt
                                         FROM message_log ml, sandbox.iphone_app_sessions ias
                                         WHERE ias.session_id = ml.session_id
                                         AND ml.sub_type = 'deliver'
                                         AND ml.dt >= :start_date
                                         AND ml.dt < :end_date
                                         GROUP BY ml.to_user_id, DATE(ml.dt)
                                         "
queryByDateRange(query.insert.daily_messages_received, bottom_date, max_date) # Fill table

# Get daily messages sent
query.get.daily_messages_sent <- "SELECT b.user_id, udl.cancel_reason_code,
                                  SUM(dms.messages_sent) as total_messages_sent,
                                  b.total_messages_received,
                                  COUNT(dms.dt) AS days_messaged
                                  FROM (SELECT dmr.user_id, SUM(dmr.messages_received) as total_messages_received
                                        FROM sandbox.daily_messages_received dmr
                                        WHERE dmr.dt <= :start_date
                                        AND dmr.dt > :start_date - integer '14'
                                        GROUP BY dmr.user_id) b
                                  INNER JOIN sandbox.daily_messages_sent dms
                                    ON dms.user_id = b.user_id
                                  INNER JOIN userdata_light udl
                                    ON udl.user_id = b.user_id
                                  WHERE dms.dt <= :start_date
                                  AND dms.dt > :start_date - integer '14'
                                  GROUP BY b.user_id, udl.cancel_reason_code, b.total_messages_received"
dms <- queryByDateRange(query.get.daily_messages_sent, min_date, max_date)



# Filter the users
users <- mclapply(dms, function(i) i$user_id[i$total_messages_sent >= min_messages_sent & 
                                             i$total_messages_received >= min_messages_received & 
                                             i$days_messaged >= min_days_messages_sent & 
                                             is.na(i$cancel_reason_code)])
users <- unique(Reduce(c, users))
print(length(unique(users))) # This is the final answer.

query.check.user_id <- "SELECT m.time_sent, m.sub_type, m.to_user_id, m.from_user_id, m.msg_id 
                        FROM message_log m, sandbox.iphone_app_sessions ias
                        WHERE (m.from_user_id = 7265644918
                        OR m.to_user_id = 7265644918)
                        AND m.dt >= :start_date
                        AND m.dt < :end_date
                        AND ias.session_id = m.session_id"

check <- queryByDateRange(query.check.user_id, bottom_date, max_date)
