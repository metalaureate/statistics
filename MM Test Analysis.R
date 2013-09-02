# By: Helena Buhr
# Date: August 29, 2013
# Description: Analysis of the Android redesign for Meet Me

#----------------------
#  Delete old and establish connection to GP
#--------------------------

#delete_old <- function()
#  rm(list=ls(pos=.GlobalEnv), pos=.GlobalEnv)
#delete_old()
#ls()

# Set up connection to GP
require(RPostgreSQL)
makeCxn <- function(user = "helena", password = "helena",
                    dbname = "prod", host = "gp.tagged.com", 
                    port = 5432, driver = "PostgreSQL") {
  drv <- dbDriver(driver)
  cxn <- dbConnect(drv, user = user, password = password,
                   dbname = dbname, host = host, port = port)
  cxn
}
fetchQuery <- function(query, user = "helena", password = "helena",
                       dbname = "prod", host = "gp.tagged.com", 
                       port = 5432, n = -1, verbose = TRUE) {
  res <- NULL
  cxn <- makeCxn(user = user, password = password,
                 dbname = dbname, host = host, port = port)
  t1 <- Sys.time()
  tmp <- try(dbSendQuery(cxn, query))
  if (!('try-error' %in% class(tmp))) {
    res <- fetch(tmp, n)
    if (verbose) {
      print(Sys.time() - t1)
      print(dim(res))
    }
  }
  dbDisconnect(cxn)
  res
}

#-----------------------
# Error calculation by day for a vector of daily values 
# ----------------------



#---------------------------------------------
# Get all users who have been test assigned for this test 
#---------------------------------------------

query <- "
drop table if exists sandbox.hb_androidmmtest; " 
fetchQuery(query)

query <- "
create table sandbox.hb_androidmmtest as
SELECT distinct test_id, version_id, key as user_id
FROM test_assign_log
WHERE  test_id = 54103
AND    dt >= '2013-8-24'
AND    dt <= '2013-8-30' ;  " 
fetchQuery(query)

# Check results
query <- "
select version_id, count(*) as num_users
from sandbox.hb_androidmmtest group by version_id order by version_id ;  " 
tal <- fetchQuery(query)
print(tal)







#-----------------------------------------
# Meet use by hour for people who use Andoid (new version)
#-----------------------------------------

#Here's my gross MM clicks and clickers query, that limits to Android version - you may want to look deeper than this:
query <- "
select l.dt::date, 
count(CASE WHEN version_id=1 THEN l.user_id END) as mmclicks_A, count(DISTINCT CASE WHEN version_id=1 THEN  l.user_id END) as mmclickers_A,
count(CASE WHEN version_id=2 THEN l.user_id END) as mmclicks_B, count(DISTINCT CASE WHEN version_id=2 THEN  l.user_id END) as mmclickers_B
from login_detail_log l
join meetme_log m on l.user_id = m.user_id and l.session_id = m.session_id
join userdata_light u on l.user_id = u.user_id  
join sandbox.hb_androidmmtest tst on (u.user_id=tst.user_id)
where (u.cancel_reason_code = 9 or u.cancel_reason_code is null)
and (ua like 'Tagged/3.9.0%' ) 
and l.dt >= '2013-08-26' AND l.dt <  '2013-08-30'::date
and m.dt >= '2013-08-26' AND m.dt < '2013-08-30'::date
group by 1 order by 1 " 
mm_overview <- fetchQuery(query)
print(mm_overview)

# Scale to the two buckets to 100 % of population (assume that test assignment is 90/10)
mm_overview$mmclicks_a_scaled  <-  mm_overview$mmclicks_a / 90 * 100
mm_overview$mmclicks_b_scaled  <-  mm_overview$mmclicks_b / 10 * 100

mm_overview$mmclickers_a_scaled  <-  mm_overview$mmclickers_a / 90 * 100
mm_overview$mmclickers_b_scaled  <-  mm_overview$mmclickers_b / 10 * 100

# Plot
par(mfrow = c(1, 2))
# Absolute number clicks
#with(mm_overview, plot(dt, mmclicks_a_scaled,  type = "l", col = "black", lwd = 2))
#with(mm_overview, lines(dt, mmclicks_b_scaled,  type = "l", col = "green", lwd = 2))
# Daily difference clicks
with(mm_overview, plot(dt, ((mmclicks_b_scaled /mmclicks_a_scaled ) - 1 ) * 100 ,  type = "l", col = "black", lwd = 2, ylim = c(-40, 40), 
                       ylab = "% change in Meet Me clicks", xlab = "Day", main = "Clicks"))
abline(h = 0 , lty = 3)
# Daily difference  clickers
with(mm_overview, plot(dt, ((mmclickers_b_scaled /mmclickers_a_scaled ) - 1 ) * 100 ,  type = "l", col = "black", lwd = 2, ylim = c(-10, 10), 
                       ylab = "% change in Meet Me clickers", xlab = "Day", main = "Clickers"))
abline(h = 0 , lty = 3)


#Meet Me Click by date and type
query <- "
select l.dt::date, 
count(CASE WHEN version_id=1 AND vote = 'Y' and is_match= 0 THEN l.user_id END) as mm_initial_yes_clicks_A, 
count(DISTINCT CASE WHEN version_id=1 AND vote = 'Y' and is_match = 0 THEN  l.user_id END) as mm_initial_yes_clickers_A,
count(CASE WHEN version_id=2 AND vote = 'Y' and is_match= 0 THEN l.user_id END) as mm_initial_yes_clicks_B, 
count(DISTINCT CASE WHEN version_id=2 AND vote = 'Y' and is_match= 0 THEN  l.user_id END) as mm_initial_yes_clickers_B, 

count(CASE WHEN version_id=1 AND vote = 'N' and is_match= 0 THEN l.user_id END) as mm_initial_no_clicks_A,
count(DISTINCT CASE WHEN version_id=1 AND vote = 'N' and is_match = 0 THEN  l.user_id END) as mm_initial_no_clickers_A,
count(CASE WHEN version_id=2 AND vote = 'N' and is_match= 0 THEN l.user_id END) as mm_initial_no_clicks_B,
count(DISTINCT CASE WHEN version_id=2 AND vote = 'N' and is_match= 0 THEN  l.user_id END) as mm_initial_no_clickers_B,

count(CASE WHEN version_id=1 AND vote = 'Y' and is_match= 1 THEN l.user_id END) as mm_interesed_yes_clicks_A, 
count(DISTINCT CASE WHEN version_id=1 AND vote = 'Y' and is_match = 1 THEN  l.user_id END) as mm_interested_yes_clickers_A,
count(CASE WHEN version_id=2 AND vote = 'Y' and is_match= 1 THEN l.user_id END) as mm_interested_yes_clicks_B, 
count(DISTINCT CASE WHEN version_id=2 AND vote = 'Y' and is_match= 1 THEN  l.user_id END) as mm_interested_yes_clickers_B,

count(CASE WHEN version_id=1 AND vote = 'N' and is_match= 1 THEN l.user_id END) as mm_interested_no_clicks_A, 
count(DISTINCT CASE WHEN version_id=1 AND vote = 'N' and is_match = 1 THEN  l.user_id END) as mm_interested_no_clickers_A,
count(CASE WHEN version_id=2 AND vote = 'N' and is_match= 1 THEN l.user_id END) as mm_interested_no_clicks_B, 
count(DISTINCT CASE WHEN version_id=2 AND vote = 'N' and is_match= 1 THEN  l.user_id END) as mm_interested_no_clickers_B

from login_detail_log l
join meetme_log m on l.user_id = m.user_id and l.session_id = m.session_id
join userdata_light u on l.user_id = u.user_id  
join sandbox.hb_androidmmtest tst on (u.user_id=tst.user_id)
where (u.cancel_reason_code = 9 or u.cancel_reason_code is null)
and (ua like 'Tagged/3.9.0%' ) 
and l.dt >= '2013-08-26' AND l.dt <  '2013-08-30'::date
and m.dt >= '2013-08-26' AND m.dt < '2013-08-30'::date
group by 1 order by 1 " 
mm_clicks_type <- fetchQuery(query)
print(mm_clicks_type)


# Scale to the two buckets to 100 % of population (assume that test assignment is 90/10)
mm_clicks_type$mm_initial_yes_clicks_a_s  <-  mm_clicks_type$mm_initial_yes_clicks_a  / 90 * 100
mm_clicks_type$mm_initial_yes_clicks_b_s  <-  mm_clicks_type$mm_initial_yes_clicks_b  / 10 * 100

mm_clicks_type$mm_initial_no_clicks_a_s  <-  mm_clicks_type$mm_initial_no_clicks_a  / 90 * 100
mm_clicks_type$mm_initial_no_clicks_b_s  <-  mm_clicks_type$mm_initial_no_clicks_b  / 10 * 100

mm_clicks_type$mm_interesed_yes_clicks_a_s  <-  mm_clicks_type$mm_interesed_yes_clicks_a  / 90 * 100
mm_clicks_type$mm_interesed_yes_clicks_b_s  <-  mm_clicks_type$mm_interested_yes_clicks_b  / 10 * 100

mm_clicks_type$mm_interested_no_clicks_a_s  <-  mm_clicks_type$mm_interested_no_clicks_a  / 90 * 100
mm_clicks_type$mm_interested_no_clicks_b_s  <-  mm_clicks_type$mm_interested_no_clicks_b  / 10 * 100


# Daily difference clicks
par(mfrow = c(1, 1))
with(mm_clicks_type, plot(dt, ((mm_initial_yes_clicks_b_s /mm_initial_yes_clicks_a_s ) - 1 ) * 100 ,  type = "l", col = "green", lwd = 2, lty = 2, ylim = c(-40, 40), 
                          ylab = "% change in Meet Me clicks", xlab = "Day", main = "Clicks"))
abline(h = 0 , lty = 3)
with(mm_clicks_type, lines(dt, ((mm_initial_no_clicks_b_s /mm_initial_no_clicks_a_s ) - 1 ) * 100 ,  type = "l", col = "red", lwd = 2, lty = 2, ylim = c(-40, 40), 
                           ylab = "% change in Meet Me clicks", xlab = "Day", main = "Clicks"))
with(mm_clicks_type, lines(dt, ((mm_interesed_yes_clicks_b_s /mm_interesed_yes_clicks_a_s ) - 1 ) * 100 ,  type = "l", col = "green", lwd = 2, ylim = c(-40, 40), 
                           ylab = "% change in Meet Me clicks", xlab = "Day", main = "Clicks"))
with(mm_clicks_type, lines(dt, ((mm_interested_no_clicks_b_s /mm_interested_no_clicks_a_s ) - 1 ) * 100 ,  type = "l", col = "red", lwd = 2, ylim = c(-40, 40), 
                           ylab = "% change in Meet Me clicks", xlab = "Day", main = "Clicks"))
legend("topright", bty = "n", c("Initial Yes","initial No", "Interested Yes", "Interested No"), # puts text in the legend 
       lty=c(2, 2, 1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("green","red", "green", "red")) # gives the legend lines the correct color and width