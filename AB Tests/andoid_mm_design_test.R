# Date: September 1, 2013
# Description: Analysis of the MeetMe Redesign on Android
setwd("~/Documents/R Scripts/AB Tests/data")
getData=1
#----------------------
#  Delete old and establish connection to GP
#--------------------------
if (getData==1) {
  delete_old <- function()
    rm(list=ls(pos=.GlobalEnv), pos=.GlobalEnv)
  delete_old()
  ls()
}
getData=1

# Set up variables
# Name of test: Meet Me redesign for android
test_start = '2013/8/27 12:00:00'
test_end = '2013/8/31 23:59:00'
test_id = '54103'  
test_assign_start = '2013/8/24 00:00:00'
weight_A = 90
weight_B = 10
appVersions="ua like 'Tagged/3.9.0%'"


testassigntmp=paste("sandbox.sjh_tst_asgn_",test_id,sep="")
# Set up connection to GP
require(RPostgreSQL)
library(plotrix)
makeCxn <- function(user = "simon", password = "simon",
                    dbname = "prod", host = "gp.tagged.com", 
                    port = 5432, driver = "PostgreSQL") {
  drv <- dbDriver(driver)
  cxn <- dbConnect(drv, user = user, password = password,
                   dbname = dbname, host = host, port = port)
  cxn
}
fetchQuery <- function(query, user = "simon", password = "simon",
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

#---------------------------------------------
# Get all users who have been test assigned for this test 
#---------------------------------------------

query <- paste("
               drop table if exists ",testassigntmp,"; ",sep="");
if (getData==1) {fetchQuery(query)}

query <- paste("
               create table ", testassigntmp, " as
               SELECT distinct test_id, version_id, key as user_id
               FROM test_assign_log
               WHERE  test_id = ", test_id, "
               AND    dt >=  '", test_assign_start, "'
               AND    dt <=  '", test_end, "' ;  ", sep = "")
if (getData==1) {fetchQuery(query)}

# Check test assignment
query <- paste("
               select version_id, count(*) as num_users
               from ", testassigntmp," group by version_id order by version_id ;  ")
if (getData==1) {tal <- fetchQuery(query)}
print(tal)
#convert to % to verify test assignment
paste(round(tal$num_users[version_id=1]/sum(tal$num_users),4)*100,"% in A",sep="");
paste(round(tal$num_users[version_id=2]/sum(tal$num_users),4)*100,"% in B",sep="");
#-----------------------------------------
# Meet use by day for people who use Andoid (new version)
#-----------------------------------------

#Here's my gross MM clicks and clickers query, that limits to Android version - you may want to look deeper than this:
query <- paste("
               select l.dt::date, 
               count(CASE WHEN version_id=1 THEN l.user_id END) as mmclicks_A, 
               count(DISTINCT CASE WHEN version_id=1 THEN  l.user_id END) as mmclickers_A,
               count(CASE WHEN version_id=2 THEN l.user_id END) as mmclicks_B, 
               count(DISTINCT CASE WHEN version_id=2 THEN  l.user_id END) as mmclickers_B
               from login_detail_log l
               join meetme_log m on l.user_id = m.user_id and l.session_id = m.session_id
               join userdata_light u on l.user_id = u.user_id   
               join ",testassigntmp," tst on (u.user_id=tst.user_id)
               where (u.cancel_reason_code = 9 or u.cancel_reason_code is null)
               and (",appVersions,") 
               and l.dt >= '", test_start, "' AND l.dt <  '", test_end, "'::date
               and m.dt >= '", test_start, "' AND m.dt < '", test_end, "'::date
               group by 1 order by 1 ", sep = "")
if (getData==1) {mm_overview <- fetchQuery(query)}
print(mm_overview)


# Scale to the two buckets to 100 % of population (assume that test assignment is random and even)
mm_overview$mmclicks_a_scaled  <-  (mm_overview$mmclicks_a / weight_A) * 100
mm_overview$mmclicks_a_scaled_err <- ((sqrt(mm_overview$mmclicks_a)*1.645) / weight_A) * 100
mm_overview$mmclicks_b_scaled  <-  (mm_overview$mmclicks_b / weight_B) * 100
mm_overview$mmclicks_b_scaled_err <- ((sqrt(mm_overview$mmclicks_b)*1.645) / weight_B) * 100
mm_overview$mmclickers_a_scaled  <-  (mm_overview$mmclickers_a / weight_A) * 100
mm_overview$mmclickers_a_scaled_err  <-  ((sqrt(mm_overview$mmclickers_a)*1.645) / weight_A) * 100
mm_overview$mmclickers_b_scaled  <-  (mm_overview$mmclickers_b / weight_B) * 100
mm_overview$mmclickers_b_scaled_err <-  ((sqrt(mm_overview$mmclickers_b)*1.645) / weight_B) * 100

write.table(mm_overview,file=paste(test_id,"_mm_overview.csv",sep=""),sep=",",row.names=F)

# Significance
t.test(mm_overview$mmclicks_a,mm_overview$mmclicks_b)
t.test(mm_overview$mmclickers_a,mm_overview$mmclickers_b)


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

#----------------------------------
#Meet Me Click by date and type
#----------------------------------
query <- paste("
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
               join ",testassigntmp," tst on (u.user_id=tst.user_id)
               where (u.cancel_reason_code = 9 or u.cancel_reason_code is null)
               and (",appVersions," ) 
               and l.dt >= '", test_start, "' AND l.dt <  '", test_end, "'::date
               and m.dt >= '", test_start, "' AND m.dt < '", test_end, "'::date
               group by 1 order by 1 " , sep = "")
if (getData==1) {mm_clicks_type <- fetchQuery(query)}
print(mm_clicks_type)


# Scale to the two buckets to 100 % of population (assume that test assignment is 90/10)
mm_clicks_type$mm_initial_yes_clicks_a_s  <-  mm_clicks_type$mm_initial_yes_clicks_a   / weight_A * 100
mm_clicks_type$mm_initial_yes_clicks_b_s  <-  mm_clicks_type$mm_initial_yes_clicks_b   / weight_B * 100

mm_clicks_type$mm_initial_no_clicks_a_s  <-  mm_clicks_type$mm_initial_no_clicks_a   / weight_A * 100
mm_clicks_type$mm_initial_no_clicks_b_s  <-  mm_clicks_type$mm_initial_no_clicks_b   / weight_B * 100

mm_clicks_type$mm_interesed_yes_clicks_a_s  <-  mm_clicks_type$mm_interesed_yes_clicks_a   / weight_A * 100
mm_clicks_type$mm_interesed_yes_clicks_b_s  <-  mm_clicks_type$mm_interested_yes_clicks_b   / weight_B * 100

mm_clicks_type$mm_interested_no_clicks_a_s  <-  mm_clicks_type$mm_interested_no_clicks_a   / weight_A * 100
mm_clicks_type$mm_interested_no_clicks_b_s  <-  mm_clicks_type$mm_interested_no_clicks_b   / weight_B * 100


write.table(mm_clicks_type,file=paste(test_id,"_mm_clicks_type.csv",sep=""),sep=",",row.names=F)


# Significance
t.test(mm_clicks_type$mm_initial_yes_clicks_a,mm_clicks_type$mm_initial_yes_clicks_b )
t.test(mm_clicks_type$mm_interesed_yes_clicks_a,mm_clicks_type$mm_interesed_yes_clicks_b )


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
legend("topright", bty = "n", c("Initial Yes","Initial No", "Interested Yes", "Interested No"), # puts text in the legend 
       lty=c(2, 2, 1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("green","red", "green", "red")) # gives the legend lines the correct color and width

#-----------------------------------------
# Messages sent by day people who use Andoid (new version)
#-----------------------------------------

#Here's my gross MM clicks and clickers query, that limits to Android version - you may want to look deeper than this:
query <- paste("
               select l.dt::date, 
               count(CASE WHEN version_id=1 THEN l.user_id END) as msg_A, 
               count(DISTINCT CASE WHEN version_id=1 THEN  l.user_id END) as msg_senders_A,
               count(CASE WHEN version_id=2 THEN l.user_id END) as msg_B, 
               count(DISTINCT CASE WHEN version_id=2 THEN  l.user_id END) as msg_senders_B
               from login_detail_log l
               join message_log m on l.user_id = m.from_user_id and l.session_id = m.session_id
               join userdata_light u on l.user_id = u.user_id   
               join ",testassigntmp," tst on (u.user_id=tst.user_id)
               where (u.cancel_reason_code = 9 or u.cancel_reason_code is null)
               and (", appVersions,") 
               and l.dt >= '", test_start, "' AND l.dt <  '", test_end, "'::date
               and m.dt >= '", test_start, "' AND m.dt < '", test_end, "'::date
               and sub_type = 'send'
               group by 1 order by 1 ", sep = "")

if (getData==1) {msg_overview <- fetchQuery(query)}
print(msg_overview)

# Scale to the two buckets to 100 % of population (assume that test assignment is random and even)
msg_overview$msg_a_scaled  <-  msg_overview$msg_a / weight_A * 100
msg_overview$msg_b_scaled  <-  msg_overview$msg_b / weight_B * 100

msg_overview$msg_senders_a_scaled  <-  msg_overview$msg_senders_a / weight_A * 100
msg_overview$msg_senders_b_scaled  <-  msg_overview$msg_senders_b / weight_B * 100

write.table(msg_overview,file=paste(test_id,"_msg_overview.csv",sep=""),sep=",",row.names=F)


# Significance
t.test(msg_overview$msg_a,msg_overview$msg_b)
t.test(msg_overview$msg_senders_a,msg_overview$msg_senders_b)

# Plot
par(mfrow = c(1, 2))
# Daily difference messages
with(msg_overview, plot(dt, ((msg_b_scaled /msg_a_scaled ) - 1 ) * 100 ,  type = "l", col = "black", lwd = 2, ylim = c(-10, 10), 
                        ylab = "% change in messages sent", xlab = "Day", main = "Messages"))
abline(h = 0 , lty = 3)
# Daily difference  users sending messages
with(msg_overview, plot(dt, ((msg_senders_b_scaled /msg_senders_a_scaled ) - 1 ) * 100 ,  type = "l", col = "black", lwd = 2, ylim = c(-5, 5), 
                        ylab = "% change in users sending messages", xlab = "Day", main = "Message Senders"))
abline(h = 0 , lty = 3)

#-----------------------------------------
# Gifts sent by day people who use Andoid (new version)
#-----------------------------------------

query <- paste("
               select l.dt::date, 
               count(CASE WHEN version_id=1 THEN l.user_id END) as gifts_A, 
               count(DISTINCT CASE WHEN version_id=1 THEN  l.user_id END) as gift_senders_A,
               count(CASE WHEN version_id=2 THEN l.user_id END) as gifts_B, 
               count(DISTINCT CASE WHEN version_id=2 THEN  l.user_id END) as gift_senders_B
               from login_detail_log l
               join (SELECT * FROM virtual_gift_log WHERE action='sent') m on l.user_id = m.from_user_id
               join userdata_light u on l.user_id = u.user_id   
               join ",testassigntmp," tst on (u.user_id=tst.user_id)
               where (u.cancel_reason_code = 9 or u.cancel_reason_code is null)
               and (", appVersions,") 
               and l.dt >= '", test_start, "' AND l.dt <  '", test_end, "'::date
               and m.dt >= '", test_start, "' AND m.dt < '", test_end, "'::date
               group by 1 order by 1 ", sep = "")

if (getData==1) {gift_overview <- fetchQuery(query)}
print(gift_overview)
# Scale to the two buckets to 100 % of population (assume that test assignment is random and even)
gift_overview$gifts_a_scaled  <-  gift_overview$gifts_a / weight_A * 100
gift_overview$gifts_b_scaled  <-  gift_overview$gifts_b / weight_B * 100

gift_overview$gift_senders_a_scaled  <-  gift_overview$gift_senders_a / weight_A * 100
gift_overview$gift_senders_b_scaled  <-  gift_overview$gift_senders_b / weight_B * 100

write.table(gift_overview,file=paste(test_id,"_gift_overview.csv",sep=""),sep=",",row.names=F)


# Signficance
t.test(gift_overview$gifts_a,gift_overview$gifts_b )
t.test(gift_overview$gift_senders_a,gift_overview$gift_senders_b)
# Plot
par(mfrow = c(1, 2))

# Daily difference  gits sent
with(gift_overview, plot(dt, ((gifts_b_scaled /gifts_a_scaled ) - 1 ) * 100 ,  type = "l", col = "black", lwd = 2, ylim = c(-50, 20), 
                         ylab = "% change in gifts sent", xlab = "Day", main = "Gifts"))
abline(h = 0 , lty = 3)
# Daily difference  users sending gifts
with(gift_overview, plot(dt, ((gift_senders_b_scaled /gift_senders_a_scaled ) - 1 ) * 100 ,  type = "l", col = "black", lwd = 2, ylim = c(-15, 35), 
                         ylab = "% change in users sending gifts", xlab = "Day", main = "Gift Senders"))
abline(h = 0 , lty = 3)
