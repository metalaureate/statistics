# Date: September 1, 2013
# Description: Analysis of the Push Dingbats Test
# Limit to Android because of iOS issues over test period
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
# Name of test: dingbats_push 
test_start = '2013/8/28 12:00:00'
test_end = '2013/8/31 23:59:00'
test_id = '54406'  
test_assign_start = '2013/8/27 13:04:00'
weight_A = 50
weight_B = 50
appVersions="ua like 'Tagged/3.%'" #any version or platform


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
# DAU comparison
#-----------------------------------------

query <- paste("
               select l.dt::date, 
               count(DISTINCT CASE WHEN version_id=1 THEN l.user_id END) as dau_A, 
               count(DISTINCT CASE WHEN version_id=2 THEN l.user_id END) as dau_B 
               from login_detail_log l
               join userdata_light u on l.user_id = u.user_id
               join ",testassigntmp," tst on (u.user_id=tst.user_id)
               where (u.cancel_reason_code = 9 or u.cancel_reason_code is null)
               and (",appVersions,") 
               and l.dt >= '", test_start, "' AND l.dt <  '", test_end, "'::date
               group by 1 order by 1 ", sep = "")
if (getData==1) {dau_overview <- fetchQuery(query)}
print(dau_overview)


# Scale to the two buckets to 100 % of population (assume that test assignment is random and even)
dau_overview$dau_a_scaled  <-  (dau_overview$dau_a / weight_A) * 100
dau_overview$dau_a_scaled_err <- ((sqrt(dau_overview$dau_a)*1.645) / weight_A) * 100

dau_overview$dau_b_scaled  <-  (dau_overview$dau_b / weight_B) * 100
dau_overview$dau_b_scaled_err <- ((sqrt(dau_overview$dau_b)*1.645) / weight_B) * 100
dau_overview$dau_bva  <-  (dau_overview$dau_b_scaled / dau_overview$dau_a_scaled) * 100

# Error ranges @ 90% CI
dau_err_a=sd(dau_overview$dau_a)*1.645*(1/weight_A)
dau_err_b=sd(dau_overview$dau_b)*1.645*(1/weight_B)

print(paste("Mean B vs A",sum(dau_overview$dau_b_scaled)/sum(dau_overview$dau_a_scaled)))
print(paste("High B vs A",(sum(dau_overview$dau_b_scaled)+dau_err_b)/sum(dau_overview$dau_a_scaled)))
print(paste("Low B vs A",(sum(dau_overview$dau_b_scaled)-dau_err_b)/sum(dau_overview$dau_a_scaled)))


write.table(dau_overview,file=paste(test_id,"_dau_overview.csv",sep=""),sep=",",row.names=F)

# Plot

par(mfrow = c(1, 2))
# Daily difference clicks
with(dau_overview, plot(dt, ((dau_bva ) - 100 ) ,  type = "l", col = "black", lwd = 2, ylim = c(-1, 1), 
                       ylab = "% change in DAU", xlab = "Day", main = "Daily Active Users"))
abline(h = 0 , lty = 3)

#-----------------------------------------
# Sessions comparison
#-----------------------------------------

query <- paste("
               select l.dt::date, 
               count(CASE WHEN version_id=1 THEN l.user_id END) as sess_A, 
               count(CASE WHEN version_id=2 THEN l.user_id END) as sess_B 
               from login_detail_log l
               join userdata_light u on l.user_id = u.user_id
               join ",testassigntmp," tst on (u.user_id=tst.user_id)
               where (u.cancel_reason_code = 9 or u.cancel_reason_code is null)
               and (",appVersions,") 
               and l.dt >= '", test_start, "' AND l.dt <  '", test_end, "'::date
               group by 1 order by 1 ", sep = "")
if (getData==1) {sess_overview <- fetchQuery(query)}
print(sess_overview)


# Scale to the two buckets to 100 % of population (assume that test assignment is random and even)
sess_overview$sess_a_scaled  <-  (sess_overview$sess_a / weight_A) * 100
sess_overview$sess_a_scaled_err <- ((sqrt(sess_overview$sess_a)*1.645) / weight_A) * 100

sess_overview$sess_b_scaled  <-  (sess_overview$sess_b / weight_B) * 100
sess_overview$sess_b_scaled_err <- ((sqrt(sess_overview$sess_b)*1.645) / weight_B) * 100
sess_overview$sess_bva  <-  (sess_overview$sess_b_scaled / sess_overview$sess_a_scaled) * 100

# Error ranges @ 90% CI
sess_err_a=sd(sess_overview$sess_a)*1.645*(1/weight_A)
sess_err_b=sd(sess_overview$sess_b)*1.645*(1/weight_B)

print(paste("Mean B vs A",sum(sess_overview$sess_b_scaled)/sum(sess_overview$sess_a_scaled)))
print(paste("High B vs A",(sum(sess_overview$sess_b_scaled)+sess_err_b)/sum(sess_overview$sess_a_scaled)))
print(paste("Low B vs A",(sum(sess_overview$sess_b_scaled)-sess_err_b)/sum(sess_overview$sess_a_scaled)))


write.table(sess_overview,file=paste(test_id,"_sess_overview.csv",sep=""),sep=",",row.names=F)

# Plot

#par(mfrow = c(1, 2))
# Daily difference clicks
with(sess_overview, plot(dt, (sess_bva - 100 ) ,  type = "l", col = "black", lwd = 2, ylim = c(-1, 1), 
                       ylab = "% change in sessions", xlab = "Day", main = "Total Daily Sessions"))
abline(h = 0 , lty = 3)



