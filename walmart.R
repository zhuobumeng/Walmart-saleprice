# required packages
library(lubridate)  # to deal with Dates in R
library(lattice)
setwd("~/Desktop/374 project")

###############################################
# Pre-processing of the Data
###############################################
feature=read.csv("features.csv")
train=read.csv("train.csv")
store=read.csv("stores.csv")
test=read.csv("test.csv")

dim(store) # 45 stores

# Merge train, store, and feature
mytrain = merge(x=train, y=store, all.x=TRUE)
mytrain = merge(x=mytrain, y=feature, all.x=TRUE)
mytest = merge(x=test, y=store, all.x=TRUE)
mytest = merge(x=mytest, y=feature, all.x=TRUE)

# Remove "MarkDown" -- many participants said they are not much useful
remove.var = NULL; 
for(k in 1:5) remove.var=c(remove.var, paste("MarkDown", k, sep=''))
mytrain = mytrain[, !names(mytrain) %in% remove.var]
mytest = mytest[, !names(mytest) %in% remove.var]


# (Caution: may crash your laptop; suggest to close other apps)
# Add new vars
# "month", "year", "mday" (day in month), 
# "yday" (day in year), "wk" (week in year)
# renames some columns, and sort the data by store, dept, and date. 
names(mytrain)=c("store", "date", "isholiday", "dept", "sales", "type", "size", 
                 "temp", "fuel", "cpi", "unemp")
mytrain$year = year(mytrain$date)
mytrain$month = month(mytrain$date)
mytrain$mday = day(mytrain$date)
mytrain$yday = yday(mytrain$date)
mytrain$wk = week(mytrain$date)
mytrain = mytrain[order(mytrain$store, mytrain$dept, mytrain$date), ]
rownames(mytrain)=1:nrow(mytrain); # otherwise, the row names are the pre-ordering row numbers

names(mytest)=c("store", "date", "isholiday", "dept", "type", "size", 
                "temp", "fuel", "cpi", "unemp")
mytest$year = year(mytest$date)
mytest$month = month(mytest$date)
mytest$mday = day(mytest$date)
mytest$yday = yday(mytest$date)
mytest$wk = week(mytest$date)
rownames(mytest)=NULL; 

# Check for holidays
# Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
# Labor Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
# Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
# Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13 

tmp=mytrain[, c("date", "isholiday", "wk")]
tmp=tmp[!duplicated(tmp),]
tmp[which(tmp$isholiday==1),]
# 138 2010-02-12    TRUE  7 Super Bowl
#2265 2010-09-10    TRUE 37 Labor Day
#3021 2010-11-26    TRUE 48 Thanksgiving
#3393 2010-12-31    TRUE 53 Christmas
#3825 2011-02-11    TRUE  7 Super Bowl
#6005 2011-09-09    TRUE 37 Labor day
#6784 2011-11-25    TRUE 48 Thanksgiving
#7114 2011-12-30    TRUE 53 Christmas
#7549 2012-02-10    TRUE  6 Super Bowl
#9716 2012-09-07    TRUE 36 Labor Day

tmp=mytest[, c("date", "isholiday", "wk")]
tmp=tmp[!duplicated(tmp),]
tmp[which(tmp$isholiday==1),]
# 214  2012-11-23    TRUE 47 Thanksgiving
# 587  2012-12-28    TRUE 52 Christmas
#1011  2013-02-08    TRUE  6 Super Bowl
# Note: 1) no labor day in the test set; 2) for those holidays, the corresponding 
# "wk" index is different in the test set. 

# Create a new factor column "holidays"
tmp=rep("No", nrow(mytrain));
tmp[which(mytrain$date %in% c("2010-02-12","2011-02-11","2012-02-10"))]="Super_Bowl"
tmp[which(mytrain$date %in% c("2010-09-10","2011-09-09","2012-09-07"))]="Labor_Day"
tmp[which(mytrain$date %in% c("2010-11-26","2011-11-25"))]="Thanksgiving"
tmp[which(mytrain$date %in% c("2010-12-31","2011-12-30"))]="Christmas"
mytrain$holiday = as.factor(tmp)

tmp=rep("No", nrow(mytest));
tmp[which(mytest$date == "2013-02-08")]="Super_Bowl"
tmp[which(mytest$date =="2012-11-23")]="Thanksgiving"
tmp[which(mytest$date =="2012-12-28")]="Christmas"
mytest$holiday = as.factor(tmp)

# Not all store+dept in the test data have historical data.
# Create a new var "missing" in mytest: 1 means no data for 
# that particular store+dept combination. 
dept_store_train = table(mytrain$dept,mytrain$store);
dept_store_test = table(mytest$dept, mytest$store)
dim(dept_store_train)
dim(dept_store_test)
#[1] 81 45

dept.names = sort(unique(mytrain$dept)) # names for the 81 departments

tmp=(dept_store_train ==0 )*(dept_store_test>0)
sum(tmp)
#[1] 11
missing_dept_store = which(tmp>0, arr.ind=TRUE, useNames = FALSE)
missing_dept_store[,1] = dept.names[missing_dept_store[,1]]
missing_dept_store = missing_dept_store[order(missing_dept_store[,1], missing_dept_store[,2]),]
# order the missing dept+store by stores and dept
missing_dept_store
#[1,]    29   37  # no historical data for store 37, dept 29. 
#[2,]    30   36
#[3,]    30   42
#[4,]    39   34
#[5,]    39   45
#[6,]    43   18
#[7,]    43   24
#[8,]    99    5
#[9,]    99    9
#[10,]   99   10
#[11,]   99   25

id=NULL
tmptest = as.matrix(mytest[, c("dept", "store")])
for(k in 1:nrow(missing_dept_store)){
  tmp.flag = apply(tmptest, 1, function(x) identical(as.numeric(x), as.numeric(missing_dept_store[k,])))
  id=c(id, which(tmp.flag==TRUE))
}
length(id)
#[1] 36
mytest$missing = rep(0, nrow(mytest))
mytest$missing[id]=1;


###############################################
# Save a copy of the data
###############################################

save("mytrain", file="myTrain.Rdata")
save("mytest", file="myTest.Rdata")


###############################################
# A quick summary of the data
###############################################
rm(list=objects())
load("myTrain.Rdata")
load("myTest.Rdata")

dim(mytrain)
#[1] 421570     17
names(mytrain)
#[1] "store"     "date"      "isholiday" "dept"      "sales"     "type"     
#[7] "size"      "temp"      "fuel"      "cpi"       "unemp"     "year"     
#[13] "month"     "mday"      "yday"      "wk"        "holiday"  

dim(mytest)
#[1] 115064     17
names(mytest)
#[1] "store"     "date"      "isholiday" "dept"      "type"      "size"     
#[7] "temp"      "fuel"      "cpi"       "unemp"     "year"      "month"    
#[13] "mday"      "yday"      "wk"        "holiday"   "missing" 

summary(mytrain$sales) # minimal sale value is -4989

# Display the sales for a particular department across all stores
# Crate wk_index: set the week of 2010-02-05 as the origin

dept.names = sort(unique(mytrain$dept))
k=2; # plot the dept with name dept.names[k], k=1, .., 81. 

day1="2010-02-05"
tmp=subset(mytrain, dept==dept.names[k])
tmp$wk_index = as.numeric(difftime(tmp$date, day1, units="weeks"))
xyplot(sales ~ wk_index | store, tmp, par.strip.text=list(cex=0.75), pch=".", 
       strip = strip.custom(strip.names = TRUE, strip.levels = TRUE), 
       main=paste("Dept = ", dept.names[k]))
# There is a huge store variation amongs sales in the same department
# You can try some other values of k. 

# Or produce 81 plots using the loop below, which are saved
# in your "fig" directory. 

for(k in 1:length(dept.names)){
  #for(k in 1:2){# test with just two departments
  file.name = paste("fig/fig_sales_dept_", dept.names[k], ".png", sep="")
  png(file.name)
  tmp=subset(mytrain, dept==dept.names[k])
  tmp$wk_index = as.numeric(difftime(tmp$date, day1, units="weeks"))
  print(xyplot(sales ~ wk_index | store, tmp, par.strip.text=list(cex=0.75), pch=".", 
               strip = strip.custom(strip.names = TRUE, strip.levels = TRUE), 
               main=paste("Dept = ", dept.names[k])))
  dev.off()
}

# you can also plot log(sales+4990)


###################################################################
# A Simple Model:
###################################################################
# predict the sale at (store, dept, wk, year) by the median of the
# following three sales a year ago at the same (store, dept) 
# at (wk-1, wk, wk+1). 
# If there is not enough historical data (history < 1 yr), 
# then just use the median of all other non-NA predictions. 

# Need to adjust the sale prediction for holidays. 
# Need to provide prediction for some store + dept combinations which
# have no historical data

table(mytrain$year, mytrain$wk) # 53 weeks per year

dept.names = sort(unique(mytrain$dept))
num.stores = length(unique(mytest$store))
num.depts = length(dept.names)


dept_store_test = table(mytest$dept, mytest$store)
dept_store_train = table(mytrain$dept, mytrain$store)

mypred = rep(NA, nrow(mytest)) # vector used to store the prediction

for(i in 1:num.depts){
  print(paste("dept = ", i, "....")) # keep track of the process
  for(j in 1:num.stores){
    if (dept_store_test[i,j]==0 | dept_store_train[i,j]==0) break
    
    # for each store+dept combo, filter the train/test data   
    tmp.train.id = which(mytrain$dept==dept.names[i] & mytrain$store==j)
    tmp.train.sales = mytrain$sales[tmp.train.id]; 
    tmp.test.id = which(mytest$dept==dept.names[i] & mytest$store==j)
    
    # cumulative weeks; set week 1 in training as the reference
    week1 = week("2010-02-05")
    cum.wk.train = (mytrain$year[tmp.train.id] -2010)*53+mytrain$wk[tmp.train.id]
    cum.wk.train = cum.wk.train - week1+1; 
    cum.wk.test = (mytest$year[tmp.test.id] -2010)*53+mytest$wk[tmp.test.id]
    cum.wk.test = cum.wk.test - week1 + 1; 
    
    # there could be some missing weeks in the training data for store+dept
    # Note that tmp.train.sales may have NA values
    tmp.train.sales = rep(NA, max(cum.wk.train))
    tmp.train.sales[cum.wk.train] = mytrain$sales[tmp.train.id];
    
    
    # create the 3 sales for each record in the test data
    hist.data = matrix(, length(tmp.test.id), 3)
    hist.data[,1] = tmp.train.sales[cum.wk.test-53-1]; 
    hist.data[,2] = tmp.train.sales[cum.wk.test-53];
    hist.data[,3] = tmp.train.sales[cum.wk.test-53+1];
    
    med.pred = apply(hist.data, 1, function(x) median(x[!is.na(x)]))
    
    # If no prior year's data available, just predict it to be the median
    # of the other predicted values
    med.pred[is.na(med.pred)] = median(med.pred, na.rm=TRUE)
    mypred[tmp.test.id] = med.pred     
  }
}

##################################################################
# WRITE YOUR CODE HERE
# No histrical data available for the 36 obs in the test set
##################################################################

mypred[mytest$missing==1] # the 36 obs in the test data


##################################################################
# WRITE YOUR CODE HERE
# The prediction for holidays need to be adjusted
##################################################################



##################################################################
# Prepare the submission file
##################################################################

# For each row in the test set (store + department + date triplet), 
# you should predict the weekly sales of that department. The Id column 
# is formed by concatenating the Store, Dept, and Date with underscores 
# (e.g. Store_Dept_2012-11-02).  The file should have a header and looks 
# like the following:
#
# Id,Weekly_Sales
# 1_1_2012-11-02,0
# 1_1_2012-11-09,0
# 1_1_2012-11-16,0


filepath = "output/submission1.csv";
ID=apply(mytest[,c("store", "dept", "date")], 1, function(x)  
  paste(x[1],  x[2],  as.character(x[3]), sep='_'))
#ID[1:5] 
# need to trim the space in ID
ID = gsub("\\s","", ID)
myout = data.frame(ID=ID, Weekly_Sales = mypred)
write.csv(myout, file=filepath,  row.names=FALSE, quote = FALSE);

