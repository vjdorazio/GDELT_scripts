## normalizing_gdelt_blog.r
## Vito D'Orazio, December 20, 2013
## constructs monthly-level protest intensity from residuals (D'Orazio & Masad, http://gdeltblog.wordpress.com/2013/12/30/more-on-normalizing-gdelt-data/#comments)
## also constructs monthly and yearly normalized ratios http://gdeltblog.wordpress.com/2013/09/28/normalizing-gdelt-protest-data/


rm(list=ls())

install.packages("Zelig", repos="http://r.iq.harvard.edu", type="source")
library(Zelig)
library(foreign)
library(ggplot2)
library(gridExtra)
library(stringr)
library(Amelia)

## this installs countrycode from github, the version contains the fips104 bug fix
library(devtools)
dev_mode(on=T)
install_github("vincentarelbundock/countrycode")
library(countrycode)
dev_mode(on=F)


set.seed(4426)
#setwd("/Users/vjdorazio/Desktop/PITF/GDELT/counts")
## if you'd like these data, email me: dorazio@iq.harvard.edu
t1 <- read.csv("total_events.csv", header=TRUE)
t2 <- read.csv("year_protest_count.csv", header=TRUE)
t3 <- read.csv("month_geo_total_count.csv", header=TRUE)
t4 <- read.csv("month_geo_protest_count.csv", header=TRUE)
colnames(t3) <- c("MonthYear", "ActionGeo_CountryCode", "count.all")
colnames(t4) <- c("MonthYear", "ActionGeo_CountryCode", "count.pro")
colnames(t1) <- c("Year", "count.events")
colnames(t2) <- c("Year", "count.protests")


mydata <- merge(x=t3, y=t4, all=TRUE, by=c("MonthYear", "ActionGeo_CountryCode"))
mydata$Year <- substr(mydata$MonthYear, 1, 4)

mydata <- merge(x=mydata, y=t1, by=c("Year"), all.x=TRUE)
mydata <- merge(x=mydata, y=t2, by=c("Year"), all.x=TRUE)
mydata$count.pro[which(is.na(mydata$count.pro))] <- 0
mydata$frac_event <- mydata$count.protests / mydata$count.events
mydata$frac_pro <- mydata$count.pro / mydata$count.all
mydata$Year <- as.numeric(mydata$Year)
mydata <- na.omit(mydata)


########################
## initial residuals model
## monthly level

fit <- lm(frac_pro ~ frac_event + as.factor(Year) + as.factor(ActionGeo_CountryCode), data=mydata)
#predict(fit)
#fit$residuals is your data

egypt <- "EG"
syria <- "SY"
france <- "FR"

intensity <- cbind(mydata$MonthYear, mydata$ActionGeo_CountryCode, fit$residuals)

pro1 <- as.data.frame(intensity[which(mydata$ActionGeo_CountryCode==egypt),])
pro2 <- as.data.frame(intensity[which(mydata$ActionGeo_CountryCode==syria),])
pro3 <- as.data.frame(intensity[which(mydata$ActionGeo_CountryCode==france),])

colnames(pro1) <- c("MonthYear", "Country", "Residuals")
colnames(pro2) <- c("MonthYear", "Country", "Residuals")
colnames(pro3) <- c("MonthYear", "Country", "Residuals")

my.ylim <- c(-.02, .06)

xlab <- c("1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013")

p1 <- ggplot(pro1, aes(x=MonthYear, y=Residuals)) + geom_point(shape=1) + scale_x_discrete(name="MonthYear", breaks=c(12,24,36,48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168, 180, 192, 204, 216, 228, 240, 252, 264, 276, 288, 300, 312, 324, 336, 348, 360, 372, 384, 396, 408), labels=xlab) + scale_y_continuous(limits = c(-0.02, 0.06))+ ggtitle("Egypt")


p2 <- ggplot(pro2, aes(x=MonthYear, y=Residuals)) + geom_point(shape=1) + scale_x_discrete(name="MonthYear", breaks=c(12,24,36,48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168, 180, 192, 204, 216, 228, 240, 252, 264, 276, 288, 300, 312, 324, 336, 348, 360, 372, 384, 396, 408), labels=xlab) + scale_y_continuous(limits = c(-0.02, 0.06))+ ggtitle("Syria")

p3 <- ggplot(pro3, aes(x=MonthYear, y=Residuals)) + geom_point(shape=1) + scale_x_discrete(name="MonthYear", breaks=c(12,24,36,48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168, 180, 192, 204, 216, 228, 240, 252, 264, 276, 288, 300, 312, 324, 336, 348, 360, 372, 384, 396, 408), labels=xlab) + scale_y_continuous(limits = c(-0.02, 0.06))+ ggtitle("France")

grid.arrange(p1, p2, p3, ncol=1, nrow=3)



#############
## original ratio transformation
## number of protests / number of events in each country year

# count.all is all events in a country in a month
# count.pro is all protests in a country in a month
mydata2 <- mydata[,c(1,3,4,5)]
aggdata <- aggregate(x=mydata2[,3:4], by=list(mydata2[,1], mydata2[,2]), FUN="sum")
aggdata <- aggdata[which(aggdata[,2] != ""),]


aggdata$State <- countrycode(sourcevar=as.character(aggdata[,2]), origin="fips104", destination="iso3c", warn=TRUE)

#post-conversion
# aggdata NAs: AQ (American Samoa), BD (Bermuda), BQ (Navassa Island), BS (Bassas da India), BV (Bouvet Island), CR (Coral Sea Islands), DQ (Jarvis Island), FQ (Baker Island), GQ (Guam), HM (Heard Island and McDonald Islands), HQ (Howland Island), IO (British Indian Ocean Territory), JE (Jersey), JN (Jan Mayen), JQ (Johnston Atoll), JU (Juan de Nova Island), KQ (Kingman Reef), KT (Christmas Island), LQ (Palmyra Attol), MF (Mayotte), MQ (Midway Islands), NF (Norfolk Island), NT (unknown), OC (unknown), OS (unknown), PC (Pitcairn Islands), PF (Paracel Islands), RB (unknown), RN (Saint Martin), SB (Saint Pierre and Miquelon), TE (Tromelin Island), TK (Turks and Caicos Islands), TL (Tokelau), WF (Wallis and Futuna), WQ (Wake Island), YI (unknown)


aggdata$State[which(aggdata[,2]=="GZ")] <- "GAZ" #gaza
aggdata$State[which(aggdata[,2]=="KV")] <- "KOS" #kosovo
aggdata$State[which(aggdata[,2]=="PG")] <- "SPL" #spratley
aggdata$State[which(aggdata[,2]=="WE")] <- "WBK" #west bank
aggdata$State[which(aggdata[,2]=="HK")] <- "HKG" #hong kong

colnames(aggdata) <- c("Year","State_fips","count_all","count_protests","State_iso3c")
aggdata$norm_protests <- aggdata$count_protests / aggdata$count_all


out.month <- mydata[,c("ActionGeo_CountryCode","MonthYear","count.all","count.pro","frac_pro")]

out.month$State <- countrycode(sourcevar=as.character(out.month[,1]), origin="fips104", destination="iso3c", warn=TRUE)

#post-conversion
# aggdata NAs: AQ (American Samoa), BD (Bermuda), BQ (Navassa Island), BS (Bassas da India), BV (Bouvet Island), CR (Coral Sea Islands), DQ (Jarvis Island), FQ (Baker Island), GQ (Guam), HM (Heard Island and McDonald Islands), HQ (Howland Island), IO (British Indian Ocean Territory), JE (Jersey), JN (Jan Mayen), JQ (Johnston Atoll), JU (Juan de Nova Island), KQ (Kingman Reef), KT (Christmas Island), LQ (Palmyra Attol), MF (Mayotte), MQ (Midway Islands), NF (Norfolk Island), NT (unknown), OC (unknown), OS (unknown), PC (Pitcairn Islands), PF (Paracel Islands), RB (unknown), RN (Saint Martin), SB (Saint Pierre and Miquelon), TE (Tromelin Island), TK (Turks and Caicos Islands), TL (Tokelau), WF (Wallis and Futuna), WQ (Wake Island), YI (unknown)


out.month$State[which(out.month[,1]=="GZ")] <- "GAZ" #gaza
out.month$State[which(out.month[,1]=="KV")] <- "KOS" #kosovo
out.month$State[which(out.month[,1]=="PG")] <- "SPL" #spratley
out.month$State[which(out.month[,1]=="WE")] <- "WBK" #west bank
out.month$State[which(out.month[,1]=="HK")] <- "HKG" #hong kong

out.month <- out.month[which(out.month[,1] != ""),]
colnames(out.month) <- c("State_fips", "MonthYear", "count_all", "count_protests", "norm_protests", "State_iso3c")

write.table(aggdata, "gdelt_protests_yearly.tsv", sep="\t", row.names=FALSE)
write.table(out.month, "gdelt_protests_monthly.tsv", sep="\t", row.names=FALSE)





