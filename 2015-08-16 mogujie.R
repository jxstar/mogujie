library(quantmod)
library(leaps)
library(Hmisc) #describe
library(psych) #describe
library(GPArotation)
library(pastecs) #stat.desc
library(corrgram) # for corralation analysis
library(gvlma)
library(relaimpo)
library(RSQLite)

library(xlsx)
library(RMySQL)
library(ggplot2) #add for ggplot
library(reshape2)
library(dplyr)

#library(bigmemory)



#==============================================================================================
#1# read the data
#==============================================================================================

#----------------------------------------------------------------------------------------------
# make sure the time range you wanna analysis
if (T){
  
  conn <- dbConnect(MySQL(), dbname = "thdata", 
                    username="thdata_stats", password="sg40kssrlER30kGSw2rgrw",host="data.thdata.com",port=3308)
  #dbGetQuery(conn,"set names utf8")  #for macos
  dbGetQuery(conn,"set names gbk") # for win
  test=dbGetQuery(conn,"select * from simple_mogujie_cat")
  (date.range=as.data.frame(table(test$date)))
  dbDisconnect(conn)
}

#make sure the previous and the current date
#month report: we use the latest 4weeks
#================================================================
#IMPORTANT: Mogujie can only calcualte 90days deals and GMV
#           So period shoud be 90days

#previous:90days ago, here we use last month for test
(previous=as.Date(as.Date("2015-07-06"):as.Date("2015-08-02")))
(current=as.Date(as.Date("2015-08-03"):as.Date("2015-08-09")))


#----------------------------------------------------------------------------------------------
# Read data from DataBase

if (T){
  
  conn <- dbConnect(MySQL(), dbname = "thdata", 
                    username="thdata_stats", password="sg40kssrlER30kGSw2rgrw",host="data.thdata.com",port=3308)
  #dbGetQuery(conn,"set names utf8")  #for macos
  dbGetQuery(conn,"set names gbk") # for win
  

  #-----------------------------------------
  #Read SKU: >last(previous)  <=last(current)
  sku=dbGetQuery(conn,"select * from simple_mogujie_item where date>20150802 and date<=20150809")
  #Read the glastxxx: <=last(previous)
  glastsku=dbGetQuery(conn,"select itemid, date, cfav, sale, price, items, cmtn, storeid
                      from (select * from simple_mogujie_item where date<=20150802 order by date desc) as sku group by itemid order by date desc")
  
  table(sku$date)
  names(sku)=c("kid","type1","type2","tid2","gid","gname","flag","likes","sales","created","price","atid","promotion","date","COGS", "grade","cmts","sid","originprice")
  sku$date=as.Date(as.character(sku$date),format="%Y%m%d")
  class(sku$created)=c('POSIXt','POSIXct')
  sku$price=sku$price/100
  str(sku)
  
  #glastsku
  names(glastsku)=c("gid", "date", "likes", "sales", "price","COGS", "cmts", "sid")
  glastsku$date=as.Date(as.character(glastsku$date),format="%Y%m%d")
  glastsku$price=glastsku$price/100
  glastsid=distinct(select(glastsku,sid),sid)
  
  
  #-----------------------------------------
  #Read store
  #store=dbGetQuery(conn,"select * from simple_mogujie_store where date>20150705 and date<=(20150811)")
  store=dbGetQuery(conn,"select *
                      from (select * from simple_mogujie_store where date>20150802 and date<=20150809 order by date desc) as store group by storeid order by date desc")
  
  glaststore=dbGetQuery(conn,"select storeid, date, fans, items, avgtime, cplts, rtns, sold, money, cmt2,cmt3, cmt4
                      from (select * from simple_mogujie_store where date<=20150802 order by date desc) as store group by storeid order by date desc")
  table(store$date)
  dim(glaststore)
  
  names(store)=c("kid","sid","sname","city","fans", "ngoods", "auth", "deliverytime", "cplts", "rtns", "sales", "warranty","grade1","grade2","grade3","grade4","created","date")
  store$date=as.Date(as.character(store$date),format="%Y%m%d")
  class(store$created)=c('POSIXt','POSIXct')
  
  names(glaststore)=c("sid","date","fans", "ngoods", "deliverytime", "cplts", "rtns", "sales", "warranty","grade2","grade3","grade4")
  glaststore$date=as.Date(as.character(glaststore$date),format="%Y%m%d")
  #class(glaststore$created)=c('POSIXt','POSIXct')
  
  
  #-----------------------------------------
  #Read tuan:>last(previous) <=last(current)
  tuan=dbGetQuery(conn,"select * from simple_mogujie_tuan where date>20150802 and date<=20150809")
  glasttuan=dbGetQuery(conn,"select *
                        from (select * from simple_mogujie_tuan where date<=20150802 order by date desc) as sku group by itemid order by date desc")
  table(tuan$date)
  table(glasttuan$date)
  
  names(tuan)=c("kid","gid","gname","endtime","price", "originprice", "sales","date")
  tuan$date=as.Date(as.character(tuan$date),format="%Y%m%d")
  class(tuan$endtime)=c('POSIXt','POSIXct')
  tuan$endtime=as.Date(tuan$endtime)
  tuan$price=tuan$price/100
  
  names(glasttuan)=c("kid","gid","gname","endtime","price", "originprice", "sales","date")
  glasttuan$date=as.Date(as.character(glasttuan$date),format="%Y%m%d")
  class(glasttuan$endtime)=c('POSIXt','POSIXct')
  glasttuan$endtime=as.Date(glasttuan$endtime)
  glasttuan$price=glasttuan$price/100
  
  dbDisconnect(conn)
  save(file=paste(first(current), last(current),"mogujie.Rdata",sep="-"),sku, glastsku, glastsid, store, glaststore, tuan, glasttuan)
}


#define 2 compared time spot
load(file=paste(first(current), last(current),"mogujie.Rdata",sep="-"))



#================================================
# select the last season from date1 to date2
skup=filter(glastsku,date %in% previous)   
sidp=distinct(select(skup,sid),sid)
dim(glastsku)
dim(skup)



#----------------------------------------------------------------------------------------------
 # Define the Macros Variables

(previous)
(gdate=unique(sku$date))
(gnperiod=length(gdate))
(gstatus=factor(c("classical","new","offshelf"),levels=c("classical","new","offshelf")))
(gcol=paste("m", 1:gnperiod, sep=""))
(gcolx=paste("m", 1:gnperiod, ".x", sep=""))
(gcoly=paste("m", 1:gnperiod, ".y", sep=""))
(gfile=paste(last(previous)+1,"to",max(gdate),"MoGuJie SKU Analysis By JX @", Sys.Date(),".xlsx",sep = " "))






#==============================================================================================
#2# check and clean the data, then reshape
#==============================================================================================
n_na=function(x){
  return(sum(is.na(x)))
}

sku_cast=function(sku, x, glastsku){
  xlist=sku[,c("gid", "date", x)]
  xlist=melt(xlist,id=c("gid","date"))
  xlist=dcast(xlist,gid~date)
  xlist=left_join(xlist,glastsku[,c("gid", "date", x)],by=c("gid"="gid"))
  names(xlist)=c("gid",gcol,"lastdate","m0")
  xlist=select(xlist,gid,m0,starts_with("m"),lastdate)
  xlist$numna=NA
  if (gnperiod==1) xlist$numna=as.integer(is.na(xlist[,gcol]))
  else xlist$numna=apply(xlist[,gcol],1,n_na)
  
  if (x=="sales") xlist[,"D90deals"]= apply(xlist[,c("m0",gcol)],1, function(y){return((last(y[which(!is.na(y))])))})
  else if (x=="price") xlist[,"price"]= apply(xlist[,c("m0",gcol)],1, function(y){return(mean(y ,na.rm=T))})
  else {
    xlist[,paste("delta", x, sep="")]= apply(xlist[,c("m0",gcol)],1, 
                                   function(y){return((last(y[which(!is.na(y))]))-sum(y[1],na.rm=T))})
    xlist[,paste("tot", x, sep="")]= apply(xlist[,c("m0",gcol)],1, 
                                 function(y){return((last(y[which(!is.na(y))])))})
  }
  return(xlist)
}

#------------melt and cast--------------
likelist=sku_cast(sku, "likes", glastsku); 
cmtslist=sku_cast(sku, "cmts", glastsku)
cogslist=sku_cast(sku, "COGS", glastsku)
dealist=sku_cast(sku, "sales", glastsku)
pricelist=sku_cast(sku, "price", glastsku)

#------------check--------------
table(likelist$numna)
table(cmtslist$numna)
table(dealist$numna)
table(cogslist$numna)
table(pricelist$numna)
if (any(pricelist$numna==gnperiod)) {print("Error: all price is NA")}
if (any(dealist$numna==gnperiod)) {print("Error: all sales is NA")}

head(likelist,10)
head(cmtslist,10)
head(cogslist,10)
head(pricelist,10)
head(dealist,10)


#==============================================================================================
#3# calculate the GMV and deal for SKU
#==============================================================================================
dealist$GMV=0
dealist=left_join(dealist, select(pricelist, gid, price,numna), by=c("gid"="gid"))
head(dealist[which(!(dealist$numna.x==dealist$numna.y)),])
dealist$GMV=dealist$D90deals*dealist$price
head(dealist)

#-----------------------------
# print the status 
dealist$status=gstatus[1]
dealist[which(!(dealist$gid %in% glastsku$gid)),"status"]=gstatus[2]
table(dealist$status)
# the mission samples
#a=head( dealist[(!is.na(dealist[,2]) & !is.na(dealist[,5]) & (is.na(dealist[,3])|is.na(dealist[,4]))),],100)
skup$status=gstatus[1]
skup[which(!(skup$gid %in% dealist$gid)),"status"]=gstatus[3]
table(skup$status)


#---------------------------------------------------------------------------
# skub combine
#---------------------------------------------------------------------------

skub=select(dealist,gid,lastdate,status,D90deals,GMV,price)
skub=left_join(skub,select(likelist,gid,deltalikes,totlikes),by=c("gid"="gid"))
skub=left_join(skub,select(cmtslist,gid,deltacmts,totcmts),by=c("gid"="gid"))
skub=left_join(skub,select(cogslist,gid,deltaCOGS,totCOGS),by=c("gid"="gid"))
sku=arrange(sku, desc(date))
skub=left_join(skub,distinct(select(sku,gid,gname,type1,type2,tid2,sid,grade),gid),by=c("gid"="gid"))

range(skub$GMV,na.rm=T)

skub$dealdist=cut(skub$D90deals,breaks=c(-1,0,100,1000,10000,100000,1000000),labels=c(0,100,1000,10000,100000,1000000))
skub$GMVdist=cut(skub$GMV,breaks=c(-1,0,100,1000,10000,100000,1000000,10000000),labels=c(0,100,1000,10000,100000,1000000,10000000))
skub$likedist=cut(skub$totlikes,breaks=c(-1,0,100,1000,10000,100000,1000000),labels=c(0,100,1000,10000,100000,1000000))
skub$cmtsdist=cut(skub$totcmts,breaks=c(-1,0,100,1000,10000,100000,1000000),labels=c(0,100,1000,10000,100000,1000000))
skub$COGSdist=cut(skub$totCOGS,breaks=c(-1,0,100,1000,10000,100000,1000000,10000000),labels=c(0,100,1000,10000,100000,1000000,10000000))

table(cut(skub$D90deals,breaks=c(-1,0,100,1000,10000,100000,1000000),labels=c(0,100,1000,10000,100000,1000000)))
table(cut(skub$GMV,breaks=c(-1,0,100,1000,10000,100000,1000000,10000000),labels=c(0,100,1000,10000,100000,1000000,10000000)))
table(cut(skub$totlikes,breaks=c(-1,0,100,1000,10000,100000,1000000),labels=c(0,100,1000,10000,100000,1000000)))
table(cut(skub$totcmts,breaks=c(-1,0,100,1000,10000,100000,1000000),labels=c(0,100,1000,10000,100000,1000000)))
table(cut(skub$totCOGS,breaks=c(-1,0,100,1000,10000,100000,1000000,10000000),labels=c(0,100,1000,10000,100000,1000000,10000000)))

head(skub)




#==============================================================================================
#4# Analysis SKU in diff Dimensions
#==============================================================================================

#----------------------------------------------------------------------------------------------
#1# overview : gid
#----------------------------------------------------------------------------------------------

skubsmr=data.frame(date=last(gdate),ngoods=nrow(skub),
                   D90deals=sum(skub$D90deals,na.rm=T),avgprice=mean(skub$price,na.rm=T),GMV=sum(skub$GMV,na.rm=T),
                   totlikes=sum(skub$totlikes,na.rm=T), deltalikes=sum(skub$deltalikes,na.rm=T),
                   totCOGS=sum(skub$totCOGS,na.rm=T), deltaCOGS=sum(skub$deltaCOGS,na.rm=T),
                   totcmts=sum(skub$totcmts,na.rm=T), deltacmts=sum(skub$deltacmts,na.rm=T),
                   avgdeals=mean(skub$D90deals,na.rm=T),avgGMV=mean(skub$GMV,na.rm=T),
                   avgtotlikes=mean(skub$totlikes,na.rm=T), avgdeltalikes=mean(skub$deltalikes,na.rm=T),
                   avgtotCOGS=mean(skub$totCOGS,na.rm=T), avgdeltaCOGS=mean(skub$deltaCOGS,na.rm=T),
                   avgtotcmts=mean(skub$totcmts,na.rm=T), avgdeltacmts=mean(skub$deltacmts,na.rm=T),
                   ntype1=length(unique(skub$type1)),ntype2=length(unique(skub$tid2)),nstore=length(unique(skub$sid)),
                   classic=sum(skub$status==gstatus[1]), newgoods=sum(skub$status==gstatus[2]),offshelf=sum(skup$status==gstatus[3])
                  )
#knitr::kable(as.data.frame(skubsmr),caption="SKU Summary Overview Table")

head(skubsmr)
# Deals Top 10
top10=head(as.data.frame(skub[order(desc(skub$GMV)),]),100)
top10=select(top10, gid, gname, status,type1, type2, sid,grade,  GMV, D90deals, price,totlikes, deltalikes, totcmts,deltacmts, totCOGS,deltaCOGS)
knitr::kable(as.data.frame(top10),caption="SKU Deal Number Top10 Table")

write.xlsx(file=gfile,x=t(skubsmr),sheetName = "Summary", append = T, showNA = F)
write.xlsx(file=gfile,x=top10,sheetName = "TOP100", append = T, showNA = F)

 

#----------------------------------------------------------------------------------------------
#2# overview : status     (sales=deals   dingdan)
#----------------------------------------------------------------------------------------------


sumtotdeals=sum(skub$D90deals,na.rm=T)
sumGMV=sum(skub$GMV,na.rm=T)
sumtotlikes=sum(skub$totlikes,na.rm=T)
sumtotcmts=sum(skub$totcmts,na.rm=T)
sumtotCOGS=sum(skub$totCOGS,na.rm=T)

skuanalyze=function(skub,skup,bywhat){
  
  groupsmr=summarise(skub,gdate=NA,gngoods=n(),ggoodsratio=n()/nrow(skub),gavgprice=mean(price,na.rm=T),
                     gD90deals=sum(D90deals,na.rm=T),gdealratio=sum(D90deals,na.rm=T)/sumtotdeals,
                     gGMV=sum(GMV,na.rm=T),gGMVratio=sum(GMV,na.rm=T)/sumGMV,
                     gtotlikes=sum(totlikes,na.rm=T), glikeratio=sum(totlikes,na.rm=T)/sumtotlikes , gdeltalikes=sum(deltalikes,na.rm=T),
                     gtotCOGS=sum(totCOGS,na.rm=T), gCOGSratio=sum(totCOGS,na.rm=T)/sumtotCOGS ,gdeltaCOGS=sum(deltaCOGS,na.rm=T),
                     gtotcmts=sum(totcmts,na.rm=T), gcmtsratio=sum(totcmts,na.rm=T)/sumtotcmts ,gdeltacmts=sum(deltacmts,na.rm=T),
                     gavgD90deals=mean(D90deals,na.rm=T),gavgGMV=mean(GMV,na.rm=T),
                     gavgtotlikes=mean(totlikes,na.rm=T), gavgdeltalikes=mean(deltalikes,na.rm=T),
                     gavgtotCOGS=mean(totCOGS,na.rm=T), gavgdeltaCOGS=mean(deltaCOGS,na.rm=T),
                     gavgtotcmts=mean(totcmts,na.rm=T), gavgdeltacmts=mean(deltacmts,na.rm=T)
  )
  
  names(groupsmr)=substr(names(groupsmr),2,100)
  names(groupsmr)[1]=bywhat
  #groupsmr[,bywhat]=as.character(groupsmr[,bywhat])
  if (bywhat =="status") groupsmr$status=as.character(groupsmr$status)
  avg=names(groupsmr)[grep("avg",names(groupsmr))]
  
  n=nrow(groupsmr)+1
  groupsmr[n,]=NA
  groupsmr[n,3:ncol(groupsmr)]=apply(groupsmr[1:n-1,3:ncol(groupsmr)],2,sum)
  groupsmr[n,bywhat]="Summary"
  groupsmr[n,avg]=apply((skub[,substr(avg,4,100)]),2,mean,na.rm=T)
  
  if (bywhat=="status"){
    groupsmr[n+1,]=NA
    groupsmr[n+1,bywhat]="offshelf"
    groupsmr[n+1,"ngoods"]=sum(skup$status==gstatus[3])
    
  }
  
  groupsmr$date=last(gdate)
  return(groupsmr)
}

skub=group_by(skub,status)
bywhat="status"
groupsmr=skuanalyze(skub,skup,bywhat)
as.data.frame(groupsmr)

knitr::kable(as.data.frame(groupsmr),caption=paste("SKU Analyzed By", bywhat, sep = " "))
write.xlsx(file=gfile,x=t(groupsmr),sheetName = bywhat, append = T, showNA = F)

skub=ungroup(skub)
top10=head(as.data.frame(arrange(skub[which(skub$status==gstatus[1]),],desc(GMV))),100)
top10=select(top10, gid, gname, status,type1, type2, sid,grade,  GMV, D90deals, price,totlikes, deltalikes, totcmts,deltacmts, totCOGS,deltaCOGS)
knitr::kable(as.data.frame(top10),caption=paste(gstatus[1],"SKU Deal Number Top10 Table"))
write.xlsx(file=gfile,x=top10,sheetName = "classical top10", append = T, showNA = F)

top10=head(as.data.frame(arrange(skub[which(skub$status==gstatus[2]),],desc(GMV))),100)
top10=select(top10, gid, gname, status,type1, type2, sid,grade,  GMV, D90deals, price,totlikes, deltalikes, totcmts,deltacmts, totCOGS,deltaCOGS)
knitr::kable(as.data.frame(top10),caption=paste(gstatus[2],"SKU Deal Number Top10 Table"))
write.xlsx(file=gfile,x=top10,sheetName = "new top10", append = T, showNA = F)


#----------------------------------------------------------------------------------------------
#3# overview : type1     (sales=deals   dingdan)
#----------------------------------------------------------------------------------------------
skub=group_by(skub,type1)
bywhat="type1"
groupsmr=skuanalyze(skub,skup,bywhat)
as.data.frame(groupsmr)

knitr::kable(as.data.frame(groupsmr),caption=paste("SKU Analyzed By", bywhat, sep = " "))
write.xlsx(file=gfile,x=t(groupsmr),sheetName = bywhat, append = T, showNA = F)
skub=ungroup(skub)

typelist=distinct(select(sku,type1,type2,tid2),type1,tid2)
typelsit=arrange(typelist,type1,tid2)
#knitr::kable(typelist,caption="SKU Type List Table")
write.xlsx(file=gfile,x=typelist,sheetName = "typelist", append = T, showNA = F)


#----------------------------------------------------------------------------------------------
#4# overview : grade
#----------------------------------------------------------------------------------------------
skub=group_by(skub,grade)
bywhat="grade"
groupsmr=skuanalyze(skub,skup,bywhat)
as.data.frame(groupsmr)

knitr::kable(as.data.frame(groupsmr),caption=paste("SKU Analyzed By", bywhat, sep = " "))
write.xlsx(file=gfile,x=t(groupsmr),sheetName = bywhat, append = T, showNA = F)
skub=ungroup(skub)







#==============================================================================================
#5# Analysis Store in diff Dimensions
#==============================================================================================

#----------------------------------------------------------------------------------------------
#1# store: sku by sid
#----------------------------------------------------------------------------------------------
if (T){
  skub=group_by(skub,sid)
  bywhat="sid"
  groupsmr=skuanalyze(skub,skup,bywhat)
  head(as.data.frame(groupsmr))
  
  #knitr::kable(as.data.frame(groupsmr),caption=paste("SKU Analyzed By", bywhat, sep = " "))
#  write.xlsx(file=gfile,x=t(groupsmr),sheetName = bywhat, append = T, showNA = F)
  skub=ungroup(skub)
  storesku=groupsmr
  save(file=paste(last(previous)+1,max(gdate),"groupby store.Rdata",sep="-"),storesku)
} else {
  load(file=paste(last(previous)+1,max(gdate),"groupby store.Rdata",sep="-"))
  bywhat="sid"
#  write.xlsx(file=gfile,x=t(storesku),sheetName = bywhat, append = T, showNA = F)
}


# mark the status
storesku$status=gstatus[1]
storesku[which(!(storesku$sid %in% sidp$sid)),"status"]=gstatus[2]
table(storesku$status)
sidp$status=gstatus[1]
sidp[which(!(sidp$sid %in% storesku$sid)),"status"]=gstatus[3]
table(sidp$status)


#----------------------------------------------------------------------------------------------
#2# store from the DB
#----------------------------------------------------------------------------------------------

table(store$date)
table(storesku$date)
head(store)

(gdate=unique(store$date))
(gnperiod=length(gdate))
(gstatus=factor(c("classical","new","offshelf"),levels=c("classical","new","offshelf")))
(gcol=paste("m", 1:gnperiod, sep=""))
(gcolx=paste("m", 1:gnperiod, ".x", sep=""))
(gcoly=paste("m", 1:gnperiod, ".y", sep=""))
(gfile=paste(last(previous)+1,"to",max(gdate),"MoGuJie Store Analysis By JX @", Sys.Date(),".xlsx",sep = " "))


storeb=select(store,-grade1,-kid)
storeb=left_join(storeb, select(glaststore, sid, fans, ngoods, deliverytime, cplts, rtns, sales, warranty), by=c("sid"="sid"))
storeb[,"deltafans"]=apply(storeb[,c("fans.x", "fans.y")],1,function(x){return(sum(x[1],na.rm=T)-sum(x[2],na.rm=T))})
storeb[,"deltangoods"]=apply(storeb[,c("ngoods.x", "ngoods.y")],1,function(x){return(sum(x[1],na.rm=T)-sum(x[2],na.rm=T))})
storeb[,"deltadeliverytime"]=apply(storeb[,c("deliverytime.x", "deliverytime.y")],1,function(x){return(sum(x[1],na.rm=T)-sum(x[2],na.rm=T))})
storeb[,"deltacplts"]=apply(storeb[,c("cplts.x", "cplts.y")],1,function(x){return(sum(x[1],na.rm=T)-sum(x[2],na.rm=T))})
storeb[,"deltartns"]=apply(storeb[,c("rtns.x", "rtns.y")],1,function(x){return(sum(x[1],na.rm=T)-sum(x[2],na.rm=T))})
storeb[,"deltawarranty"]=apply(storeb[,c("warranty.x", "warranty.y")],1,function(x){return(sum(x[1],na.rm=T)-sum(x[2],na.rm=T))})
storeb[,"deltasales"]=apply(storeb[,c("sales.x", "sales.y")],1,function(x){return(sum(x[1],na.rm=T)-sum(x[2],na.rm=T))})

storeb=select(storeb,date,sid, sname, city, grade2, grade3, grade4, created, auth, 
              fans=fans.x, ngoods=ngoods.x, deliverytime=deliverytime.x, cplts=cplts.x,rtns=rtns.x, totdeals=sales.x, wrranty=warranty.x,
              deltafans, deltangoods, deltadeliverytime, deltacplts, deltartns, deltadeals=deltasales,deltawarranty)

storeb=left_join(storeb, select(storesku, sid,D90deals, D90GMV=GMV, COGS=totCOGS), by=c("sid"="sid"))
storeb$created=as.Date(storeb$created)
storeb[,"status"]=gstatus[1]
storeb[which(storeb$created>last(previous) & storeb$created<=last(gdate)),"status"]=gstatus[2]
storepre=filter(glaststore, date %in% previous)
if (nrow(storepre)>0) {
  storepre$status=gstatus[1]
  storepre[which(!(storepre$sid %in% storeb$sid)),"status"]=gstatus[3]
}

head(storeb)
dim(storeb)
table(storeb$status)
table(storepre$status)

#----------------------------------------------------------------------------------------------
#3# analyze by status
#----------------------------------------------------------------------------------------------


sumtotdeals=sum(storeb$totdeals,na.rm=T)
sumtotngoods=sum(storeb$ngoods,na.rm=T)
sumtotfans=sum(storeb$fans,na.rm=T)
sumtotD90deals=sum(storeb$D90deals,na.rm=T)
sumtotD90GMV=sum(storeb$D90GMV,na.rm=T)
sumtotCOGS=sum(storeb$COGS,na.rm=T)

storeanalyze=function(storeb,storepre,bywhat){
  
  groupsmr=summarise(storeb,gdate=NA,gnstore=n(),gnstoratio=n()/nrow(storeb),
                     gtotdeals=sum(totdeals,na.rm=T),gdealratio=sum(totdeals,na.rm=T)/sumtotdeals, gdeltadeals=sum(deltadeals,na.rm=T),
                     gtotngoods=sum(ngoods,na.rm=T),gngoodsratio=sum(ngoods,na.rm=T)/sumtotngoods, gdeltangoods=sum(deltangoods,na.rm=T),
                     gtotfans=sum(fans,na.rm=T),gfansratio=sum(fans,na.rm=T)/sumtotfans, gdeltafans=sum(deltafans,na.rm=T),
                     gD90deals=sum(D90deals,na.rm=T),gD90dealratio=sum(D90deals,na.rm=T)/sumtotD90deals,
                     gD90GMV=sum(D90GMV,na.rm=T),gD90GMVratio=sum(D90GMV,na.rm=T)/sumtotD90GMV,
                     
                     gavgtotdeals=mean(totdeals,na.rm=T),gavgdeltadeals=mean(deltadeals,na.rm=T),
                     gavgngoods=mean(ngoods,na.rm=T),gavgdeltangoods=mean(deltangoods,na.rm=T),
                     gavgfans=mean(fans,na.rm=T), gavgdeltafans=mean(deltafans,na.rm=T),
                     gavgD90deals=mean(D90deals,na.rm=T),gavgD90GMV=mean(D90GMV,na.rm=T),
                     gavgdeliverytime=mean(deliverytime,na.rm=T),gavgdeltadeliverytime=mean(deltadeliverytime,na.rm=T),
                     gavgcplts=mean(cplts,na.rm=T),gavgdeltacplts=mean(deltacplts,na.rm=T),
                     gavgrtns=mean(rtns,na.rm=T),gavgdeltartns=mean(deltartns,na.rm=T),
                     gavgwrranty=mean(wrranty,na.rm=T),gavgdeltawarranty=mean(deltawarranty,na.rm=T)
  )
  
  names(groupsmr)=substr(names(groupsmr),2,100)
  names(groupsmr)[1]=bywhat
  #groupsmr[,bywhat]=as.character(groupsmr[,bywhat])
  if (bywhat =="status") groupsmr$status=as.character(groupsmr$status)
  avg=names(groupsmr)[grep("avg",names(groupsmr))]
  
  n=nrow(groupsmr)+1
  groupsmr[n,]=NA
  groupsmr[n,3:ncol(groupsmr)]=apply(groupsmr[1:n-1,3:ncol(groupsmr)],2,sum)
  groupsmr[n,bywhat]="Summary"
  groupsmr[n,avg]=apply((storeb[,substr(avg,4,100)]),2,mean,na.rm=T)
  
  if (bywhat=="status"){
    groupsmr[n+1,]=NA
    groupsmr[n+1,bywhat]="offshelf"
    groupsmr[n+1,"nstore"]=sum(storepre$status==gstatus[3],na.rm=T)
    
  }
  
  groupsmr$date=last(gdate)
  return(groupsmr)
} 

storeb=group_by(storeb,status)
bywhat="status"
groupsmr=storeanalyze(storeb,storepre,bywhat)
head(as.data.frame(groupsmr))

write.xlsx(file=gfile,x=t(groupsmr),sheetName = "status", append = T, showNA = F)


top10=head(as.data.frame(storeb[order(desc(storeb$totdeals)),]),100)
#knitr::kable(as.data.frame(top10),caption="Store totdeals Top10 Table")
write.xlsx(file=gfile,x=top10,sheetName = "store totdeals top100", append = T, showNA = F)

storeb=ungroup(storeb)
top10=head(as.data.frame(arrange(storeb[which(storeb$status==gstatus[1]),],desc(totdeals))),100)
#knitr::kable(as.data.frame(top10),caption="Classical Store GMV Top10 Table")
write.xlsx(file=gfile,x=top10,sheetName = "classic ndeal top100", append = T, showNA = F)

storeb=ungroup(storeb)
top10=head(as.data.frame(arrange(storeb[which(storeb$status==gstatus[2]),],desc(totdeals))),100)
knitr::kable(as.data.frame(top10),caption="New ndeals Top10 Table")
if (nrow(top10)>0) write.xlsx(file=gfile,x=top10,sheetName = "New ndeals top100", append = T, showNA = F)

#----------------------------------------------------------------------------------------------
#4# analyze by city
#----------------------------------------------------------------------------------------------

storeb=group_by(storeb,city)
bywhat="city"
groupsmr=storeanalyze(storeb,storepre,bywhat)
head(as.data.frame(groupsmr))
#table(store$city)

groupsmr=ungroup(groupsmr)
groupsmr=arrange(groupsmr,desc(nstore))
write.xlsx(file=gfile,x=groupsmr,sheetName = bywhat, append = T, showNA = F)


#----------------------------------------------------------------------------------------------
#5# analyze by grade2 quality
#----------------------------------------------------------------------------------------------

storeb=group_by(storeb,grade2)
bywhat="grade2"
groupsmr=storeanalyze(storeb,storepre,bywhat)
head(as.data.frame(groupsmr))
#table(store$grade2)
groupsmr=ungroup(groupsmr)
groupsmr=arrange(groupsmr,desc(grade2))
write.xlsx(file=gfile,x=groupsmr,sheetName = bywhat, append = T, showNA = F)

storeb=group_by(storeb,grade3)
bywhat="grade3"
groupsmr=storeanalyze(storeb,storepre,bywhat)
head(as.data.frame(groupsmr))
#table(store$grade3)
groupsmr=ungroup(groupsmr)
groupsmr=arrange(groupsmr,desc(grade3))
write.xlsx(file=gfile,x=groupsmr,sheetName = bywhat, append = T, showNA = F)

storeb=group_by(storeb,grade4)
bywhat="grade4"
groupsmr=storeanalyze(storeb,storepre,bywhat)
head(as.data.frame(groupsmr))
table(store$grade4)
groupsmr=ungroup(groupsmr)
groupsmr=arrange(groupsmr,desc(grade4))
write.xlsx(file=gfile,x=groupsmr,sheetName = bywhat, append = T, showNA = F)










#==============================================================================================
#6# Analysis tuangou in diff Dimensions
#==============================================================================================

#----------------------------------------------------------------------------------------------
#1# store: sku by sid
#----------------------------------------------------------------------------------------------

head(tuan)
table(tuan$date)
head(glasttuan)
tuan=distinct(arrange(tuan,desc(date)), date, gid)

#----------------------------------------------------------------------------------------------
# Define the Macros Variables
#(previous=max(glasttuan$date))
(previous)
(gdate=unique(tuan$date))
(gdate=gdate[order(as.integer(gdate))])
(gnperiod=length(gdate))
(gstatus=factor(c("classical","new","offshelf"),levels=c("classical","new","offshelf")))
(gcol=paste("m", 1:gnperiod, sep=""))
(gcolx=paste("m", 1:gnperiod, ".x", sep=""))
(gcoly=paste("m", 1:gnperiod, ".y", sep=""))
(gfile=paste(last(previous)+1,"to",max(gdate),"MoGuJie tuan Analysis By JX @", Sys.Date(),".xlsx",sep = " "))
#tuan



#----------------------------------------------------------------------------------------------
#2 check and clean the data, then reshape
n_na=function(x){
  return(sum(is.na(x)))
}

tuan_cast=function(tuan, x, glasttuan){
  xlist=tuan[,c("gid", "date", x)]
  xlist=melt(xlist,id=c("gid","date"))
  xlist=dcast(xlist,gid~date)
  xlist=left_join(xlist,glasttuan[,c("gid", "date", x)],by=c("gid"="gid"))
  names(xlist)=c("gid",gcol,"lastdate","m0")
  xlist=select(xlist,gid,m0,starts_with("m"),lastdate)
  xlist$numna=NA
  if (gnperiod==1) xlist$numna=as.integer(is.na(xlist[,gcol]))
  else xlist$numna=apply(xlist[,gcol],1,n_na)

  if (x=="price") xlist[,"price"]= apply(xlist[,c("m0",gcol)],1, function(y){return(mean(y ,na.rm=T))})
  else {
    
    xlist[,paste("delta", x, sep="")]= apply(xlist[,c("m0",gcol)],1, 
                                             function(y){return((last(y[which(!is.na(y))]))-sum(y[1],na.rm=T))})
    xlist[,paste("tot", x, sep="")]= apply(xlist[,c("m0",gcol)],1, 
                                           function(y){return((last(y[which(!is.na(y))])))})
  }
  return(xlist)
}

#------------melt and cast--------------
dealist=tuan_cast(tuan, "sales", glasttuan)
pricelist=tuan_cast(tuan, "price", glasttuan)
head(pricelist)
head(dealist)

#------------check--------------
price_equal=as.data.frame(apply(select(pricelist,starts_with("m")), 1, function(x){
  if (sum(is.na(x))==length(x)) return(1)
  if (is.na(mean(x, na.rm=T))) return(2)
  if (all(mean(x, na.rm=T)==x[!is.na(x)], na.rm=T)) return (3)
  else {print(4);print(x);return(4)}
}))
sum(price_equal==gnperiod)
dim(price_equal)

table(dealist$numna)
table(pricelist$numna)
if (any(pricelist$numna==gnperiod)) {print("Error: all price is NA")}
if (any(dealist$numna==gnperiod)) {print("Error: all sales is NA")}

#----------------------------------------------------------------------------------------------
#3 calculate the GMV and deal for SKU
dealist$GMV=0
dealist=left_join(dealist, select(pricelist, gid, price,numna), by=c("gid"="gid"))
head(dealist[which(!(dealist$numna.x==dealist$numna.y)),])
dealist$GMV=dealist$deltasales*dealist$price
head(dealist)

#-----------------------------
# print the status 
dealist$status=gstatus[1]
dealist[which(!(dealist$gid %in% glasttuan$gid)),"status"]=gstatus[2]
table(dealist$status)
tuanp=filter(glasttuan, date %in% previous)
dim(tuanp)
tuanp$status=gstatus[1]
tuanp[which(!(tuanp$gid %in% dealist$gid)),"status"]=gstatus[3]
table(tuanp$status)
head(dealist,10)

#---------------------------------------------------------------------------
# tuanb combine
#---------------------------------------------------------------------------
tuanb=select(dealist,gid,lastdate,status,deltasales,totsales,GMV,price)
tuan=arrange(tuan, desc(date))
tuanb=left_join(tuanb,distinct(select(tuan,gid,gname),gid),by=c("gid"="gid"))
names(tuanb)[4:5]=c("deltadeals", "totdeals")
head(tuanb)

#----------------------------------------------------------------------------------------------
#2# overview : status     (sales=deals   dingdan)
#----------------------------------------------------------------------------------------------

sumtotdeals=sum(tuanb$totdeals,na.rm=T)
sumdeltadeals=sum(tuanb$deltadeals,na.rm=T)
sumGMV=sum(tuanb$GMV,na.rm=T)

tuananalyze=function(tuanb,tuanp,bywhat){
  groupsmr=summarise(tuanb,gdate=NA,gngoods=n(),ggoodsratio=n()/nrow(tuanb),gavgprice=mean(price,na.rm=T),
                     gdeltadeals=sum(deltadeals,na.rm=T),gdeltadealratio=sum(deltadeals,na.rm=T)/sumdeltadeals,
                     gGMV=sum(GMV,na.rm=T),gGMVratio=sum(GMV,na.rm=T)/sumGMV,
                     gtotdeals=sum(totdeals,na.rm=T),gtotdealratio=sum(totdeals,na.rm=T)/sumtotdeals,
                     gavgdeltadeals=mean(deltadeals,na.rm=T),gavgGMV=mean(GMV,na.rm=T),
                     gavgtotdeals=mean(totdeals,na.rm=T)
  )                     
  
  names(groupsmr)=substr(names(groupsmr),2,100)
  names(groupsmr)[1]=bywhat
  #groupsmr[,bywhat]=as.character(groupsmr[,bywhat])
  if (bywhat =="status") groupsmr$status=as.character(groupsmr$status)
  avg=names(groupsmr)[grep("avg",names(groupsmr))]
  
  n=nrow(groupsmr)+1
  groupsmr[n,]=NA
  groupsmr[n,3:ncol(groupsmr)]=apply(groupsmr[1:n-1,3:ncol(groupsmr)],2,sum)
  groupsmr[n,bywhat]="Summary"
  groupsmr[n,avg]=apply((tuanb[,substr(avg,4,100)]),2,mean,na.rm=T)
  
  if (bywhat=="status"){
    groupsmr[n+1,]=NA
    groupsmr[n+1,bywhat]="offshelf"
    groupsmr[n+1,"ngoods"]=sum(skup$status==gstatus[3])
    
  }
  
  groupsmr$date=last(gdate)
  return(groupsmr)
}

tuanb=group_by(tuanb,status)
bywhat="status"
groupsmr=tuananalyze(tuanb,tuanp,bywhat)
as.data.frame(groupsmr)

knitr::kable(as.data.frame(groupsmr),caption=paste("Tuan Analyzed By", bywhat, sep = " "))
write.xlsx(file=gfile,x=t(groupsmr),sheetName = bywhat, append = T, showNA = F)




