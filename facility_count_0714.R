library(data.table)
library(xlsx)
library(gtools)
library(knitr)
library(reporttools)
library(xtable)
library(dplyr)
library(reshape)
library(plyr)

setwd("E:/job/interm/CSHO_data/")
##Read into the dataset for quick check;
data_2010 <- read.delim(file=paste("1F0400_2010_upd.txt", sep=""), header=TRUE, sep=",")
data_2011 <- read.delim(file=paste("1F0400_2011_upd.txt", sep=""), header=TRUE, sep=",")
data_2012 <- read.delim(file=paste("1F0400_2012_upd.txt", sep=""), header=TRUE, sep=",")
data_2013 <- read.delim(file=paste("1F0400_2013_upd.txt", sep=""), header=TRUE, sep=",")
data_2014 <- read.delim(file=paste("1F0400_2014_upd.txt", sep=""), header=TRUE, sep=",")
data_2015 <- read.delim(file=paste("1F0400_2015_fullexport.txt", sep=""), header=TRUE, sep=",")

fac_2010 <- read.delim(file=paste("1F0400_2010_facilityID_CoC_abstractrecs.txt", sep=""), header=TRUE, sep=",")
fac_2011 <- read.delim(file=paste("1F0400_2011_facilityID_CoC_abstractrecs.txt", sep=""), header=TRUE, sep=",")
fac_2012 <- read.delim(file=paste("1F0400_2012_facilityID_CoC_abstractrecs.txt", sep=""), header=TRUE, sep=",")
fac_2013 <- read.delim(file=paste("1F0400_2013_facilityID_CoC_abstractrecs.txt", sep=""), header=TRUE, sep=",")
fac_2014 <- read.delim(file=paste("1F0400_2014_facilityID_CoC_abstractrecs.txt", sep=""), header=TRUE, sep=",")

join_2010<-merge(x = data_2010, y = fac_2010, by = c("PatientID","DxDate","PSite"), all= TRUE)
join_2011<-merge(x = data_2011, y = fac_2011, by = c("PatientID","DxDate","PSite"), all= TRUE)
join_2012<-merge(x = data_2012, y = fac_2012, by = c("PatientID","DxDate","PSite"), all= TRUE)
join_2013<-merge(x = data_2013, y = fac_2013, by = c("PatientID","DxDate","PSite"), all= TRUE)
join_2014<-merge(x = data_2014, y = fac_2014, by = c("PatientID","DxDate","PSite"), all= TRUE)

names(data_2015)[dim(data_2015)[2]] <- "FacilityName"
data_2010_2015 <- smartbind(join_2011, join_2012, join_2013, join_2014, data_2015)


#recode county name
data_2010_2015$County<- recode(data_2010_2015$DxCounty, '1'= "Adams",	'69'="Lackawanna",'3'= "Allegheny",'71'="Lancaster",'5'="Armstrong",'73'="Lawrence",
                               '7'="Beaver",'75'="Lebanon",'9'="Bedford",'77'="Lehigh",'11'="Berks",'79'="Luzerne",'13'="Blair",'81'="Lycoming",'15'="Bradford",	
                               '83'="McKean",'17'="Bucks",'85'="Mercer",'19'="Butler",'87'="Mifflin",'21'="Cambria",	'89'="Monroe",'23'="Cameron",	'91'="Montgomery",
                               '25'="Carbon",		'93'="Montour",'27'="Centre",'95'="Northampton",'29'="Chester",	'97'="Northumberland",'31'="Clarion",	'99'="Perry",
                               '33'="Clearfield",'101'="Philadelphia",'35'="Clinton",'103'="Pike",'37'="Columbia",'105'="Potter",'39'="Crawford", '107'="Schuylkill",
                               '41'="Cumberland", '109'="Snyder",'43'="Dauphin",'111'="Somerset",'45'="Delaware", '113'="Sullivan",'47'="Elk", '115'="Susquehanna",
                               '49'="Erie", '117'="Tioga",'51'="Fayette", '119'="Union",'53'="Forest", '121'="Venango",'55'="Franklin",'123'="Warren", '57'="Fulton",'125'="Washington",
                               '59'="Greene", '127'="Wayne", '61'="Huntingdon", '129'="Westmoreland",'63'="Indiana", '131'="Wyoming",'65'="Jefferson",'133'="York",'67'="Juniata")


## transform age to agegroup
agebreaks <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,200)
agelabels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
               "70-74","75-79","80-84","85+")
setDT(data_2010_2015)[ ,agegroups := cut(AgeDx, breaks = agebreaks,right = FALSE,labels = agelabels)]

#########define specific cancer types: PSite=C500-C509 for female breast; PSite=C619 for male prostate; PSite=C340-C349 for lung/Bronchus; PSite=C180-C189, C199, C209, C260 while excluding ((histtypeicdo3>=9050 & histtypeicdo3<=9055) | histtypeicdo3==9140 | (histtypeicdo3>=9590 & histtypeicdo3<=9992)) for Colorectal; PSite=C000-C148 for Oral Cavity/Pharynx; PSite=C569 for female Ovary; PSite=C739 for Thyroid; PSite=C530-C539 for female Cervix. 
###note that sex=1 for male and sex=2 for female;
data_2010_2015$exclude <- (data_2010_2015$HistTypeICDO3>=9050 & data_2010_2015$HistTypeICDO3<=9055) | data_2010_2015$HistTypeICDO3==9140 | (data_2010_2015$HistTypeICDO3>=9590 & data_2010_2015$HistTypeICDO3<=9992)

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C180","C181","C182","C183","C184","C185","C186","C187","C188")|data_2010_2015$PSite %in% c("C189","C260","C199","C209") & data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "colorectal"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C500","C501","C502","C503","C504","C505","C506","C507","C508","C509")& data_2010_2015$exclude!=1 & data_2010_2015$Sex!=1 & data_2010_2015$BehaviorICDO3==3] <- "breast"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C619")& data_2010_2015$exclude!=1 & data_2010_2015$Sex!=2 & data_2010_2015$BehaviorICDO3==3] <- "prostate"

data_2010_2015$Cancer[(data_2010_2015$PSite %in% c("C710","C711","C712","C713","C714","C715","C716","C717","C718","C719","C700","C701","C702","C703","C704","C705","C706","C707","C708","C720","C721","C722","C723","C724","C725","C726","C727","C728","C719","C709","C729") & data_2010_2015$exclude!=1)|(data_2010_2015$PSite %in% c("C710","C711","C712","C713","C714","C715","C716","C717","C718","C719") & data_2010_2015$HistTypeICDO3 >=9530 & data_2010_2015$HistTypeICDO3 <=9539) & data_2010_2015$BehaviorICDO3==3] <- "brain_oth_nervous"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C530","C531","C532","C533","C534","C535","C536","C537","C538","C539")& data_2010_2015$exclude!=1 & data_2010_2015$Sex!=1 & data_2010_2015$BehaviorICDO3==3] <- "cervical"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C540","C541","C542","C543","C544","C545","C546","C547","C548","C549","C559")& data_2010_2015$exclude !=1 & data_2010_2015$BehaviorICDO3==3] <- "corpus_uterus"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C150","C151","C152","C153","C154","C155","C156","C157","C158","C159")& data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "esophagus"

data_2010_2015$Cancer[data_2010_2015$HistTypeICDO3>=9650 & data_2010_2015$HistTypeICDO3 <=9667 & data_2010_2015$BehaviorICDO3==3] <- "hodgkin_lymph"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C649","C659")& data_2010_2015$exclude!=1  & data_2010_2015$BehaviorICDO3==3] <- "kidney_renal_pelvis"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C320","C321","C322","C323","C324","C325","C326","C327","C328","C329")& data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "larynx"

data_2010_2015$Cancer[(data_2010_2015$PSite %in% c("C420","C421","C424") & data_2010_2015$HistTypeICDO3 %in% c(9811,9812,9813,9814,9815,9816,9817,9818,9837,9823,9827))|data_2010_2015$HistTypeICDO3 %in% c(9826,9835,9836,9820,9832,9833,9834,9940,9840,9861,9865,9866,9867,9869,9871,9872,9873,9874,9895,9896,9897,9898,9910,9911,9920,9891,9863,9875,9876,9945,9946,9860,9930,9801,9805,9806,9807,9808,9809,9931,9733,9742,9800,9831,9870,9948,9963,9964) & data_2010_2015$BehaviorICDO3==3] <- "leukemia"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C340","C341","C342","C343","C344","C345","C346","C347","C348","C349")& data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "lung_bronchus"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C220","C221")& data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "liver_ibd"

data_2010_2015$Cancer[data_2010_2015$HistTypeICDO3 %in% c(9731,9732,9734) & data_2010_2015$BehaviorICDO3==3] <- "myeloma"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C440","C441","C442","C443","C444","C445","C446","C447","C448","C449")& data_2010_2015$HistTypeICDO3>=8720 & data_2010_2015$HistTypeICDO3<=8790 & data_2010_2015$BehaviorICDO3==3] <- "melanoma"

data_2010_2015$Cancer[data_2010_2015$HistTypeICDO3 >=9590 & data_2010_2015$HistTypeICDO3 <=9597 | data_2010_2015$HistTypeICDO3 %in% c(9670,9671,9673,9675,9678,9679,9680,9684,9687,9688,9689,9690,9691,9695,9698,9699,9700,9701,9702,9705,9708,9709,9712,9714,9715,9716,9717,9718,9719,9724,9725,9726,9727,9728,9729,9735,9737,9738) |(! data_2010_2015$PSite %in% c("C421","C424")& data_2010_2015$HistTypeICDO3 %in% c(9811,9812,9813,9814,9815,9816,9817,9818,9823,9827,9837))& data_2010_2015$BehaviorICDO3==3] <- "non_hodgkin_lymphoma"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C000","C001","C002","C003","C004","C005","C006","C007","C008","C009","C019","C029","C049","C059","C069","C079","C089","C039","C099","C109","C119","C129","C139","C020","C021","C022","C023","C024","C025","C026","C027","C028","C080","C081","C082","C083","C084","C085","C086","C087","C088","C040","C041","C042","C043","C044","C045","C046","C047","C048","C030","C031","C032","C033","C034","C035","C036","C037","C038","C050","C051","C052","C053","C054","C055","C056","C057","C058","C060","C061","C062","C063","C064","C065","C066","C067","C068","C110","C111","C112","C113","C114","C115","C116","C117","C118","C090","C091","C092","C093","C094","C095","C096","C097","C098","C100","C101","C102","C103","C104","C105","C106","C107","C108","C130","C131","C132","C133","C134","C135","C136","C137","C138","C140","C142","C148") & data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "oral_pharyn"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C569")& data_2010_2015$exclude!=1 & data_2010_2015$Sex!=1 & data_2010_2015$BehaviorICDO3==3] <- "ovary"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C250","C251","C252","C253","C254","C255","C256","C257","C258","C259")& data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "pancreas"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C160","C161","C162","C163","C164","C165","C166","C167","C168","C169")& data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "stomach"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C620","C621","C622","C623","C624","C625","C626","C627","C628","C629")& data_2010_2015$exclude!=1 & data_2010_2015$Sex!=2 & data_2010_2015$BehaviorICDO3==3] <- "testis"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C739")& data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "thyroid"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C670","C671","C672","C673","C674","C675","C676","C677","C678","C679")& data_2010_2015$exclude!=1] <- "bladder"

####
####
####
#export data for future use##
write.csv(data_2010_2015, file = 'E:/job/interm/CSHO_data/DT_f5.csv')
#import the data
setwd("E:/job/interm/CSHO_data/")
DT_f<- read.csv(file = 'DT_f5.csv', na.strings=c(" ","","NA"))

catchment_name <- c("Adams", "Berks", "Blair", "Carbon","Centre","Clinton","Columbia","Cumberland","Dauphin","Franklin",
                    "Fulton","Huntingdon", "Juniata", "Lancaster", "Lebanon", "Lehigh", "Luzerne", "Lycoming", "Mifflin",
                    "Montour","Northumberland", "Perry", "Schuylkill", "Snyder", "Sullivan", "Union", "Wyoming","York")
DT_f$catchment<- ifelse(DT_f$County %in% catchment_name,"catchment","noncatch")
# remove duplicate id 
sDT_f<-unique(setDT(DT_f),by=c("PatientID","PSite","DxDate","FacilityName"))
# only serious case
#limit to serious case--> sDT is a data set with only serious cases and no replicated tumor
sDT_f$ICO3<-ifelse(sDT_f$Cancer=="bladder"& (sDT_f$BehaviorICDO3==2|sDT_f$BehaviorICDO3==3),1,ifelse(sDT_f$BehaviorICDO3==3,1,0))
sDT_f<-subset(sDT_f,sDT_f$ICO3==1)

sDT_fc<-subset(sDT_f,sDT_f$catchment=="catchment")


#test if missing at random
###########################

DT_f<-sDT_f

levels <- levels(DT_f$FacilityName )
levels[length(levels) + 1] <- "None"

# refactor Species to include "None" as a factor level
# and replace NA with "None"
DT_f$FacilityName <- factor(DT_f$FacilityName, levels = levels)
DT_f$FacilityName[is.na(DT_f$FacilityName)] <- "None" 

test<-melt(table(County=DT_f$County,Facility=DT_f$FacilityName,exclude=NULL))

test$data<-ifelse(test$Facility=="None","None","data")

test<-aggregate(test$value,by=list(test$County,test$data), FUN="sum")
test<-dcast(data = test,formula = Group.1 ~Group.2,fun.aggregate = sum,value.var = "x")
test$MissingRate<-with(test,None/(data+None)*100)
test
sum(test$No)/sum(test$data+test$None)*100

#i) Hershey (correct I think Penn State is only one in Hershey)
#ii) Fox Chase looks good
#iii) UPMC_1 (define as having UPMC and PIttsburgh)
#iv) UPMC_2 (any UPMC as you had)
#v) Penn (Penn Presbyterian Medical Ctr, Pennsylvania Hospital, Hospital of the Univ of PA)
#vi) Geisinger_1 (Geisinger in Danville)
#vii) Geisinger_2 (any Geisinger as you have)

#DT_f<-subset(sDT_f,sDT_f$Cancer=="melanoma")
DT_f<-sDT_f

levels <- levels(DT_f$FacilityName )
levels[length(levels) + 1] <- "None"

# refactor Species to include "None" as a factor level
# and replace NA with "None"
DT_f$FacilityName <- factor(DT_f$FacilityName, levels = levels)
DT_f$FacilityName[is.na(DT_f$FacilityName)] <- "None" 


DT_f$hospital[DT_f$FacilityName %like% "UPMC"] <-"UPMC_2"
DT_f$hospital2[DT_f$FacilityName %like% "UPMC"& DT_f$FacilityName %like% "Pittsburgh"]<-"UPMC_1"
DT_f$hospital[DT_f$FacilityName %like% "Hershey"] <-"Hershey"
DT_f$hospital[DT_f$FacilityName %like% "Geisinger"] <-"Geisinger_2"
DT_f$hospital2[DT_f$FacilityName %like% "Danville Geisinger"]<-"Geisinger_1"
DT_f$hospital[DT_f$FacilityName %like% "Fox Chase"] <-"Fox Chase"
DT_f$hospital[DT_f$FacilityName %like% "Penn Presbyterian Medical Ctr"|DT_f$FacilityName %like% "Pennsylvania Hospital"| DT_f$FacilityName %like% "Hospital of the Univ of PA"]<- "Penn"
DT_f$hospital[DT_f$FacilityName =="None"]<-"None"

a<-table(County=DT_f$County,hospital=DT_f$hospital,exclude=NULL)
b<-table(County=DT_f$County,hospital=DT_f$hospital2,exclude=NULL)
c<-cbind(a,b)
c<-c[,c(3,5,1,2,8,6,9,7,4)]

#############################
### fraction adjusted
#############################

DT_f$n<-with(sDT_f,1)
n.1<-aggregate(DT_f$n,by=list(PatientID=DT_f$PatientID,PSite=DT_f$PSite,DxDate=DT_f$DxDate,FacilityName=DT_f$FacilityName),FUN="sum")
n.2<-aggregate(DT_f$n,by=list(PatientID=DT_f$PatientID,DxDate=DT_f$DxDate,PSite=DT_f$PSite),FUN="sum")
DT_f<-merge(DT_f,n.1,by=c("PatientID","PSite","DxDate","FacilityName"))
DT_f<-merge(DT_f,n.2,by=c("PatientID","PSite","DxDate"))
DT_f$aaValue<-with(DT_f,n/x.y)

test<- DT_f[order(-x.y),]
test<-subset(test,test$PatientID==131453)
test<-test[,c("PatientID","PSite","DxDate","FacilityName","n","x.y","aaValue")]
test<-aggregate(test$aaValue ,by=list(Facility=test$FacilityName),FUN="sum")

#DT_f<-subset(DT_f,DT_f$Cancer=="melanoma")

t<-aggregate(DT_f$aaValue,by=list(County=DT_f$County,hospital=DT_f$hospital),FUN="sum")
t1<-aggregate(DT_f$aaValue,by=list(County=DT_f$County,hospital=DT_f$hospital2),FUN="sum")
t_county<-aggregate(DT_f$aaValue,by=list(COunty=DT_f$County),FUN="sum")
tc<-rbind(t,t1)

tc<-xtabs(tc$x~tc$County + tc$hospital ,tc)
tc<-tc[,c(4,6,1,3,2,8,7,5)]

#write.csv(c, file = 'E:/job/interm/melanoma/hospital_pa_nonadj_melanoma_0714.csv')
write.csv(tc, file = 'E:/job/interm/melanoma/hospital_pa_adj_melanoma_0714.csv')

#################################################
#top ten facility patient going cross county
#################################################

DT_f1<-DT_f
#DT_f1<-subset(DT_f,DT_f$Cancer=="melanoma")
af<-aggregate(DT_f1$aaValue,by=list(County=DT_f1$County,facility=DT_f1$FacilityName),FUN="sum")
colnames(af)[3]<-"value"
#af<-melt(table(County=DT_f1$County,facility=DT_f1$FacilityName,exclude=NULL))

#require(plyr)
af10<-ddply(af, c("County"), function(x) head(x[order(x$value, decreasing = TRUE) , ], 10))
ofc<-ddply(af, c("County"), function(x) head(x[order(x$value),], 335))
ofc<-aggregate(ofc$value,by=list(ofc$County),FUN=sum)
ofc$facility<-"Others"
colnames(ofc)[1]<-"County"
colnames(ofc)[2]<-"value"
r<-rbind(af10,ofc)

tem1<-aggregate(r$value,by=list(r$County),FUN="sum")
tem1$facility<-"Total"
colnames(tem1)[1]<-"County"
colnames(tem1)[2]<-"value"
r<-rbind(r,tem1)
colnames(tem1)[2]<-"x"
tem1<-tem1[,-c(3)]
r<-merge(r,tem1,list=c("County"))
r$percentage<-with(r,value/x*100)
r<-r[,-c(4)]
r<-r[order(r$County,r$facility),]

write.csv(r, file = 'E:/job/interm/melanoma/Top10Facility_0715.csv')

############################################################
##average number facility per patient visits cross county
############################################################
DT_f1<-DT_f
DT_f1<-subset(DT_f,DT_f$Cancer=="melanoma")

################ For stage grouping ############

#DT_f1$stage<-ifelse(DT_f1$DerivedSS2000==0,"in-situ",
                  ifelse(DT_f1$DerivedSS2000==1,"local",
                           ifelse(DT_f1$DerivedSS2000 %in% c(2,3,4,5),"regional",
                                  ifelse(DT_f1$DerivedSS2000==7,"distant",
                                         ifelse(DT_f1$DerivedSS2000==9,"unknown","others")))))

#DT_f1<-subset(DT_f1,DT_f1$stage=="distant")

DT_f1$n<-with(DT_f1,1)
#all cancer
n.1<-aggregate(DT_f1$n,by=list(County=DT_f1$County,PatientID=DT_f1$PatientID,PSite=DT_f1$PSite,DxDate=DT_f1$DxDate),FUN="sum")
#melanoma
n.2<-aggregate(DT_f1$n,by=list(County=DT_f1$County,PatientID=DT_f1$PatientID,PSite=DT_f1$PSite,DxDate=DT_f1$DxDate),FUN="sum")
t.test(n.1$x,n.2$x)

######################3
#for patients go to hershey
###########################
#DT_f1<-DT_f
DT_f1<-subset(DT_f,DT_f$Cancer=="melanoma")
DT_f1$n<-1
DT_f2<-subset(DT_f1,DT_f1$FacilityName %like% "Hershey")
DT_f2$n<-with(DT_f2,1)
n.1<-aggregate(DT_f1$n,by=list(County=DT_f1$County,PatientID=DT_f1$PatientID,PSite=DT_f1$PSite,DxDate=DT_f1$DxDate,Facility=DT_f1$FacilityName),FUN="sum")
######################3
n.2<-aggregate(DT_f2$n,by=list(County=DT_f2$County,PatientID=DT_f2$PatientID,PSite=DT_f2$PSite,DxDate=DT_f2$DxDate),FUN="sum")
n.3<-merge(n.1,n.2,list=c("PatientID","PSite","DxDate"))
#all cancer
n.1<-aggregate(n.3$x,by=list(County=n.3$County,PatientID=n.3$PatientID,PSite=n.3$PSite,DxDate=n.3$DxDate),FUN="sum")
#melanoma
n.2<-aggregate(n.3$x,by=list(County=n.3$County,PatientID=n.3$PatientID,PSite=n.3$PSite,DxDate=n.3$DxDate),FUN="sum")
t.test(n.1$x,n.2$x)
######################
###############################

####checking if mean make sense############# 

#n.facility<-aggregate(n.1$x,by=list(County=n.1$County),FUN="sum")
#total<-merge(n.facility,patient,list=("County"))
#total$average<-with(total,x/value)

mean<-aggregate(x ~ County, data = n.1, FUN="mean")
max<-aggregate(x ~ County, data = n.1, FUN="max")
min<-aggregate(x ~ County, data = n.1, FUN="min")
median<-aggregate(x ~ County, data=n.1,FUN="median")
summary<-merge(mean,min,by="County")
summary<-merge(summary,max,by="County")
summary<-merge(summary,median,by="County")
colnames(summary)[2]<-"mean"
colnames(summary)[3]<-"min"
colnames(summary)[4]<-"max"
colnames(summary)[5]<-"median"
x<-t(t(county_name))
colnames(x)[1]<-"County"
test<-merge(x,summary,list=("county"),all=TRUE)
test<-test[order(test$County),]
View(test)
View(summary)
summary(n.1$x)
##############################################
##for total
n.1<-aggregate(DT_f1$n,by=list(PatientID=DT_f1$PatientID,PSite=DT_f1$PSite,DxDate=DT_f1$DxDate),FUN="sum")
summary(n.1$x)

####################
###number of stage
####################

DT_fx<-unique(setDT(DT_f1),by=c("PatientID","PSite","DxDate"))
test<-melt(table(County=DT_fx$County,Stage=DT_fx$stage))
test<-xtabs(test$value~test$County + test$Stage,test)

write.csv(test, file = 'E:/job/interm/melanoma/test.csv')

#############################################
#######for percentage of go to 5,4,3,2,1 facility#########
######################################################

sn<-subset(n.1,n.1$x==2)
a<-melt(table(county=sn$County))
View(a)

test<-n.1[order(-n.1$x),]
head(test)
test<-subset(DT_f2,DT_f2$PatientID== 1996680)
test

#NOTE the oder is somehow incorrect. need to be fixed 
#install.packages("magicfor")
#library(magicfor)               # load library
#library(dsrTest)

#magic_for(silent = TRUE)

#for (y in c(1,2,3,4,5)){
#  sn<-subset(n.1,n.1$x==y)
#  a<-melt(table(County=sn$County))
#    put (a$County,a$value)
#}

#test<-magic_result_as_dataframe()     # get the result
#colnames(test)[2]<-"County"
#colnames(test)[3]<-"value"
#test$y<-as.character(test$y)
#test<-xtabs(test$value~test$County + test$y,test)


##this is for total number of cases
##########
patient<-melt(table(County=n.1$County))

test<-subset(n.1,n.1$PatientID==131453)
test
