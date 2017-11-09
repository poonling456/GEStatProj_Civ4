rawdata <- read.csv("201617_KaiMing.csv", header = TRUE, sep = ",", na.strings=c("NA", "BLANK", "MULT", "ERROR #3100", "#VALUE!", "Not Asked"), check.names = FALSE, stringsAsFactors = FALSE)
rawdata <- rawdata[which(!is.na(rawdata[,1])),]

#Quantifying Q18-Q23, borrowed from Danny
quantify <- function(response, choice, converted_num) {
  sapply(as.factor(response), function(x) converted_num[which(x == choice)])
}


part1=rawdata[,5:55]
part2to3=rawdata[,56:73]

OD=cbind("OD",rawdata[which(is.na(rawdata$Q41)),111:115])
Civ4=cbind("Civ4",rawdata[which(!is.na(rawdata$Q41)),116:120])
names(OD)=c("Outside Class Activity","LA1","LA2","LA3","LA4","LA5")
names(Civ4)=c("Outside Class Activity","LA1","LA2","LA3","LA4","LA5")
part4=rbind(Civ4,OD)

grade=rawdata[,94:97]


#demographic
game=factor(is.na(rawdata$Q41),levels=c(T,F),labels = c("OD","Civ4"))
sex=factor(rawdata$Sex)
eng=factor(rawdata$`English Proficiency`,levels = c("Others", "DSE 4 or below", "DSE 5 or above" ))
fac=factor(rawdata$Faculty)
coll=factor(rawdata$College)
year=factor(rawdata$`Year of Study`)
first=factor(rawdata$`First GEF?`)

demographic=data.frame("Outside Class Activity"=game,"Sex"=sex,"DSE English Grade"=eng, "Faculty"=fac, "College"=coll, "Year of Study"=year, "First GEF"=first, check.names = F)
elec=rawdata[,75:88]


#Quantification
#questionaire Q18
option <- c("0 to 1", "2 to 3", "4 to 5", "6 to 7", "8 to 9", "10 to 11")
num <- c(0.5, 2.5, 4.5, 6.5, 8.5, 10.5)
part2to3$`Q18 (Assigned Text Read Completely)`=factor(part2to3$`Q18 (Assigned Text Read Completely)`,option,num)
part2to3$`Q18 (Assigned Text Read Completely)`=num[part2to3$`Q18 (Assigned Text Read Completely)`]

#questionaire Q19
option <- c("0 to 1", "2 to 3", "4 to 5", "6 to 7", "8 to 9", "10 to 11")
num <- c(0.5, 2.5, 4.5, 6.5, 8.5, 10.5)
part2to3$`Q19 (Chinese Translation)`=factor(part2to3$`Q19 (Chinese Translation)`,option,num)
part2to3$`Q19 (Chinese Translation)`=num[part2to3$`Q19 (Chinese Translation)`]

#questionaire Q20
replace=c( "1 to < 3 hrs","3 to < 6 hrs","6 to < 10 hrs","10 to < 15 hrs")
with=c( "1 to <3","3 to <6","6 to <10","10 to <15")
part2to3$`Q20 (Time/RJ)`=sapply(part2to3$`Q20 (Time/RJ)`,function(x) {ifelse(x%in%replace,with[which(replace%in%x)],x)})
option <- c("less than 1", "1 to <3", "3 to <6", "6 to <10", "10 to <15", "more than 15")
num <- c(0.5, 2, 4.5, 8, 12.5, 16)
response <- part2to3$`Q20 (Time/RJ)`
part2to3$`Q20 (Time/RJ)`=quantify(response, option, num)


#questionaire Q21
replace=c( "1 to < 2 hrs","2 to < 3 hrs","3 to < 4 hrs","4 to < 5 hrs")
with=c( "1 to <2","2 to <3","3 to <4","4 to <5")
part2to3$`Q21 (Time/reading)`=sapply(part2to3$`Q21 (Time/reading)`,function(x) {ifelse(x%in%replace,with[which(replace%in%x)],x)})
option <- c("less than 1", "1 to <2", "2 to <3", "3 to <4", "4 to <5", "more than 5")
num <- c(0.5, 1.5, 2.5, 3.5, 4.5, 6)
response <- part2to3$`Q21 (Time/reading)`
part2to3$`Q21 (Time/reading)`=quantify(response, option, num)

#questionaire Q22
option <- c("0", "1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
num <- c(0, .10, .30, .50, .70, .90)
part2to3$`Q22 (% Lecture)`=factor(part2to3$`Q22 (% Lecture)`,option,num)
part2to3$`Q22 (% Lecture)`=num[part2to3$`Q22 (% Lecture)`]

#questionaire Q23
option <- c("0", "1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
num <- c(0, .10, .30, .50, .70, .90)
part2to3$`Q23 (% tutorial participation)`=factor(part2to3$`Q23 (% tutorial participation)`,option,num)
part2to3$`Q23 (% tutorial participation)`=num[part2to3$`Q23 (% tutorial participation)`]

#Grade
option <- c("A", "A-","B+","B","B-","C+","C","C-","D+","D","F")
num <- c(4, 3.7, 3.3, 3, 2.7, 2.3, 2, 1.7, 1.3, 1, 0)
grade$Grade=factor(grade$Grade,option,num)
grade$Grade=num[grade$Grade]

#cGPA Before
grade$`cGPA (Before)`[which(grade$`cGPA (Before)`==0)]=NA

mean_part1=aggregate(part1,list(demographic[,1]),function(x) mean(as.numeric(x),na.rm=T))
names(mean_part1)[1]="Mean"

mean_part2to3=aggregate(part2to3,list(demographic[,1]),function(x) mean(as.numeric(x),na.rm=T))
names(mean_part2to3)[1]="Mean"

mean_part4=aggregate(part4[,-1],list(part4[,1]),function(x) mean(as.numeric(x),na.rm=T))
names(mean_part4)[1]="Mean"

mean_grade=aggregate(grade,list(demographic[,1]),function(x) mean(as.numeric(x),na.rm=T))
names(mean_grade)[1]="Mean"

#ttest
testResult=t(apply(cbind(part1,part2to3,grade),2, function(x) unlist(t.test(x~demographic$`Outside Class Activity`))))
write.csv(testResult,file="ttest.csv")

testResult_4=t(apply(part4[,-1],2, function(x) unlist(t.test(x~part4$`Outside Class Activity`))))
write.csv(testResult_4,file="ttest part 4.csv")
