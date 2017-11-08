library(ggplot2)

rawdata_e_e=subset(rawdata_e,rawdata_e$Faculty=="ENF")

#Quantifying Q18-Q23, borrowed from Danny
quantify <- function(response, choice, converted_num) {
  sapply(as.factor(response), function(x) converted_num[which(x == choice)])
}


part1=rawdata_e[,5:55]
part2to3=rawdata_e[,56:73]

OD=rawdata_e[which(is.na(rawdata_e$Q41)),111:115]
Civ4=rawdata_e[which(!is.na(rawdata_e$Q41)),116:120]
names(OD)=c("LA1","LA2","LA3","LA4","LA5")
names(Civ4)=c("LA1","LA2","LA3","LA4","LA5")
part4=rbind(Civ4,OD)

grade=rawdata_e[,94:97]


#demographic
game=factor(is.na(rawdata_e$Q41),levels=c(T,F),labels = c("OD","Civ4"))
sex=factor(rawdata_e$Sex)
eng=factor(rawdata_e$`English Proficiency`,levels = c("Others", "DSE 4 or below", "DSE 5 or above" ))
fac=factor(rawdata_e$Faculty)
coll=factor(rawdata_e$College)
year=factor(rawdata_e$`Year of Study`)
first=factor(rawdata_e$`First GEF?`)

demographic=data.frame("Outside Class Activity"=game,"Sex"=sex,"DSE English Grade"=eng, "Faculty"=fac, "College"=coll, "Year of Study"=year, "First GEF"=first, check.names = F)
elec=rawdata_e[,75:88]


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

mean_part4=aggregate(part4,list(demographic[,1]),function(x) mean(as.numeric(x),na.rm=T))
names(mean_part4)[1]="Mean"

mean_grade=aggregate(grade,list(demographic[,1]),function(x) mean(as.numeric(x),na.rm=T))
names(mean_grade)[1]="Mean"

#ttest
testResult=t(apply(cbind(part1,part2to3,part4,grade),2, function(x) unlist(t.test(x~demographic$`Outside Class Activity`))))
write.csv(testResult,file="ttest_e.csv")

#graph

varname=paste("Q",rep(1:17,each=3),rep(c(" (Before)"," (After)"," (Change)")),sep = "")
for(i in 1:ncol(part1))
{
  data=as.data.frame(table(cbind(demographic[,1,drop=F],part1[,i,drop=F])))
  data$Freq[which(data$Outside.Class.Activity=="OD")]=-1*data$Freq[which(data$Outside.Class.Activity=="OD")]
  rng=range(part1[,i],na.rm = T)
  n=nrow(data)/2
  png(paste("ENF ", varname[i],".png"))
  print(
    ggplot(data,aes(x=data[,2],y=Freq,fill=Outside.Class.Activity))+
      geom_bar(stat="identity",position = "identity",alpha=.6)+
      labs(x=varname[i],y="Frequency",fill="Outside Class Activity")+
      scale_y_continuous(labels=abs)+
      ggtitle(varname[i])+
      theme(plot.title = element_text(face="bold",size=24),panel.grid.major.x=element_line(colour = "grey"),panel.grid.minor.x=element_line(colour = "grey"),panel.background = element_rect(fil = "white"))+
      coord_flip()+
      geom_vline(data=mean_part1, aes(xintercept=(mean_part1[,i+1]-rng[1])/(rng[2]-rng[1])*(n-1)+1,colour=Mean),size=1,linetype="dashed"))
  dev.off()
}



varname=paste("Q",18:35,sep = "")
for(i in 1:ncol(part2to3))
{
  data=as.data.frame(table(cbind(demographic[,1,drop=F],part2to3[,i,drop=F])))
  data$Freq[which(data$Outside.Class.Activity=="OD")]=-1*data$Freq[which(data$Outside.Class.Activity=="OD")]
  rng=range(part2to3[,i],na.rm = T)
  n=nrow(data)/2
  png(paste("ENF ", varname[i], ".png"))
  print(
    ggplot(data,aes(x=data[,2],y=Freq,fill=Outside.Class.Activity))+geom_bar(stat="identity",position = "identity",alpha=.6)+
      labs(x=varname[i],y="Frequency",fill="Outside Class Activity")+
      scale_y_continuous(labels=abs)+
      ggtitle(varname[i])+
      theme(plot.title = element_text(face="bold",size=24),panel.grid.major.x=element_line(colour = "grey"),panel.grid.minor.x=element_line(colour = "grey"),panel.background = element_rect(fil = "white"))+
      coord_flip()+
      geom_vline(data=mean_part2to3, aes(xintercept=(mean_part2to3[,i+1]-rng[1])/(rng[2]-rng[1])*(n-1)+1,colour=Mean),size=1,linetype="dashed"))
  dev.off()
}

varname=paste("LA",1:5,sep = "")
for(i in 1:ncol(part4))
{
  data=as.data.frame(table(cbind(demographic[,1,drop=F],part4[,i,drop=F])))
  data$Freq[which(data$Outside.Class.Activity=="OD")]=-1*data$Freq[which(data$Outside.Class.Activity=="OD")]
  png(paste("ENF ", varname[i], ".png"))
  print(
    ggplot(data,aes(x=data[,2],y=Freq,fill=Outside.Class.Activity))+
      geom_bar(stat="identity",position = "identity",alpha=.6)+
      labs(x=varname[i],y="Frequency",fill="Outside Class Activity")+
      scale_y_continuous(labels=abs)+
      ggtitle(varname[i])+
      geom_vline(data=mean_part4, aes(xintercept=mean_part4[,i+1],colour=Mean),size=1,linetype="dashed")+
      theme(plot.title = element_text(face="bold",size=24),panel.grid.major.x=element_line(colour = "grey"),panel.grid.minor.x=element_line(colour = "grey"),panel.background = element_rect(fil = "white"))+
      coord_flip())
  dev.off()
}

varname=names(grade)
label=c("F", "D","D+","C-","C","C+","B-","B","B+","A-","A")
brk=c(4,3.85,3.5,3.15,2.85,2.5,2.15,1.85,1.5,1.15,.5,0)
for(i in 1:ncol(grade))
{
  if(i==2)
  {
    data=as.data.frame(table(cbind(demographic[,1,drop=F],factor(grade[,i],c(0,1,1.3,1.7,2,2.3,2.7,3,3.3,3.7,4),label))))
    data$Freq[which(data$Outside.Class.Activity=="OD")]=-1*data$Freq[which(data$Outside.Class.Activity=="OD")]
    n=nrow(data)/2
  }
  else
  {
    data=as.data.frame(table(cbind(demographic[,1,drop=F],cut(grade[,i],brk,label))))
    data$Freq[which(data$Outside.Class.Activity=="OD")]=-1*data$Freq[which(data$Outside.Class.Activity=="OD")]
    n=nrow(data)/2
  }
  png(paste("ENF", varname[i], ".png"))
  print(
    ggplot(data,aes(x=data[,2],y=Freq,fill=Outside.Class.Activity))+
      geom_bar(stat="identity",position = "identity",alpha=.6)+
      labs(x=varname[i],y="Frequency",fill="Outside Class Activity")+
      scale_y_continuous(labels=abs)+
      ggtitle(varname[i])+
      theme(plot.title = element_text(face="bold",size=24),panel.grid.major.x=element_line(colour = "grey"),panel.grid.minor.x=element_line(colour = "grey"),panel.background = element_rect(fil = "white"))+
      geom_vline(data = mean_grade, mapping = aes(xintercept=mean_grade[,i+1]/4*(n-1)+1,colour=Mean),size=1,linetype="dashed")+
      coord_flip())
  dev.off()
}