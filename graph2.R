library(ggplot2)

#mirror hist
varname=names(demographic)
for(i in 2:ncol(demographic))
{
  data=as.data.frame(table(demographic[,c(1,i),drop=F]))
  data$Freq[which(data$Outside.Class.Activity=="OD")]=-1*data$Freq[which(data$Outside.Class.Activity=="OD")]
  png(paste(varname[i],"mirrored hist",".png"))
  print(
    ggplot(data,aes(x=data[,2],y=Freq,fill=Outside.Class.Activity))+
      geom_bar(stat="identity",position = "identity",alpha=.6)+
      labs(x=varname[i],y="Frequency",fill=varname[1])+
      scale_y_continuous(labels=abs)+
      coord_flip()+
      ggtitle(varname[i])+
      theme(plot.title = element_text(face="bold",size=24),panel.grid.major.x=element_line(colour = "grey"),panel.grid.minor.x=element_line(colour = "grey"),panel.background = element_rect(fill = "white")))
  dev.off()
}

elec_count=c(-apply(subset(elec,game=="OD"),2,function(x){length(which(!is.na(x)))}),apply(subset(elec,game=="Civ4"),2,function(x){length(which(!is.na(x)))}))
data=data.frame("Outside Class Activity"=factor(rep(c("OD","Civ4"),each=14),levels = c("OD","Civ4"),labels = c("OD","Civ4")),
                "Elective"=rep(c("Phy", "Chem", "Bio", "Com Sci", "Inter Sci", "Eng Lit", "Chin Lit", "Hist", "Chin Hist", "Ethics & RS", "Music", "Visual Art", "Econ", "Geog"),times=2),
                "Freq"=elec_count)
png("High School Electives mirrored hist.png")
ggplot(data,aes(x=data[,2],y=Freq,fill=Outside.Class.Activity))+
  geom_bar(stat="identity",position = "identity",alpha=.6)+
  scale_y_continuous(labels=abs)+
  coord_flip()+
  labs(x="Elective",y="Frequency",fill="Outside Class Activity")+
  ggtitle("High School Electives")+
  theme(plot.title = element_text(face="bold",size=24),panel.grid.major.x=element_line(colour = "grey"),panel.grid.minor.x=element_line(colour = "grey"),panel.background = element_rect(fill = "white"))
dev.off()


varname=paste("Q",rep(1:17,each=3),rep(c(" (Before)"," (After)"," (Change)")),sep = "")
for(i in 1:ncol(part1))
{
  data=as.data.frame(table(cbind(demographic[,1,drop=F],part1[,i,drop=F])))
  data$Freq[which(data$Outside.Class.Activity=="OD")]=-1*data$Freq[which(data$Outside.Class.Activity=="OD")]
  rng=range(part1[,i],na.rm = T)
  n=nrow(data)/2
  png(paste(varname[i],"mirrored hist",".png"))
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
  png(paste(varname[i],"mirrored hist",".png"))
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
  png(paste(varname[i],"mirrored hist",".png"))
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
  png(paste(varname[i],"mirrored hist",".png"))
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