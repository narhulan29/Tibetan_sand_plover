 ####This code is mapping and analysis of incubation part in paper: ¡°Breeding ecology of high-altitude nesting shorebird in the Qinghai-Tibetan Plateau¡±
### Xi Lin 2022.9


rm(list = ls())
setwd("C:/Users/LX/Desktop/Data and R code")

### 1 Figure 5a----

#### Import Data----
nest<-c("lsp_d003","lsp_a003","lsp_a001","lsp3","2017LSP3","lsp6","lsp7")
nest2<-c("d","c","b","e","a","f","g")


for(k in 1:7){
data.all.dir<-paste0("nest/",nest[k],"-2.Rdata")
load(data.all.dir)

#### Organize data----
##Sort the time for drawing
library(plotly)
nestdata$FTime<-NA
for(i in 0:23){
  nestdata$FTime[which(nestdata$TFhour==1&nestdata$hour==i)]<-0:(length(nestdata$FTime[which(nestdata$TFhour==1&nestdata$hour==i)])-1)
  nestdata$FTime[which(nestdata$TFhour==2&nestdata$hour==i)]<-0:(length(nestdata$FTime[which(nestdata$TFhour==2&nestdata$hour==i)])-1)
  nestdata$FTime[which(nestdata$TFhour==3&nestdata$hour==i)]<-0:(length(nestdata$FTime[which(nestdata$TFhour==3&nestdata$hour==i)])-1)
}

library(tidyr)
for(i in 0:9){
  nestdata$FTime[which(nestdata$FTime==i)]<-paste0("0",i)
  nestdata$hour[which(nestdata$hour==i)]<-paste0("0",i)
}
nestdat<-tidyr::unite(nestdata, "FFtime", hour,FTime ,sep=".", remove = FALSE)

##Check time sequence
library(gtools)
orderFFtime<-mixedsort(nestdat$FFtime[which(nestdat$TFhour==1)]);orderFFtime


##Change the Nest_Attendance representative number to match the corresponding color

nestdat$Nest_Attendance[which(nestdat$Nest_Attendance==2)]<-0.5
nestdat$Nest_Attendance[which(nestdat$Nest_Attendance==1)]<-1.5
nestdat$Nest_Attendance[which(nestdat$Nest_Attendance==0)]<-1


col<-c("#fb9a99","#f7f7f7","#67a9cf")

##Change to new nest number
nestdat$Nest2<-nest2[k]



####PLOT----
nestdat$Nest_Attendance<-as.character(nestdat$Nest_Attendance)
nestdat$TFhour<-as.character(nestdat$TFhour)

sunr<-which(!is.na(nestdat$sunrise))
suns<-which(!is.na(nestdat$sunset))

p <- ggplot(nestdat, aes(x=FFtime, y=TFhour)) +
  geom_tile(aes(fill = Nest_Attendance))+
  scale_fill_manual(values = col,name="sex:",labels=c("Female","No bird","Male")) +
  labs(title = paste0("Incubation rhythm:",nestdat$Nest2[1]),
       y = "Day",x="Time(h)")+
  coord_fixed(ratio = 60)+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",text = element_text(size = 20),axis.text.y = element_text(colour = "black"))+
  geom_vline(aes(xintercept="00.01"),linetype="dashed",colour="#525252",size=0.7)+
  geom_vline(aes(xintercept="06.00"),linetype="dashed",colour="#525252",size=0.7)+
  geom_vline(aes(xintercept="12.00"),linetype="dashed",colour="#525252",size=0.7)+ 
  geom_vline(aes(xintercept="18.00"),linetype="dashed",colour="#525252",size=0.7)+
  geom_vline(aes(xintercept="23.58"),linetype="dashed",colour="#525252",size=0.7)+
  geom_vline(aes(xintercept=FFtime[sunr[1]]),colour="#fed976",size=0.7)+
  geom_vline(aes(xintercept=FFtime[suns[1]]),colour="#8c6bb1",size=0.7)


p

ggsave(paste0("result_plot/",nestdata$Nest[k],".png"),p , width = 6, height = 3, dpi = 500)

}



###2 Figure 4a----
rm(list = ls())

library(ggplot2)
####Import Data----
load("analyse/all_o_TFnest.Rdata")

##Statistical median
summary(all_o_TFnest$F_incubationmin/1440)
summary(all_o_TFnest$M_incubationmin/1440)
summary((all_o_TFnest$M_incubationmin+all_o_TFnest$F_incubationmin)/1440)
##Change to new nest number
all_o_TFnest$Nest2<-NA
all_o_TFnest$Nest2[which(all_o_TFnest$Nest=="lsp_d003")]<-"d"
all_o_TFnest$Nest2[which(all_o_TFnest$Nest=="lsp_a003")]<-"c"
all_o_TFnest$Nest2[which(all_o_TFnest$Nest=="lsp_a001")]<-"b"
all_o_TFnest$Nest2[which(all_o_TFnest$Nest=="lsp3")]<-"e"
all_o_TFnest$Nest2[which(all_o_TFnest$Nest=="2017LSP3")]<-"a"
all_o_TFnest$Nest2[which(all_o_TFnest$Nest=="lsp6")]<-"f"
all_o_TFnest$Nest2[which(all_o_TFnest$Nest=="lsp7")]<-"g"

####Organize data----
numrow<-(length(all_o_TFnest$Year))*6
all_o_TFnest_trueplot<-as.data.frame(matrix(nrow=numrow,ncol = 8))
colnames(all_o_TFnest_trueplot)<-c("Year","Nest","TFhour","Name1","Value1","timelength","Name2","Value2")
all_o_TFnest_trueplot$Year[1:96]<-rep(all_o_TFnest$Year, times=6)
all_o_TFnest_trueplot$Nest[1:96]<-rep(all_o_TFnest$Nest, times=6)
all_o_TFnest_trueplot$Nest2[1:96]<-rep(all_o_TFnest$Nest2, times=6)
all_o_TFnest_trueplot$TFhour[1:96]<-rep(all_o_TFnest$TFhour, times=6)
Name1<-c("F_exchange_gapmin","M_exchange_gapmin","F_absencemin","M_absencemin","F_incubationmin","M_incubationmin")
Name2<-c("F_aexchange_gap%","M_aexchange_gap%","F_eabsence%","M_eabsence%","F_incubation%","M_incubation%")
a<-rep("F_exchange_gapmin", times=16)
for (i in 2:6) {
  a<-c(a,rep(Name1[i],times=16))
}
b<-rep("F_aexchange_gap%", times=16)
for (j in 2:6) {
  b<-c(b,rep(Name2[j],times=16))
}
all_o_TFnest_trueplot$Name1[1:96]<-a
all_o_TFnest_trueplot$Name2[1:96]<-b
all_o_TFnest_trueplot$timelength[1:96]<-rep(all_o_TFnest$timelength, times=6)
c<-c(all_o_TFnest$F_exchange_gapmin,all_o_TFnest$M_exchange_gapmin,all_o_TFnest$F_absencemin,all_o_TFnest$M_absencemin,all_o_TFnest$F_incubationmin,all_o_TFnest$M_incubationmin)
all_o_TFnest_trueplot$Value1[1:96]<-c
all_o_TFnest_trueplot$Value2<-all_o_TFnest_trueplot$Value1*100/all_o_TFnest_trueplot$timelength

all_o_TFnest_trueplot<-tidyr::unite(all_o_TFnest_trueplot, "Nest_Day", Nest,TFhour ,sep="-", remove = FALSE)
all_o_TFnest_trueplot<-tidyr::unite(all_o_TFnest_trueplot, "Nest_Day2", Nest2,TFhour ,sep="-", remove = FALSE)




####PLOT----

col2<-c("#f7f7f7","#fa9fb5","#f768a1","#f7f7f7","#74a9cf","#3690c0")

q<-ggplot(all_o_TFnest_trueplot, aes(x =Nest_Day2,y=Value2,fill = Name2))+
  geom_col(position = 'stack', width = 0.6,color = "#737373")+
  theme_bw()+
  scale_fill_manual(values=col2,name="",labels=c("Exchange gap","Female abscence within incubation bout","Female incubation","Exchange gap","Male abscence within incubation bout","Male incubation"))+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "24 Hour[%]",x="Nest_Day")+
  theme(text=element_text(size=20),legend.position = "bottom",axis.text.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"))

q

ggsave(paste0("result_plot/","Figure 4a.png"),q , width = 10, height = 8, dpi = 500)




###3 Figure 7abcd----
rm(list = ls())
####3.1 7ab-incubation bout----
library(ggplot2)
#####Import Data----
load("analyse/all_o_eachbout.Rdata")

##change to sex full name
all_o_eachbout$Sex[which(all_o_eachbout$Sex=="F")]<-"Female"
all_o_eachbout$Sex[which(all_o_eachbout$Sex=="M")]<-"Male"

##Change to new nest number
all_o_eachbout$Nest2<-NA
all_o_eachbout$Nest2[which(all_o_eachbout$Nest=="lsp_d003")]<-"d"
all_o_eachbout$Nest2[which(all_o_eachbout$Nest=="lsp_a003")]<-"c"
all_o_eachbout$Nest2[which(all_o_eachbout$Nest=="lsp_a001")]<-"b"
all_o_eachbout$Nest2[which(all_o_eachbout$Nest=="lsp3")]<-"e"
all_o_eachbout$Nest2[which(all_o_eachbout$Nest=="2017LSP3")]<-"a"
all_o_eachbout$Nest2[which(all_o_eachbout$Nest=="lsp6")]<-"f"
all_o_eachbout$Nest2[which(all_o_eachbout$Nest=="lsp7")]<-"g"

library(simplevis)
all_o_eachbout$Nest <- as.factor(all_o_eachbout$Nest)
all_o_eachbout$Nest2 <- as.factor(all_o_eachbout$Nest2)
all_o_eachbout$Sex <- as.factor(all_o_eachbout$Sex)

#####PLOT----
#between and within nests in incubation bout
p=gg_boxplot_col(data=all_o_eachbout,x_var = Nest2,y_var =boutlengthh,col_var =Sex,pal=c('#b2182b','#2166ac'))+ ylim(0, 16)+labs(title = "between and within nests in incubation bout",y="Incubation bout [h]",x="Nest")+theme_bw()+theme(text=element_text(size=20),axis.text.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"))

#between sexes in incubation bout
col3<-c('#b2182b','#2166ac')
q<-ggplot(all_o_eachbout, aes(x=Sex, y=boutlengthh,fill=Sex)) + 
  geom_boxplot(alpha=0.5)+
  theme_classic()+
  scale_fill_manual(values=col3)+
  labs(y = "Incubation bout [h]")+
  theme(text=element_text(size=20),axis.text.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"))


ggsave(paste0("result_plot/","Figure 7a.png"), p , width = 10, height = 6, dpi = 500)
ggsave(paste0("result_plot/","Figure 7b.png"), q , width = 6, height = 5, dpi = 500)

#####ANOVA----
aov.all <- aov(boutlengthh~Sex,data=all_o_eachbout)
summary(aov.all)



####3.2 7cd-absence within an incubation bout----
#####PLOT----
#between and within nests in absence within an incubation bout
p=gg_boxplot_col(data=all_o_eachbout,x_var = Nest2,y_var =absenceallmin,col_var =Sex,pal=c('#b2182b','#2166ac'))+labs(title = " between and within nests in absence within an incubation bout",y="Abscence within incubation bout [min]",x="Nest")+theme_bw()+ scale_y_continuous(limits = c(-1, 70))+ theme(text=element_text(size=20),axis.text.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"))
##Limit y range and remove two outliers

#between sexes in absence within an incubation bout
col3<-c('#b2182b','#2166ac')
q<-ggplot(all_o_eachbout, aes(x=Sex, y=absenceallmin,fill=Sex)) + 
  geom_boxplot(alpha=0.5)+
  theme_classic()+
  scale_fill_manual(values=col3)+
  labs(y = "Abscence within incubation bout [min]")+
  theme(text=element_text(size=20),axis.text.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"))+   scale_y_continuous(limits = c(-1, 70))
##Limit y range and remove two outliers

ggsave(paste0("result_plot/","Figure 7c.png"), p , width = 10, height = 6, dpi = 500)
ggsave(paste0("result_plot/","Figure 7d.png"), q , width = 7, height = 6, dpi = 500)

#####ANOVA----
aov.all <- aov(average_absencemin~Sex,data=all_o_eachbout)
summary(aov.all)



###4 Figure 5bc----
####Figure 5b
rm(list = ls())
library(ggplot2)
library(Rmisc)
#####Import Data----
load("analyse/all_o_eachhour.Rdata")

#### Organize data----
aoe<-all_o_eachhour[,1:9]
aoe$Fnest_attendance_H<-aoe$Fnest_attendance_H*100
aoe$Mnest_attendance_H<-aoe$Mnest_attendance_H*100
aoe$allnest_attendance_H<-aoe$allnest_attendance_H*100
aoeF<-aoe[,1:7]
colnames(aoeF)[7] <- 'nest_attendance_H'
aoeM<-aoe[,-c(7,9)]
colnames(aoeM)[7] <- 'nest_attendance_H'
aoeall<-aoe[,-c(7,8)]
colnames(aoeall)[7] <- 'nest_attendance_H'

aoe_plot<-rbind(aoeF,aoeM,aoeall)
aoe_plot$Sex<-NA
aoe_plot$Sex[1:length(aoe$Year)]<-"Female"
aoe_plot$Sex[(length(aoe$Year)+1):(length(aoe$Year)+length(aoe$Year))]<-"Male"
aoe_plot$Sex[(length(aoe$Year)+length(aoe$Year)+1):(length(aoe$Year)+length(aoe$Year)+length(aoe$Year))]<-"Both"
aoe_plot$Nest<-as.factor(aoe_plot$Nest)
aoe_plot$hour<-as.factor(aoe_plot$hour)
##Calculate the mean standard deviation, standard error and 95% confidence interval
tgc <- summarySE(aoe_plot, measurevar="nest_attendance_H", groupvars=c("hour","Sex"))

####PLOT Figure 5b----

col3<-c('#999999','#b2182b','#2166ac')
pd <- position_dodge(0.4)
p<-ggplot(tgc, aes(x=hour, y=nest_attendance_H, group=Sex,colour=Sex)) + 
  geom_errorbar(aes(ymin=nest_attendance_H-se, ymax=nest_attendance_H+se), width=0, position=pd,linetype=5,alpha=0.4) +
  geom_point(position=pd, size=2)+
  scale_colour_manual(values=col3)+
  xlab("Time [h]") +
  ylab("Nest attendance [%]")+
  theme_classic()+
  theme(text=element_text(size=20),legend.position = "bottom",axis.text.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"))
  
ggsave(paste0("result_plot/","Figure 5b.png"), p , width = 9, height = 5, dpi = 500)


####PLOT Figure 5c----
##Calculate the maximum and minimum values of sunrise and sunset
load("analyse/all_nestdata.Rdata")
library(lubridate)
sunr<-which(!is.na(all_nestdata$sunrise))
suns<-which(!is.na(all_nestdata$sunset))

a<-paste0("2022-5-1-",all_nestdata$Time[sunr])
date_sr<-paste0("2022-5-1-","0:00:00")
date_sr = ymd_hms(date_sr)
date_ed = ymd_hms(a)
date_all = interval(date_sr,date_ed)
time_length(date_all,'hour')
min(time_length(date_all,'hour'))#5.45
max(time_length(date_all,'hour'))#5.55

b<-paste0("2022-5-1-",all_nestdata$Time[suns])
date_sr<-paste0("2022-5-1-","0:00:00")
date_sr = ymd_hms(date_sr)
date_ed = ymd_hms(b)
date_all = interval(date_sr,date_ed)
time_length(date_all,'hour')
min(time_length(date_all,'hour'))#20.91667
max(time_length(date_all,'hour'))#21.15


####PLOT
col4<-c('#b2182b','#2166ac')
aoe_plot2<-aoe_plot[-which(aoe_plot$Sex=="Both"),]
aoe_plot2$hour<-as.numeric(aoe_plot2$hour)
p2<-ggplot(aoe_plot2, aes(x=hour, y=nest_attendance_H, group=Sex,colour=Sex)) + 
  geom_vline(aes(xintercept=5.45),linetype="dashed",colour="#525252",size=0.7,alpha=0.3)+
  geom_vline(aes(xintercept=5.55),linetype="dashed",colour="#525252",size=0.7,alpha=0.3)+
  geom_vline(aes(xintercept=20.91667),linetype="dashed",colour="#525252",size=0.7,alpha=0.3)+
  geom_vline(aes(xintercept=21.15),linetype="dashed",colour="#525252",size=0.7,alpha=0.3)+
  geom_smooth(aes(fill = Sex),alpha=0.3)+
  scale_colour_manual(values=col4)+
  scale_fill_manual(values=col4)+
  xlab("Time [h]") +
  ylab("Nest attendance [%]")+
  theme_classic()+
  scale_x_continuous(breaks=seq(from=0, to=24, by=6))+
  theme(text=element_text(size=20),legend.position = "bottom",axis.text.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"))
  
  
ggsave(paste0("result_plot/","Figure 5c.png"), p2 , width = 9, height = 5, dpi = 500)


###5 Figure 4b----

#### Organize data----
aoe<-all_o_eachhour[,-c(7,8,9)]
aoe<-aoe[,1:9]
aoe$F_nest_attendance_day<-aoe$F_nest_attendance_day*100
aoe$M_nest_attendance_day<-aoe$M_nest_attendance_day*100
aoe$all_nest_attendance_day<-aoe$all_nest_attendance_day*100
aoeF<-aoe[,1:7]
colnames(aoeF)[7] <- 'nest_attendance_day'
aoeM<-aoe[,-c(7,9)]
colnames(aoeM)[7] <- 'nest_attendance_day'
aoeall<-aoe[,-c(7,8)]
colnames(aoeall)[7] <- 'nest_attendance_day'
aoe_plot<-rbind(aoeF,aoeM,aoeall)
aoe_plot$Sex<-NA
aoe_plot$Sex[1:length(aoe$Year)]<-"Female"
aoe_plot$Sex[(length(aoe$Year)+1):(length(aoe$Year)+length(aoe$Year))]<-"Male"
aoe_plot$Sex[(length(aoe$Year)+length(aoe$Year)+1):(length(aoe$Year)+length(aoe$Year)+length(aoe$Year))]<-"Both"


col3<-c('#999999','#b2182b','#2166ac')
aoe_plot$Sex <- as.factor(aoe_plot$Sex)
p<-ggplot(aoe_plot, aes(x=Sex, y=nest_attendance_day,fill=Sex)) + 
    geom_boxplot(alpha=0.5)+
  theme_classic()+
  scale_fill_manual(values=col3)+
  labs(y = "Daily Nest attendance [%]")+
  theme(text=element_text(size=20),legend.position = "bottom",axis.text.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"))


ggsave(paste0("result_plot/","Figure 4b.png"), p , width = 10, height = 9, dpi = 500)

####ANOVA----
aov.all <- aov(nest_attendance_day~Sex,data=aoe_plot)
summary(aov.all)
## Least Significant Difference (LSD) test for significance test
library(DescTools)
PostHocTest(aov.all,method = "lsd")


###6 Figure 6----
rm(list = ls())
load("analyse/all_o_eachhour.Rdata")
#### Organize data----
aoe<-all_o_eachhour[,-c(7:12)]
aoe$F_daytime_nest_attendance<-aoe$F_daytime_nest_attendance*100
aoe$M_daytime_nest_attendance<-aoe$M_daytime_nest_attendance*100
aoe$F_night_nest_attendance<-aoe$F_night_nest_attendance*100
aoe$M_night_nest_attendance<-aoe$M_night_nest_attendance*100

aoeFdaytime<-aoe[,1:7]
colnames(aoeFdaytime)[7] <- 'nest_attendance_day'
aoeFdaytime$Daynight<-"Day"
aoeFdaytime$Sex<-'Female'
aoeMdaytime<-aoe[,c(1:6,8)]
colnames(aoeMdaytime)[7] <- 'nest_attendance_day'
aoeMdaytime$Daynight<-"Day"
aoeMdaytime$Sex<-"Male"
aoeFnight<-aoe[,c(1:6,10)]
colnames(aoeFnight)[7] <- 'nest_attendance_day'
aoeFnight$Daynight<-"Night"
aoeFnight$Sex<-'Female'
aoeMnight<-aoe[,c(1:6,11)]
colnames(aoeMnight)[7] <- 'nest_attendance_day'
aoeMnight$Daynight<-"Night"
aoeMnight$Sex<-"Male"

aoe_plot<-rbind(aoeFdaytime,aoeMdaytime,aoeFnight,aoeMnight)

#### PLOT----
p<-ggplot(aoe_plot, aes(x=Daynight, y=nest_attendance_day, fill=Sex)) +
  geom_boxplot(alpha=0.5)+
  scale_fill_manual(values=c('#b2182b','#2166ac'))+
  theme_classic()+
  theme(text=element_text(size=20),legend.position = "bottom",axis.text.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"))+
  labs(y = "Nest attendance [%]",x="")
  
ggsave(paste0("result_plot/","Figure 6.png"), p , width = 10, height = 6, dpi = 500)

####ANOVA----
##Day
aov.all <- aov(nest_attendance_day~Sex,data=aoe_plot[which(aoe_plot$Daynight=="Day"),])
summary(aov.all)

##Night
aov.all <- aov(nest_attendance_day~Sex,data=aoe_plot[which(aoe_plot$Daynight=="Night"),])
summary(aov.all)











