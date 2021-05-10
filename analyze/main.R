working_dir='/home/saeedeh/Desktop/uni Archive/Ricardo/VR-Crowding project/VR-Crowding-code+data/analyze'
source(paste0(working_dir,'/Setup.R'))

#variables for subjects and task1 are in df_subj
#variables related to the trials of task2 are in df_all (N*5 rows)
#useful function: rep_subj2all
#############################################################################

library(lme4)
library(lmerTest)
library(sjPlot)
####TASK1: Crowding and TP 
#nothing sig
sum(df_subj$first_more_crowded)

mean(df_subj$task1_chose_more_crowded)

mean(df_subj$task1_est_more_crowded)
mean(df_subj$task1_est_less_crowded)
t.test(df_subj$task1_est_less_crowded,df_subj$task1_est_more_crowded,paired = TRUE)
t.test(df_subj$task1_est1, df_subj$task1_est2, paired = TRUE)
  boxplot(df_subj$task1_est_less_crowded, df_subj$task1_est_more_crowded, ylab="Estimated duration")
  boxplot(df_subj$task1_est1, df_subj$task1_est2, ylab="Estimated duration")
df<-data.frame(names=c('1st trip', '2nd trip'), 
               means=c(mean(df_subj$task1_est1),mean(df_subj$task1_est2)), 
               sem=c(sd(df_subj$task1_est1)/sqrt(length(df_subj$task1_est1)), 
                     sd(df_subj$task1_est2)/sqrt(length(df_subj$task1_est2))) )
  ggplot(df) +
    geom_bar( aes(x=names, y=means), stat="identity", fill="skyblue", alpha=0.7) +
    geom_errorbar( aes(x=names, ymin=means-sem, ymax=means+sem), width=0.4, colour="orange", alpha=0.9, size=1.3)

df<-data.frame(names=c('Less crowded trip', 'More crowded trip'), 
                 means=c(mean(df_subj$task1_est_less_crowded),mean(df_subj$task1_est_more_crowded)), 
                 sem=c(sd(df_subj$task1_est_less_crowded)/sqrt(length(df_subj$task1_est_less_crowded)), 
                       sd(df_subj$task1_est_more_crowded)/sqrt(length(df_subj$task1_est_more_crowded))) )
  ggplot(df) +
    geom_bar( aes(x=names, y=means), stat="identity", fill="skyblue", alpha=0.7) +
    geom_errorbar( aes(x=names, ymin=means-sem, ymax=means+sem), width=0.4, colour="orange", alpha=0.9, size=1.3)
  
#######  HRV correlates
###
#comparing conditions task 1
boxplot(mean_RR~crowding, data=df_all, xlab="Crowding", ylab="sd RR")
boxplot(df_subj$tas1_RR_less_crowded, df_subj$tas1_RR_more_crowded)
t.test(df_subj$tas1_RR_less_crowded, df_subj$tas1_RR_more_crowded, paired = TRUE)

###
# task2: Is HRV related to crowding level?
#mean_RR
summary(lmer(mean_RR ~ crowding + (1|subj_id) , data<-df_all))
summary(lmer(mean_RR ~ density_est + (1|subj_id) , data=df_all))
summary(lmer(mean_RR ~ actual_times + (1|subj_id) , data=df_all))
m<-lmer( mean_RR~ est_times + (1|subj_id) , data=df_all)
t<-summary(m)
t$logLik
t<-summary(lmer(  TPB~mean_RR + (1|subj_id) , data<-df_all))
t
t$logLik

y<- df_all$mean_RR - rep_subj2all(df_subj$baseline_RR) 
m<-lmer(y ~ df_all$crowding + (1|subj_id) , data<-df_all)
summary(m)

m<-lm(y ~ df_all$crowding + 0 , data<-df_all)
summary(m)

summary(lmer(TPB ~ mixed_feelings + (1|subj_id) , data=df_all))

#rMSSD
summary(lmer(rMSSD~ crowding+(1|subj_id), data = df_all))
summary(lmer(rMSSD~ valence+(1|subj_id), data = df_all))
summary(lmer(rMSSD~ arousal+(1|subj_id), data = df_all))
summary(lm(baseline_rMSSD~ valence_crowdBias_index, data = df_subj))
summary(lmer(est_times~ rMSSD+(1|subj_id), data = df_all))
rel_hrv<-  df_all$rMSSD - rep_subj2all(df_subj$baseline_rMSSD) 
summary(lmer(mean_RR ~  sign(valence)+ (1|subj_id), data = df_all))

# slope
summary(lmer(slope_RR~ arousal2 + (1|subj_id), data = df_all))
summary(lmer(actual_times~slope_RR+(1|subj_id), data = df_all))
summary(lmer(est_times~slope_RR+(1|subj_id), data = df_all))
summary(lmer(TPB~slope_RR2+(1|subj_id), data = df_all))
summary(lmer(slope_RR~valence+(1|subj_id), data = df_all))
summary(lmer(est_times~ actual_times+ slope_RR+(1|subj_id), data = df_all))
#slope is signigicantly negative indicating that heart rate increases over time
summary(lmer(slope_RR~actual_times+est_times+(1|subj_id), data = df_all))
summary(lmer(crowding~abs(slope_RR)+(1|subj_id), data = df_all))

#delta RR
summary(lmer(actual_times~delta_RR+(1|subj_id), data = df_all))
summary(lmer(est_times~delta_RR+(1|subj_id), data = df_all))
summary(lmer(TPB~delta_RR+(1|subj_id), data = df_all))
summary(lmer(delta_RR~(1|subj_id), data = df_all))
summary(lmer(est_times ~ actual_times+ delta_RR+(1|subj_id), data = df_all))

#LF
summary(lmer(LF~ arousal+(1|subj_id), data = df_all))
summary(lmer(T2_FrenchQ_1~ LF+(1|subj_id), data = df_all))

summary(lmer(mean_RR~valence+(1|subj_id), data=df_all))
summary(lmer(valence~rel_RR+(1|subj_id), data=df_all))

cor.test(df_subj$baseline_RR, df_subj$tp_crowdBias_index)
cor.test(df_subj$baseline_RR, df_subj$valence_crowdBias_index)
cor.test(df_subj$baseline_RR, df_subj$tp_avgEstBias)
cor.test(df_subj$baseline_rMSSD, df_subj$baseline_LF)


###
#Task2: Is HRV related to TP?
library(lme4)
m<-lmer(est_times ~ mean_RR+(1|subj_id), data=df_all)
summary(m)

###
#Task2: Is HRV related to valence?
#nothing important
m<-lmer(arousal~mean_RR +(1|subj_id), data=df_all)
summary(m) #

m<-lmer(valence~mean_RR +(1|subj_id), data=df_all)
summary(m) #**** positive valence is related to lower RR that is higher heart rate! 

m<-lmer(mean_RR~ valence_abs +(1|subj_id), data=df_all)
summary(m) #

rel_RR<-  df_all$mean_RR - rep_subj2all(df_subj$baseline_RR) 

m<-lmer(rel_RR~ valence +(1|subj_id), data=df_all)
summary(m) #again more positive valence decreases RR i.e. increases HR!
#it's aligned with the literature that higher HR decceleration happens following negative stimuli 
#than positive stimuli. It's a sign of inhibition 
ggplot(df_all, aes(df_all$valence, rel_RR))+
  geom_point()+geom_smooth()

m<-lmer(valence~ rel_RR +(1|subj_id), data=df_all)
summary(m) #* This oposite direction! doesn't make sense

###########################################
###Crowding and TP Task 2

boxplot(est_times - actual_times ~ crowding,data=df_all, xlab='Crowding Level', ylab='estimated - actual time')
library(lme4)
library(lmerTest)
m1<-lmer(TPB ~ crowding+ (1|subj_id), data= df_all)
r.squaredGLMM()
summary(m1)#########COOOOL!!!!!!!!!!!!
tab_model(m1, pred.labels = c('(Intercept)','Density level'), dv.labels = 'TPB')
tab_model(m1,
          pred.labels = c('Density level'), dv.labels = 'TPB',
          show.stat = TRUE, string.stat = 't', show.re.var = FALSE, show.intercept = FALSE, show.icc = FALSE)

df_all$l1_dummy<-df_all$crowding==1
df_all$l2_dummy<-df_all$crowding==2
df_all$l3_dummy<-df_all$crowding==3
df_all$l4_dummy<-df_all$crowding==4
df_all$l5_dummy<-df_all$crowding==5
df_all$l1_or_l2_dummy<- (df_all$l1_dummy | df_all$l2_dummy | df_all$l3_dummy) 
m<-lmer(est_times - actual_times ~ l5_dummy + l4_dummy+ (1|subj_id), data= df_all)
summary(m)
####Feeling and crowding 

m<-lm(valence~ crowding , data=df_all)
summary(m) #*****

m2<-lmer(valence~ crowding+ (1|subj_id) , data=df_all)
summary(m2) #****crowding decreases valence

m3<-lmer(arousal~ crowding+ (1|subj_id) , data=df_all)
summary(m3) #****crowding doesn't impact arousal
tab_model(m2,m3,
          pred.labels = c('Density level'), dv.labels = c('Valence', 'Arousal'),
          show.stat = TRUE, string.stat = 't', show.re.var = FALSE, show.intercept = FALSE, show.icc = FALSE, p.style = 'a')

####Feeling and TP

m4<-lmer((est_times-actual_times)~valence+(1|subj_id), data=df_all)
summary(m4) #********positive valence decreases perceived time

m5<-lmer((est_times-actual_times)~arousal+(1|subj_id), data=df_all)
summary(m5) #arousal doesn't impacts time perception

tab_model(m4,m5,
          pred.labels = c('Valence', 'Arousal'), dv.labels = c('TPB', 'TPB'),
          show.stat = TRUE, string.stat = 't', show.re.var = FALSE, show.intercept = FALSE, show.icc = FALSE, p.style = 'a')


m<-lmer((est_times-actual_times)~valence_abs+(1|subj_id), data=df_all)
summary(m) #absolute value of valence doesn't impact time perception

m<-lmer((est_times-actual_times)~valence+ crowding+(1|subj_id), data=df_all)
summary(m) #*******************impact of crowding on TP is only through valence

m<-lmer((est_times-actual_times)~(1|subj_id), data=df_all)
summary(m) #absolute value of valence doesn't impact time perception

###############################################################
#feeling and perception of density

#invalid<-which(is.na(dense_ordered))

m<-lmer((est_times-actual_times)~density_est+(1|subj_id), data=df_all)
summary(m) #

m<-lmer(valence~density_est+(1|subj_id), data=df_all)
summary(m) #****************

dens_bias <- (df_all$density_est-df_all$crowding)
m<-lmer(valence~ dens_bias + (1|subj_id), data=df_all)
summary(m) #*******

m<-lmer(valence~density_est+ crowding + (1|subj_id), data=df_all)
summary(m) #


m<-lmer(dens_bias~ valence  + (1|subj_id), data=df_all)
summary(m) #****** it doesn't show anyting! density estimates could be random and we 
           #still see this effect! since crowding and valence are related themselves!

boxplot(dens_bias~df_all$crowding) #people have bias to estimate density more towards average
boxplot(density_est~crowding, data=df_all) #estimated density is totally irrelevent of their experience?!!!



m<-lmer(density_est~crowding+(1|subj_id), data=df_all)
summary(m)

m<-lm(density_est~crowding, data=df_all)
summary(m)

####
#######################3
# Model Comparison

m1<-lmer(est_times-actual_times ~valence+density_est+ crowding + (1|subj_id), data=df_all)
summary(m1) #

m2<-lmer(est_times-actual_times ~ valence  + crowding + (1|subj_id), data=df_all)
summary(m2) #


m3<-lmer(est_times-actual_times ~ valence   + (1|subj_id), data=df_all)
summary(m3) #

m4<-lmer(est_times-actual_times ~valence+density_est+ (1|subj_id), data=df_all)
summary(m4) #

lrtest(m3, m2)

####
#survey correlates
cor.test(df_subj$disgust_senstitivity, df_subj$tp_crowdBias_index)
cor.test(df_subj$disgust_propensity, df_subj$tp_crowdBias_index)

cor.test(df_subj$bis_total, df_subj$tp_avgEstBias)
cor.test(df_subj$bis_nonplaning, df_subj$tp_avgEstBias)
cor.test(df_subj$bis_motor, df_subj$tp_avgEstBias)
cor.test(df_subj$bis_attentional, df_subj$tp_avgEstBias)


#############################
#DCE
##correlation between crowding coefficient and crowding biases
plot(df_subj$tp_crowdBias_index,df_subj$dce_crowding_coef)
ggplot(data=df_subj, aes(x=tp_crowdBias_index, y=dce_crowding_coef))+geom_point()
#  geom_smooth(method = "lm", se=FALSE)
cor.test(df_subj$tp_crowdBias_index,df_subj$dce_crowding_coef)
#cor.test(df_subj$tp_crowdBias_index,df_subj$dce_crowding_coef/df_subj$dce_time_coef)

cor.test(df_subj$valence_crowdBias_index,df_subj$dce_crowding_coef) ### Wow!!! such a correlation!
#cor.test(df_subj$valence_crowdBias_index,df_subj$dce_crowding_coef/df_subj$dce_time_coef) ### Wow!!! such a correlation!
plot(df_subj$valence_crowdBias_index,df_subj$dce_crowding_coef)
abline(lm(df_subj$dce_crowding_coef~ df_subj$valence_crowdBias_index), col="red") # regression line (y~x) 
ggplot(data=df_subj, aes(x=valence_crowdBias_index, y=dce_crowding_coef))+geom_point()+
  geom_smooth(method = "lm", se=FALSE)+ theme_light()

##correlation between crowding coefficient and freq of being in crowded places
cor.test(df_subj$dce_crowding_coef, df_subj$l6_freq)
cor.test(df_subj$dce_crowding_coef, df_subj$l6_freq+df_subj$l4_freq)

cor.test(df_subj$tp_crowdBias_index, df_subj$l6_freq+df_subj$l4_freq) 
plot(df_subj$tp_crowdBias_index, df_subj$l6_freq+df_subj$l4_freq) 

cor.test(df_subj$valence_crowdBias_index, df_subj$l6_freq+df_subj$l6_freq) 
plot(df_subj$valence_crowdBias_index, df_subj$l6_freq+df_subj$l4_freq) 


########################################
#Some plots
#plot avg valence for diff crowding levels
df_all$est_time_bias<-df_all$est_times-df_all$actual_times
mean_valence<-aggregate(df_all$valence,by=list(df_all$crowding), FUN=mean)[,2]
sd_valence<-aggregate(df_all$valence,by=list(df_all$crowding), FUN=sd)[,2]
library(Rmisc)
val_res<-summarySEwithin(data = df_all, "valence", withinvars = c("crowding"), idvar = "subj_id")
tp_res<-summarySEwithin(data = df_all, "est_time_bias", withinvars = c("crowding"), idvar = "subj_id")

df_all$slope_ms_per_60sec <- df_all$slope_RR*60*1000
df_all$scaled_est_time <- df_all$est_times-df_all$mean_est_time
df_all$slope_ms_per_60sec <- df_all$slope_RR*60*1000
ggplot(data = df_all, aes(y=scaled_est_time, x=slope_ms_per_60sec))+
  geom_smooth(span=0.9, color="black")+  theme_classic()+
  xlab("Estimated trip duration")+
  ylab("slope of change in  RR (ms/min)")


  
  ggplot(data = df_all, aes(x=valence, y=slope_ms_per_60sec))+
    geom_smooth(span=0.9)
  xlab("Estimated trip duration")+
    ylab("slope of change in  RR (ms/min)")
  
ggplot(data = df_all, aes(x=est_times_ordered, y=valence))+
  geom_smooth(span=0.95)+
  xlab("Estimated trip duration")+
  ylab("valence")

png(filename="analyze/images/density-valence.png", res=700,  width=2000, height=1600)
ggplot(val_res, aes(x=crowding, y=valence, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=valence-ci, ymax=valence+ci)) +
  geom_point(shape=21, size=3, fill="white") +
  xlab('Density')+
  ylab('Valence')+
  theme_classic()
dev.off()

png(filename="analyze/images/density-TP.png", res=700,  width=2000, height=1600)
ggplot(tp_res, aes(x=crowding, y=est_time_bias, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=est_time_bias-ci, ymax=est_time_bias+ci)) +
  geom_point(shape=21, size=3, fill="white") +
  ylab('Time Estimate Bias')+
  xlab('Density')+
  theme_classic()
dev.off()

####################################
###Mediation Analysis
library(lme4)
detach("package:lmerTest", unload=TRUE)
detach("package:mediation", unload=TRUE)

library(mediation)
med.fit <- lmer(valence~ crowding +(1|subj_id), data=df_all)
out.fit <- lmer(TPB~valence + crowding +(1|subj_id), data=df_all)
direct.fit <- lmer(TPB~ crowding +(1|subj_id), data=df_all)

med.out<- mediate(med.fit, out.fit, treat = "crowding", mediator = "valence",robustSE = TRUE, sims = 100)
summary(med.out)

#***************
detach("package:lmerTest", unload=TRUE)
detach("package:mediation", unload=TRUE)
stdCoef.merMod <- function(object) {
  #obtain standardized coefficients
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}
library(lmerTest)
summary(med.fit)
summary(direct.fit)
summary(out.fit)
stdCoef.merMod(med.fit)
stdCoef.merMod(out.fit)
stdCoef.merMod(direct.fit)

#*
library(diagram)
data <- c(0, "'-0.52***'", 0,
          0, 0          , 0, 
          "'-0.12**'", "'0.07*/0.01'", 0)
M<- matrix (nrow=3, ncol=3, byrow = TRUE, data=data)
par(new=FALSE)
plot<- plotmat (M, pos=c(1,2), 
                name= c( "Valence","Density", "Time Perception\nBias"), 
                box.type = "rect", box.size = 0.12, box.prop=0.5,  curve=0)


##########################################################
library(plotrix)
library(ggplot2)
## French study Q
summary(lmer(T2_FrenchQ_1~crowding+(1|subj_id), data=df_all)) #overcloseness
summary(lmer(T2_FrenchQ_2~crowding+(1|subj_id), data=df_all)) #standing
summary(lmer(T2_FrenchQ_3~crowding+(1|subj_id), data=df_all)) #noise
summary(lmer(T2_FrenchQ_4~crowding+(1|subj_id), data=df_all)) #waste of time
#All are significant

t_color<-c(rep('VR',5),rep('image',5))

#Q1: overcloseness
r<-aggregate(x=df_all$FrenchQ_1, by=list(df_all$FrenchQ_crowding), FUN=mean)
r_sd<-aggregate(x=df_all$FrenchQ_1, by=list(df_all$FrenchQ_crowding), FUN=std.error)
rt<-aggregate(x=df_all$T2_FrenchQ_1, by=list(df_all$crowding), FUN=mean)
rt_sd<-aggregate(x=df_all$T2_FrenchQ_1, by=list(df_all$crowding), FUN=std.error)
X<-c(rt[,1],r[,1])
Y<-c(rt[,2],r[,2])
SD<-c(rt_sd[,2],r_sd[,2])
ggplot(data=NULL,aes(x=X,y=Y, color=t_color))+geom_line()+geom_point()+
  geom_errorbar(aes(ymin=Y-SD, ymax=Y+SD),width=.1)+
  labs(x="Density level", y="rating", title="Over-closedness")

#Q2: standing
r<-aggregate(x=df_all$FrenchQ_2, by=list(df_all$FrenchQ_crowding), FUN=mean, na.rm=TRUE)
r_sd<-aggregate(x=df_all$FrenchQ_2, by=list(df_all$FrenchQ_crowding), FUN=std.error, na.rm=TRUE)
rt<-aggregate(x=df_all$T2_FrenchQ_2, by=list(df_all$crowding), FUN=mean,na.rm=TRUE)
rt_sd<-aggregate(x=df_all$T2_FrenchQ_2, by=list(df_all$crowding), FUN=std.error,na.rm=TRUE)
X<-c(rt[,1],r[,1])
Y<-c(rt[,2],r[,2])
SD<-c(rt_sd[,2],r_sd[,2])
ggplot(data=NULL,aes(x=X,y=Y, color=t_color))+geom_line()+geom_point()+
  geom_errorbar(aes(ymin=Y-SD, ymax=Y+SD),width=.1)+
  labs(x="Density level", y="rating", title="Standing")

#Q3: noise
r<-aggregate(x=df_all$FrenchQ_3, by=list(df_all$FrenchQ_crowding), FUN=mean, na.rm=TRUE)
r_sd<-aggregate(x=df_all$FrenchQ_3, by=list(df_all$FrenchQ_crowding), FUN=std.error, na.rm=TRUE)
rt<-aggregate(x=df_all$T2_FrenchQ_3, by=list(df_all$crowding), FUN=mean,na.rm=TRUE)
rt_sd<-aggregate(x=df_all$T2_FrenchQ_3, by=list(df_all$crowding), FUN=std.error,na.rm=TRUE)
X<-c(rt[,1],r[,1])
Y<-c(rt[,2],r[,2])
SD<-c(rt_sd[,2],r_sd[,2])
ggplot(data=NULL,aes(x=X,y=Y, color=t_color))+geom_line()+geom_point()+
  geom_errorbar(aes(ymin=Y-SD, ymax=Y+SD),width=.1)+
  labs(x="Density level", y="rating", title="Noise")

#Q4: waste of time (Q6 of main survey)
r<-aggregate(x=df_all$FrenchQ_6, by=list(df_all$FrenchQ_crowding), FUN=mean, na.rm=TRUE)
r_sd<-aggregate(x=df_all$FrenchQ_6, by=list(df_all$FrenchQ_crowding), FUN=std.error, na.rm=TRUE)
rt<-aggregate(x=df_all$T2_FrenchQ_4, by=list(df_all$crowding), FUN=mean,na.rm=TRUE)
rt_sd<-aggregate(x=df_all$T2_FrenchQ_4, by=list(df_all$crowding), FUN=std.error,na.rm=TRUE)
X<-c(rt[,1],r[,1])
Y<-c(rt[,2],r[,2])
SD<-c(rt_sd[,2],r_sd[,2])
ggplot(data=NULL,aes(x=X,y=Y, color=t_color))+geom_line()+geom_point()+
  geom_errorbar(aes(ymin=Y-SD, ymax=Y+SD),width=.1)+
  labs(x="Density level", y="rating", title="Waste of Time")

ggplot()+
  geom_line(data=NULL,aes(x=r[,1],y=r[,2]), colour='blue')+geom_point(data=NULL,aes(x=r[,1],y=r[,2]))+
  geom_line(data=NULL,aes(x=rt[,1],y=rt[,2]), colour='red')+geom_point(data=NULL,aes(x=rt[,1],y=rt[,2]))

#
#
crowding_vec<-as.vector(repmat(c(1,2,3,4,5),dim(df_subj)[1],1))
dens_list<-c(0,1,2,4,6)

t<-cbind(df$`FrenchQ_l0#0_1`, df$`FrenchQ_l0#1_1`)


###################
library(MuMIn)
r.squaredGLMM(m)
df_all$TPB_z <- scale(df_all$TPB)
#https://www.tjmahr.com/plotting-partial-pooling-in-mixed-effects-models/
summary(lmer(df))