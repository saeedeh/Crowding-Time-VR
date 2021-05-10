read_choices_qualtrics<- function(subj_ids){
  design<-read.csv(paste0(working_dir,"/DCE-design.csv"))
  setwd(data_dir)
  fname.num<-'./qualtrics/data-num.csv'
  fname.txt<-'./qualtrics/data-txt.csv'
  d_num<-read.csv(fname.num, check.names = FALSE)
  d_num<-d_num[-1,]
  d_txt<-read.csv(fname.txt, check.names = FALSE)
  d_txt<-d_txt[-1,]
  #subject_ids
  t<- as.numeric(as.character(d_num$subj_id))
  d_num<-d_num[!is.na(t) & (d_num$Finished==1),];
  d_txt<-d_txt[!is.na(t) &(d_txt$Finished=="True"),];
  df<-data.frame(subj_ids)
  all_subj_ids<- kronecker(matrix(1,1,6), subj_ids)
  choices<-data.frame(subj_id=as.vector(all_subj_ids))
  
  #find the question block of each subject 
  for (subj_id in subj_ids) {
    for (block in c(1:4)) {
      question<-1
      q1_name<-paste0("DCE_", as.character(block), "_", as.character(question))
      subj_ind<-which(d_num$subj_id==subj_id)
      if(as.character(d_num[subj_ind,q1_name])!=""){
        df$block[which(df$subj_ids==subj_id)]<- block
      }
    }  
  }
  
  #iterate all choices
  ind<-0
  for (subj_id in subj_ids) {
    block <- df$block[which(df$subj_ids==subj_id)]
    for (question in c(1:6)) {
      ind<-ind+1
      design_ind<- which(design$Scenario==block & design$Question == question)
      choices$subj_id[ind]<-subj_id
      choices$tt.1[ind]<-design$alt1.tt[design_ind]
      choices$tt.2[ind]<-design$alt2.tt[design_ind]
      choices$cr.1[ind]<-design$alt1.crowd[design_ind]
      choices$cr.2[ind]<-design$alt2.crowd[design_ind]
      q_col_name<- paste0("DCE_", as.character(block), "_", as.character(question))
      choices$response[ind]<-as.numeric(as.character(d_num[d_num$subj_id==subj_id,q_col_name]))
    }
  }
  return(choices)
}

#############################################
library(gmnl)
library(mlogit)
#read data from file
choices<- read_choices_qualtrics(subj_ids)
d<-mlogit.data(choices, choice="response", varying = which(names(choices) %in% c("tt.1", "tt.2","cr.1","cr.2")) ,
            shape = "wide", id="subj_id")
d$tt_times_cr<- d$tt * d$cr
m<-gmnl(response~tt+tt_times_cr|0, model = "mixl", R=500,ranp = c(tt_times_cr='n', tt='n'), panel = TRUE , data = d)
summary(m)

t<-effect.gmnl(m)$mean
df_subj$dce_crowding_coef<-t[,'tt_times_cr']

#### Simulation

t<-summary(m)
m_tt_mean=-0.3251743 
m_tt_sd= 0.0758273
m_ttcr_mean=-0.0584156
m_ttcr_sd=0.0103924
sd_tt_mean=0.3054530
sd_tt_sd=0.0775318
sd_ttcr_mean=0.0231139
sd_ttcr_sd=0.0089729

set.seed(31)

N=10000
tt<-rnorm(N, m_tt_mean, m_tt_sd)
tt.sd<-rnorm(N, sd_tt_mean,sd_tt_sd)
while (sum(tt.sd<0)>0) {
  tt.sd[which(tt.sd<0)]=rnorm(sum(tt.sd<0), sd_tt_mean, sd_tt_sd)  
}

ttcr<-rnorm(N, m_ttcr_mean,m_ttcr_sd)
ttcr.sd<-rnorm(N, sd_ttcr_mean, sd_ttcr_sd)
while (sum(ttcr.sd<0)>0) {
  ttcr.sd[which(ttcr.sd<0)]=rnorm(sum(ttcr.sd<0), sd_ttcr_mean, sd_ttcr_sd)  
}

b_tt<-c(1:N)
b_ttcr<-c(1:N)
tt_over_ttcr<-c(1:N)
for (i in c(1:N)) {
  b_tt[i] <-rnorm(1,tt[i],tt.sd[i])
  b_ttcr[i] <- rnorm(1,ttcr[i],ttcr.sd[i])
  tt_over_ttcr[i]=b_ttcr[i]/b_tt[i]
}

cr<-c(1:5)
cm<-1+mean(tt_over_ttcr)*cr
cm_fake<-1+m_ttcr_mean/m_tt_mean*cr
plot(cr, cm)
library(ggplot2)
library(ggthemes)
png(paste0(working_dir,"/images/CM_Simulation"))
ggplot(data=NULL, aes(x=cr, y=cm))+
  geom_line()+geom_point(col="red", size=2)+
  labs(x="Density Level", y="Crowding Multiplier")+
  theme_hc()+
  coord_cartesian(ylim=c(1, 2)) 
dev.off()
########################
########################
#for each indvidual
t<-effect.gmnl(m)$mean
m_tt_mean=t[,'tt']
m_ttcr_mean=t[,'tt_times_cr']
t<-effect.gmnl(m)$sd.est
m_tt_sd= t[,'tt']
m_ttcr_sd=t[,'tt_times_cr']

set.seed(6)
N=5000
cr<-c(1:5)
n=length(subj_ids);
cm<-matrix(nrow = n, ncol = 5)
for (i in c(1:n)) {
  b_tt<-rnorm(N,m_tt_mean[i], m_tt_sd[i])
  b_ttcr<-rnorm(N,m_ttcr_mean[i],m_ttcr_sd[i])
  ratio <-  mean(b_ttcr/b_tt)
  cm[i,]<- 1+ratio*cr 
}
l=5
x<-data.frame(cr=rep(1:l,n))
x1<-NULL
x2<- NULL
for (c_i in c(1:l)) {
  x1<-c(x1,(cm[,c_i]))
  x2<-c(x2,rep(c_i,n))
}
x$crowding_level<-as.factor(x2)
x$CM<-x1

ggplot( x, aes(x=CM, colour=crowding_level))+geom_density()+
 coord_cartesian(xlim = c(-2,5))

cm_mean<-colMeans(cm)
ggplot(data=NULL, aes(x=cr, y=cm_mean))+
  geom_line()+geom_point(col="red", size=2)+
  labs(x="Density Level", y="Crowding Multiplier")+
  theme_hc()+
  coord_cartesian(ylim=c(1, 2)) 

