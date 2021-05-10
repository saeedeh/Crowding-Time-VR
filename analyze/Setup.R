subj_ids<-c( 100,101,104, 105,106,108, 109, 110, 111, 112,113, 114, 115, 116, 117, 118, 119, 120, 121, 
             122, 123, 124, 125, 126:130, 132:136, 138:145) #whats wrong with 131?
working_dir='/home/saeedeh/Desktop/uni Archive/Ricardo/VR-Crowding project/VR-Crowding-code+data/analyze'
data_dir='/home/saeedeh/Desktop/uni Archive/Ricardo/VR-Crowding project/VR-Crowding-code+data'
source(paste0(working_dir,'/e4 and logs functions.R'))
source(paste0(working_dir,'/Qualtrics data functions.R'))
source(paste0(working_dir,'/HRV-analysis.R'))
########################### setup
df<-read_qualtrics_data()

df_subj<- data.frame(subj_ids)
all_subj_ids<- kronecker(matrix(1,1,5), subj_ids)
all_crowding<- kronecker(matrix(1,length(subj_ids),1), t(c(1:5)))
df_all<- data.frame(subj_id=as.vector(all_subj_ids),crowding= as.vector(all_crowding))

source(paste0(working_dir,'/DCE-analysis.R'))

###HRV : 
# mean_RR1_ordered (N*2 : mean RR task1 ordered) 
# mean_RR2_ordered (N*5 : mean RR task2 ordered)
#baseline_RR
mean_RR1_ordered<-matrix(nrow = length(subj_ids), ncol = 2)
mean_RR2_ordered<-matrix(nrow = length(subj_ids), ncol = 5)
baseline_RR<-list()

RR_slope1_ordered<-matrix(nrow = length(subj_ids), ncol = 2)
RR_slope2_ordered<-matrix(nrow = length(subj_ids), ncol = 5)
baseline_slope<-list()

mean_rMSSD1_ordered<-matrix(nrow = length(subj_ids), ncol = 2)
mean_rMSSD2_ordered<-matrix(nrow = length(subj_ids), ncol = 5)
baseline_rMSSD<-list()

mean_LF1_ordered<-matrix(nrow = length(subj_ids), ncol = 2)
mean_LF2_ordered<-matrix(nrow = length(subj_ids), ncol = 5)
baseline_LF<-list()

individual<-matrix(nrow = length(subj_ids), ncol = 5)
crowding<-matrix(nrow = length(subj_ids), ncol = 5)
ind<-0
for(subj_id in subj_ids){
  ind<-ind+1;
  subj_tbl<-get_RRs_conditions(subj_id)
  #mean
  mean_RR<-get_mean_rr(subj_id)
  baseline_RR[ind]<-mean(unlist(subj_tbl$baseline$RR))
  mean_RR1_ordered[ind,]<-unlist(mean_RR$mean_RR_t1[order(subj_tbl$task1$crowding)])
  mean_RR2_ordered[ind,]<-unlist(mean_RR$mean_RR_t2[order(subj_tbl$task2$crowding)])
  
  #slope of heart rate change
  slope_rr<-get_slope_rr(subj_id)
  baseline_slope[ind] <- slope_rr$slope_bl
  RR_slope1_ordered[ind,]<-unlist(slope_rr$slope_task1)[order(subj_tbl$task1$crowding)]
  RR_slope2_ordered[ind,]<-unlist(slope_rr$slope_task2)[order(subj_tbl$task2$crowding)]
  
  ###### HRV
  hrv_res<-HRV_analyze(subj_id)
  #####
  #rMSSD
  baseline_rMSSD[ind]<-hrv_res$hrv_bl$TimeAnalysis[[1]]$rMSSD
  if(length(hrv_res$hrv_task1)>0){
    mean_rMSSD1_ordered[ind,]<-hrv_res2ar(hrv_res$hrv_task1,'rMSSD','t')[order(subj_tbl$task1$crowding)]
  }
  if(length(hrv_res$hrv_task2)>0){
    mean_rMSSD2_ordered[ind,]<-hrv_res2ar(hrv_res$hrv_task2,'rMSSD','t')[order(subj_tbl$task2$crowding)]
  }


  #LF
  baseline_LF[ind]<-hrv_res$hrv_bl$FreqAnalysis[[1]]$LF
  if(length(hrv_res$hrv_task1)>0){
    mean_LF1_ordered[ind,]<-hrv_res2ar(hrv_res$hrv_task1,'LF','f')[order(subj_tbl$task1$crowding)]
  }
  if(length(hrv_res$hrv_task2)>0){
    mean_LF2_ordered[ind,]<-hrv_res2ar(hrv_res$hrv_task2,'LF','f')[order(subj_tbl$task2$crowding)]
  }
  individual[ind,]<-ind
  crowding[ind,]<-c(1:5)
}
df_subj$baseline_RR<- unlist(baseline_RR)
df_subj$tas1_RR_less_crowded<-mean_RR1_ordered[,1]
df_subj$tas1_RR_more_crowded<-mean_RR1_ordered[,2]
df_all$mean_RR<- as.vector(mean_RR2_ordered)

df_subj$baseline_slope<- unlist(baseline_slope)
df_all$slope_RR <- as.vector(RR_slope2_ordered)
df_all$delta_RR <- df_all$slope_RR*df_all$actual_times

df_all$slope_RR[df_all$slope_RR > 0.003]= NA
#df_all$slope_RR[df_all$slope_RR < -2.5]

df_all$rMSSD<-as.vector(mean_rMSSD2_ordered)
baseline_rMSSD<-as.numeric(as.character(baseline_rMSSD))
df_subj$baseline_rMSSD<-baseline_rMSSD

df_all$LF<-as.vector(mean_LF2_ordered)
baseline_LF<-as.numeric(as.character(baseline_LF))
df_subj$baseline_LF<-baseline_LF

#### Task 1 time estimates

t<-is.element(df$subj_ids, subj_ids)
df<-df[t,]

ind<-0
first_more_crowded<-list()
est_more_crowded<-list()
est_less_crowded<-list()
for(subj_id in subj_ids){
  ind<-ind+1;
  df_obs=df[df$subj_ids==subj_id,]
  subj_tbl<-read_conditions_logs(subj_id)
  first_more_crowded[ind]<-subj_tbl$task1$crowding[1]>subj_tbl$task1$crowding[2]
  if(first_more_crowded[ind]==TRUE){
    est_more_crowded[ind]<-df_obs$t1_estimate1
    est_less_crowded[ind]<-df_obs$t1_estimate2
  }
  else{
    est_more_crowded[ind]<-df_obs$t1_estimate2
    est_less_crowded[ind]<-df_obs$t1_estimate1
  }
}

df_subj$task1_est_more_crowded<- unlist(est_more_crowded)
df_subj$task1_est_less_crowded<- unlist(est_less_crowded)
df_subj$task1_est1 <- df$t1_estimate1
df_subj$task1_est2<- df$t1_estimate2
df_subj$task1_choice <- df$t1_which
df_subj$task1_chose_more_crowded <- (df$t1_which==1 & unlist(first_more_crowded)) | (df$t1_which==2 & !unlist(first_more_crowded)) 
df_subj$first_more_crowded <- unlist(first_more_crowded)

## Task2 time estimates
est_times_ordered<-matrix(nrow = length(subj_ids), ncol = 5)
actual_times_ordered<-matrix(nrow = length(subj_ids), ncol = 5)
individual<-matrix(nrow = length(subj_ids), ncol = 5)
crowding<-matrix(nrow = length(subj_ids), ncol = 5)
condition_order<- kronecker(matrix(1,length(subj_ids),1), t(c(1:5)))

df_col_names<-c("t2_1_estimate","t2_2_estimate","t2_3_estimate","t2_4_estimate","t2_5_estimate")
ind<-0
for(subj_id in subj_ids){
  ind<-ind+1;
  df_obs=df[df$subj_ids==subj_id,]
  subj_tbl<-read_conditions_logs(subj_id)
  crowding_order<-order(subj_tbl$task2$crowding)
  actual_times_ordered[ind,]<-subj_tbl$task2$duration[crowding_order]
  est_times_ordered[ind,]<-unlist(df_obs[df_col_names[crowding_order]])
  individual[ind,]<-ind
#  crowding[ind,]<-c(1:5)
  condition_order[ind, ]<-condition_order[ind,crowding_order]
}
df_all$actual_times<- as.vector(actual_times_ordered)
df_all$est_times<- as.vector(est_times_ordered)
df_all$trip_num <- as.vector(condition_order)

## Valence and arousal

feeling_pos<-cbind(df$t2_1_pos, df$t2_2_pos,df$t2_3_pos,df$t2_4_pos,df$t2_5_pos)
feeling_neg<-cbind(df$t2_1_neg, df$t2_2_neg,df$t2_3_neg,df$t2_4_neg,df$t2_5_neg)
orders<-to_orderInds_task2(subj_ids)
valence_ordered<- matrix(nrow = length(subj_ids), ncol = 5)
arousal_ordered<- matrix(nrow = length(subj_ids), ncol = 5)
arousal_ordered2<- matrix(nrow = length(subj_ids), ncol = 5)
mixed_feeling_ordered<- matrix(nrow = length(subj_ids), ncol = 5)

for (ind in c(1:length(subj_ids))) {
  v<- feeling_pos[ind,]-feeling_neg[ind,]
  a<- feeling_pos[ind,]+feeling_neg[ind,]
  m<- pmax(feeling_pos[ind,],feeling_neg[ind,])
  mi<- pmin(feeling_pos[ind,],feeling_neg[ind,])
  valence_ordered[ind,]<-v[orders[ind,]]
  arousal_ordered[ind,]<-a[orders[ind,]]
  arousal_ordered2[ind,]<-m[orders[ind,]]
  mixed_feeling_ordered[ind,]<-mi[orders[ind,]]
}
df_all$valence<- as.vector(valence_ordered)
df_all$arousal<- as.vector(arousal_ordered)
df_all$arousal2<- as.vector(arousal_ordered2)
df_all$mixed_feelings<- as.vector(mixed_feeling_ordered)
df_all$valence_abs <- abs(df_all$valence)

#density estimate
dens<-cbind(df$t2_1_dens,df$t2_2_dens,df$t2_3_dens,df$t2_4_dens, df$t2_5_dens)
orders<-to_orderInds_task2(subj_ids)
dense_ordered<- matrix(nrow = length(subj_ids), ncol = 5)
for (ind in c(1:length(subj_ids))) {
  o<-orders[ind,]
  dense_ordered[ind,]<-dens[ind,o]
}
df_all$density_est <- as.vector(dense_ordered)


#survery variables
df_subj$age<- df$age
df_subj$gender<- df$gender
df_subj$disgust_senstitivity<- df$disgust_senstitivity
df_subj$disgust_propensity<- df$disgust_propensity

df_subj$bis_total <- df$bis_total
df_subj$bis_nonplaning<-df$bis_nonplanning
df_subj$bis_motor<-df$bis_motor
df_subj$bis_attentional<-df$bis_attentional


# frequency of being in crowded places
df_subj$l4_freq<- df$level4_freq
df_subj$l6_freq<- df$level6_freq

# French study task 2

orders<-to_orderInds_task2(subj_ids)
FQ_ordered<- matrix(nrow = length(subj_ids), ncol = 5)
FQ<-c(1:5)
for (question in c(1:4)) {
  for (ind in c(1:length(subj_ids))) {
    for (trial in c(1:5)) {
      q_name<-paste0("T2_FrenchQ_",as.character(trial), "_",as.character(question))
      FQ[trial]<-df[ind, q_name]
    }
    FQ_ordered[ind,]<-FQ[orders[ind,]]
  }
  q_name<-paste0("T2_FrenchQ_",as.character(question))
  df_all[,q_name]<-as.vector(FQ_ordered)
}
df_all$valence<- as.vector(valence_ordered)

#French study questions images
orders<-to_orderInds_task2(subj_ids)
FQ_all<- matrix(nrow = length(subj_ids), ncol = 5)
FQcr_all<- matrix(nrow = length(subj_ids), ncol = 5)
FQ<-c(1:5)
FQ_crowding<-c(1:5)
dens_list<-c(0,1,2,4,6)
for (question in c(1:8)) {
  for (ind in c(1:length(subj_ids))) {
    for (dens in c(0,1,2,4,6)) {
      q_name<-paste0("FrenchQ_l", as.character(dens),"#1_",as.character(question))
      FQ[which(dens_list==dens)]<-df[ind, q_name]
      FQ_crowding[which(dens_list==dens)] <- dens
    }
    #FQ_ordered[ind,]<-FQ[orders[ind,]]
    FQ_all[ind,]<-FQ
    FQcr_all[ind,]<-FQ_crowding
  }
  q_name<-paste0("FrenchQ_",as.character(question))
  df_all[,q_name]<-as.vector(FQ)
  df_all[,'FrenchQ_crowding']<-as.vector(FQ_crowding)
}
df_all$valence<- as.vector(valence_ordered)




################################
# More advanced estimates

# estimate TP_bias

df_all$TPB<-df_all$est_times-df_all$actual_times
df_all$absTPB<-abs(df_all$est_times-df_all$actual_times)

ind<-0
for (subj_id in subj_ids) {
  ind<-ind+1
  subj_inds <- which(df_all$subj_id==subj_id)
  ### tp slope
  y<-df_all$est_times[subj_inds]-df_all$actual_times[subj_inds]
  x<-df_all$crowding[subj_inds]
  m<- coef(lm(y~x))
  df_subj$tp_crowdBias_index[ind]<-m['x']
  ### valence slope
  y<-df_all$valence[subj_inds]
  x<-df_all$crowding[subj_inds]
  m<- coef(lm(y~x))
  df_subj$valence_crowdBias_index[ind]<-m['x']
  ### accuracy slope
  y<-df_all$actual_times[subj_inds]
  x<-df_all$est_times[subj_inds]
  x<- (x-mean(x))/sd(x)
  t<- (y-mean(y))/sd(y)
  if(is.na(x) || is.na(y)){
    df_subj$acc[ind] <- NaN
  }
  else{
    m<- coef(lm(y~x))
    df_subj$acc[ind]<-m['x']
  }
}

#average bias of a subject
ind<-0
for (subj_id in subj_ids) {
  ind<-ind+1
  subj_inds <- which(df_all$subj_id==subj_id)
  y<- mean(df_all$est_times[subj_inds]-df_all$actual_times[subj_inds])
  df_subj$tp_avgEstBias[ind]<-y
}


############################################
#Functions
rep_subj2all <- function(ar){
  ar2<-kronecker(matrix(1,1,5), ar)
  ar2<- as.vector(ar2)
  return(ar2)
}

hrv_res2ar<- function(hrv_res_list,measure,domain){
  #domain: f=freq, t=time
  res<-numeric(length = length(hrv_res_list))
  if(domain=='t'){
    for (i in c(1:length(hrv_res_list))) {
      if(is.null( unlist(hrv_res_list[[i]]))){
        res[i]<-NA
      }
      else{
        res[i]<-hrv_res_list[[i]]$TimeAnalysis[[1]][measure]
      }
    }
  }
  else if(domain=='f'){
    for (i in c(1:length(hrv_res_list))) {
      if(is.null(unlist(hrv_res_list[[i]]))){
        res[i]<-NA
      }
      else{
        res[i]<-hrv_res_list[[i]]$FreqAnalysis[[1]][measure]
      }
    }
  }
  return(unlist(res))
}

#############
#### aggregating over subj_id

t <- df_all %>% group_by(subj_id) %>% summarise(mean_slope=mean(slope_RR, na.rm=TRUE))
df_subj$mean_slope <- t$mean_slope

t <- df_all %>% group_by(subj_id) %>% summarise(mean_est_time=mean(est_times, na.rm=TRUE))
df_subj$mean_est_time <- t$mean_est_time

t <- df_all %>% group_by(subj_id) %>% summarise(mean_rMSSD=mean(rMSSD, na.rm=TRUE))
df_subj$mean_rMSSD <- t$mean_rMSSD

df_all$mean_est_time <- NaN
for (i in seq(subj_ids)) {
  inds=df_all$subj_id == subj_ids[i]
  df_all$mean_est_time[inds]=df_subj$mean_est_time[i]
}
