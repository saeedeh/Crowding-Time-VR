
read_qualtrics_data<-function(){

  setwd(data_dir)
  fname.num<-paste0(data_dir,"/qualtrics/data-num.csv")
  fname.txt<-paste0(data_dir,"/qualtrics/data-txt.csv")
  d_num<-read.csv(fname.num, check.names = FALSE)
  d_num<-d_num[-1,]
  d_txt<-read.csv(fname.txt, check.names = FALSE)
  d_txt<-d_txt[-1,]
  #subject_ids
  t<- as.numeric(as.character(d_num$subj_id))
  d_num
  d_num<-d_num[!is.na(t) & (d_num$Finished==1),];
  d_txt<-d_txt[!is.na(t) &(d_txt$Finished=="True"),];
  subj_ids<- as.numeric(as.character(d_num$subj_id))
  df<-data.frame(subj_ids)
  ###############Personal info
  df$age<-2019-as.numeric(as.character(d_txt$DOB_1))
  df$gender<- substring(as.character(d_txt$Gender),1,1)
  
  ##############Disgust sensitivity
  disgust1<-as.numeric(as.character(d_num$`disgust#1_1`))
  disgust2<-as.numeric(as.character(d_num$`disgust#1_2`))
  disgust3<-as.numeric(as.character(d_num$`disgust#1_3`))
  disgust4<-as.numeric(as.character(d_num$`disgust#1_4`))
  disgust5<-as.numeric(as.character(d_num$`disgust#1_5`))
  disgust6<-as.numeric(as.character(d_num$`disgust#1_6`))
  disgust7<-as.numeric(as.character(d_num$`disgust#1_7`))
  disgust8<-as.numeric(as.character(d_num$`disgust#1_8`))
  disgust9<-as.numeric(as.character(d_num$`disgust#1_9`))
  disgust10<-as.numeric(as.character(d_num$`disgust#1_10`))
  disgust11<-as.numeric(as.character(d_num$`disgust#1_11`))
  disgust12<-as.numeric(as.character(d_num$`disgust#1_12`))
  
  df$disgust4[which(is.na(df$disgust4))] <- median(df$disgust4, na.rm = TRUE)
  df$disgust_senstitivity <- (disgust2 + disgust3 + disgust7 + disgust9 + disgust11 + disgust12) 
  df$disgust_propensity <- (disgust1 + disgust4 + disgust5 + disgust6 + disgust8 + disgust10)
  
  ############### BIS
  BIS_res<-matrix(nrow = dim(d_num)[1], ncol = 30) #one column for each question
  for (i in c(1:30)) {
    bis_name<- paste0('BIS#1_', as.character(i))
    BIS_res[,i]<- as.numeric(as.character(d_num[,bis_name]))
    BIS_res[is.na(BIS_res[,i]),i] <- median(BIS_res[,i], na.rm = TRUE)
  }
  reverse_inds<- c(9,20,30, 1, 7, 8, 12, 13, 10, 15, 29)
  BIS_res[, reverse_inds]<- 5- BIS_res[, reverse_inds]
  
  attentional<-c(5,9, 11, 20, 28, 6, 24, 26)
  motor<- c(2, 3, 4, 17, 19, 22, 25, 16, 21, 23, 30)
  nonplanning <- c(1, 7, 8, 12, 13, 14, 10, 15, 18, 27, 29)
  df$bis_attentional<-rowSums( BIS_res[, attentional])
  df$bis_motor<-rowSums( BIS_res[, motor])
  df$bis_nonplanning<-rowSums( BIS_res[, nonplanning])
  df$bis_total<- df$bis_attentional + df$bis_motor + df$bis_nonplanning
  ###############task1
  df$t1_which<-as.numeric(as.character(d_num$T1_which))
  df$t1_relative<-as.numeric(as.character(d_num$T1_relative_1))
  df$t1_estimate1<-as.numeric(as.character(d_num$T1_estimate1))
  df$t1_estimate2<-as.numeric(as.character(d_num$T1_estimate2))
  
  ###############task2
  #density
  df$t2_1_dens<-as.numeric(as.character(d_num$`1_T2_density_estimate`))
  df$t2_2_dens<-as.numeric(as.character(d_num$`2_T2_density_estimate`))
  df$t2_3_dens<-as.numeric(as.character(d_num$`3_T2_density_estimate`))
  df$t2_4_dens<-as.numeric(as.character(d_num$`4_T2_density_estimate`))
  df$t2_5_dens<-as.numeric(as.character(d_num$`5_T2_density_estimate`))
  
  #Valence
  df$t2_1_pos<-as.numeric(as.character(d_num$`1_T2_pleasant_1`))
  df$t2_2_pos<-as.numeric(as.character(d_num$`2_T2_pleasant_1`))
  df$t2_3_pos<-as.numeric(as.character(d_num$`3_T2_pleasant_1`))
  df$t2_4_pos<-as.numeric(as.character(d_num$`4_T2_pleasant_1`))
  df$t2_5_pos<-as.numeric(as.character(d_num$`5_T2_pleasant_1`))
  
  df$t2_1_neg<-as.numeric(as.character(d_num$`1_T2_unpleasant_1`))
  df$t2_2_neg<-as.numeric(as.character(d_num$`2_T2_unpleasant_1`))
  df$t2_3_neg<-as.numeric(as.character(d_num$`3_T2_unpleasant_1`))
  df$t2_4_neg<-as.numeric(as.character(d_num$`4_T2_unpleasant_1`))
  df$t2_5_neg<-as.numeric(as.character(d_num$`5_T2_unpleasant_1`))
  
  #Time Estimate
  df$t2_1_estimate<-as.numeric(as.character(d_num$`1_T2_estimate`))
  df$t2_2_estimate<-as.numeric(as.character(d_num$`2_T2_estimate`))
  df$t2_3_estimate<-as.numeric(as.character(d_num$`3_T2_estimate`))
  df$t2_4_estimate<-as.numeric(as.character(d_num$`4_T2_estimate`))
  df$t2_5_estimate<-as.numeric(as.character(d_num$`5_T2_estimate`))
  
  
  ## frequency of being in crowded places
  df$level4_freq<-as.numeric(as.character(d_num$demo_level4_freq))
  df$level6_freq<-as.numeric(as.character(d_num$demo_level6_freq))
  
  ## french task 2

  for (trial in c(1:5)) {
    for (question in c(1:4)) {
      q_name<-paste0(as.character(trial),"_T2_FrenchQ#1_",as.character(question))
      new_q_name<-paste0("T2_FrenchQ_",as.character(trial), "_",as.character(question))
      df[,new_q_name]<- as.numeric(as.character(d_num[,q_name]))
    }
  }
  
  #### French images
  for (dens in c(0,1,2,4,6)) {
    for (question in c(1:8)) {
      q_name<-paste0("FrenchQ_l",as.character(dens),"#1_",as.character(question))
      df[,q_name]<- as.numeric(as.character(d_num[,q_name]))
    }
  }
  return(df)
}

reverse_code<- function(ar){
  #reverse code ratings in the 1-4 scale
  return(5-ar)
}
