get_SCRs<-function(subj_id){
  setwd("C:/Users/ss3767/Dropbox/VR crowding Data")
  fName<-find_e4_folder(subj_id);
  fPath<-paste(c("./E4/",fName,"/EDA.csv"), collapse = "");
  
  logs<-read_conditions_logs(subj_id)
  d0<-readLines(fPath,n=2)
  t0<-as.numeric(d0[1])
  freq<-as.numeric(d0[2])
  all_data<-readLines(fPath)
  all_data<-all_data[-c(1,2)] #remove t0 and freq at the beginning of the list
  all_data<-as.numeric(all_data)
}

