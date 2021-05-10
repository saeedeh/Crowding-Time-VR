find_e4_folder_time<-function(time_stamp){
  #find the first file that has happened after the time_stamp
  setwd(data_dir)
  e4_dirs<-list.dirs(path = "./E4", full.names = FALSE)
  e4_dirs<-e4_dirs[-1]
  e4_dirs<-sort(e4_dirs)
  folder_times<-NULL
  for(fname in e4_dirs){
    folder_times<-cbind(folder_times, as.numeric(substr(fname,1,10)))
  }
  ind<-findInterval(time_stamp, folder_times)
  return(e4_dirs[ind])
}

read_log<- function(subj_id){
  setwd(data_dir)
  fPath<-paste(c('time logs/s_',as.character(subj_id),'.txt'), collapse = "")
  d<-readLines(fPath)
  return(d)
}

find_e4_folder<-function(subj_id){
  #read first condition time for the subject and find the e4 file name that probably has it
  d<-read_log(subj_id ) 
  cond1<-grep("1_1",d)
  time_line<-read.csv(text =  d[cond1], header = FALSE)
  time_stamp<-time_line[4]
  fName<-find_e4_folder_time(time_stamp)
  return(fName)
}
read_e4_RRs<-function(subj_id){
  #get all corresponding RRs sessoon for subj_id
  #e4_times: col1: timestamp, col2:rr
  setwd(data_dir)
  fName<-find_e4_folder(subj_id);
  fPath<-paste(c("./E4/",fName,"/IBI.csv"), collapse = "");
  d<-read.csv(fPath)
  names(d)<-c("time_stamp","RR")
  d0<-readLines(fPath,n=1)
  t0<-as.numeric(substr(d0,1,unlist(gregexpr(pattern =',',d0))-1))
  #d: col1)time from beginning col2)duration of RR
  d[,1]<-d[,1]+t0
  return(d)
}

read_conditions_logs<-function(subj_id){
  #read parameters of each condition from the log files
  log_d<-read_log(subj_id)
  col_names<-c("crowding", "duration","start_time", "end_time")
  inds1<-grep("1_", log_d)
  inds2<-grep("2_",log_d)
  all1<-read.csv(text =  log_d[inds1], header = FALSE)
  all2<-read.csv(text =  log_d[inds2], header = FALSE)
  all1<-all1[,-1]
  all2<-all2[,-1]
  
  all1<-cbind(all1, all1[,3]+all1[,2])
  all2<-cbind(all2, all2[,3]+all2[,2])
  colnames(all1)<-col_names
  colnames(all2)<-col_names
  #baseline
  ind_b<-grep("Baseline",log_d)
  bl<-NULL
  if(length(ind_b)>0){
    bl<-read.csv(text =  log_d[ind_b], header = FALSE)
    bl<-bl[,-1]
    bl<-cbind(bl, bl[,3]+bl[,2])
    colnames(bl)<-col_names
  }
  
  res<-list("task1"=all1, "task2"=all2, "baseline"=bl)
  return(res)
}
get_RRs_conditions<-function(subj_id){
  task1.RR<-vector(mode="list", length=2)
  task2.RR<-vector(mode = "list", length = 5)
  task1.tt<-vector(mode="list", length=2)
  task2.tt<-vector(mode = "list", length = 5)
  bl.RR<-vector(mode = "list", length = 1)
  bl.tt<-vector(mode = "list", length = 1)
  baseline.RR<-NULL
  baseline.tt<-NULL
  e4_d<-read_e4_RRs(subj_id)
  conds<-read_conditions_logs(subj_id)
  #Task1
  t1_st<-findInterval(conds$task1$start_time,e4_d$time_stamp)
  t1_end<-findInterval(conds$task1$end_time,e4_d$time_stamp)
  task1.RR[[1]]<- e4_d[c(t1_st[1]:t1_end[1]),2]
  task1.RR[[2]]<- e4_d[c(t1_st[2]:t1_end[2]),2]
  task1.tt[[1]]<- e4_d[c(t1_st[1]:t1_end[1]),1]
  task1.tt[[2]]<- e4_d[c(t1_st[2]:t1_end[2]),1]
  
  conds$task1<- list("crowding"=conds$task1$crowding,"duration"=conds$task1$duration, 
                     "start_time"=conds$task1$start_time,"end_time"=conds$task1$end_time,
                     "RR"=task1.RR, "tt"=task1.tt) 
  #Task2
  t2_st<-findInterval(conds$task2$start_time,e4_d$time_stamp)
  t2_end<-findInterval(conds$task2$end_time,e4_d$time_stamp)
  for(i in c(1:5)){
    task2.RR[[i]]<- e4_d[c(t2_st[i]:t2_end[i]),2]
    task2.tt[[i]]<- e4_d[c(t2_st[i]:t2_end[i]),1]
  }
  conds$task2<- list("crowding"=conds$task2$crowding,"duration"=conds$task2$duration, 
                     "start_time"=conds$task2$start_time,"end_time"=conds$task2$end_time
                     , "RR"=task2.RR, "tt"=task2.tt)
  
  #baseline
  if(!is.null(conds$baseline)){
    bl_st<-findInterval(conds$baseline$start_time,e4_d$time_stamp)
    bl_end<-findInterval(conds$baseline$end_time,e4_d$time_stamp)
    bl.RR[[1]]<- e4_d[c(bl_st:bl_end),2]
    bl.tt[[1]]<- e4_d[c(bl_st:bl_end),1]
    conds$baseline<- list("crowding"=conds$baseline$crowding,"duration"=conds$baseline$duration, 
                          "start_time"=conds$baseline$start_time,"end_time"=conds$baseline$end_time,
                          conds$baseline, "RR"=bl.RR, "tt"=bl.tt)
  }
  return(conds)
}


to_orderInds_task2<- function(subj_ids){
  res<-matrix(nrow=length(subj_ids), ncol = 5)
  ind<-0
  for(subj_id in subj_ids){
    ind<-ind+1
    subj_tbl<-get_RRs_conditions(subj_id)
    res[ind,]<-order(subj_tbl$task2$crowding)
  }
  return(res)
}
