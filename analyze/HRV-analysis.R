library(RHRV)
library(pracma)
library(Bolstad2)
RRtime2beats<- function(RR_times, RR_lengths){
  if(length(RR_times)<2){
    return(NULL)
  }
  seqs<-NULL
  last_t<- -1
  for (i in c(1:length(RR_times))) {
    st<-RR_times[i]-RR_lengths[i]
    fn<-RR_times[i]
    if(abs(st-last_t)>0.01){
      seqs<-append(seqs,st)
    }
    seqs<-append(seqs,fn)
    last_t<-fn
  }
  return(seqs)
}
get_rhrv_data<- function(RR_lengths, RR_times){
  if(length(RR_lengths)<5){
    return(list(NULL))
  }
  beat_times <- RRtime2beats(RR_times = RR_times , RR_lengths = RR_lengths)
  hrv.data<-CreateHRVData()
  hrv.data <- SetVerbose(hrv.data, FALSE)
  hrv.data$Beat$Time<-beat_times
  hrv.data <- BuildNIHR(hrv.data)
  #EditNIHR(hrv.data)
  hrv.data <- FilterNIHR(hrv.data)
  hrv.data <- InterpolateNIHR(hrv.data)
  hrv.data <- CreateTimeAnalysis(hrv.data)
  hrv.data <- CreateFreqAnalysis(hrv.data)
  size<-( beat_times[length(beat_times)]-beat_times[1]-1)
  hrv.data <- CalculatePSD(hrv.data, indexFreqAnalysis = 1, method = "lomb", doPlot = F)
  t<-hrv.data$FreqAnalysis[[1]]$periodogram
  hf_inds<-t$freq>0.15 & t$freq<0.4
  lf_inds<-t$freq<0.15 & t$freq<0.05
  hrv.data$FreqAnalysis[[1]]$periodogram$hf_mean<- trapz(t$freq[hf_inds] ,t$spec[hf_inds] )
  hrv.data$FreqAnalysis[[1]]$periodogram$lf_mean<- trapz(t$freq[!hf_inds] ,t$spec[!hf_inds] )
  hrv.data<-CalculatePowerBand(hrv.data, shift = 1, size = size)
  return(hrv.data)
}

HRV_analyze<-function(sub_id,func_name){
  d<-get_RRs_conditions(subj_id)
  hrv_task1<-list()
  hrv_task1[[1]]<- get_rhrv_data(RR_lengths =  d$task1$RR[[1]],RR_times =  d$task1$tt[[1]])
  hrv_task1[[2]]<- get_rhrv_data(RR_lengths =  d$task1$RR[[2]],RR_times =  d$task1$tt[[2]])
  
  hrv_bl<- get_rhrv_data(RR_lengths =  d$baseline$RR[[1]],RR_times =  d$baseline$tt[[1]])
  
  hrv_task2<-list()
  for (i in c(1:5)) {
    t<-get_rhrv_data(RR_lengths =  d$task2$RR[[i]],RR_times =  d$task2$tt[[i]])
    hrv_task2[[i]]<- t
  }
  res<-list("hrv_task1"=hrv_task1, "hrv_task2"=hrv_task2, "hrv_bl"=hrv_bl)
  return(res)
}
get_mean_rr<-function(subj_id){
  func_name<-'mean'
  d<-get_RRs_conditions(subj_id)
  mean1<-lapply(d$task1$RR,func_name)
  mean2<-lapply(d$task2$RR,func_name)
  mean_bl=NA;
  if(!is.null(d$baseline)){
    mean_bl<-lapply(d$baseline$RR, func_name)
  }
  res=list("mean_RR_t1"=mean1, "mean_RR_t2"=mean2, "mean_RR_bl"=mean_bl)
  return(res)
}

estimate_slope <- function(X,Y){
#  X<- XY$X
#  Y<- XY$Y
  if(is.null(X) || is.null(Y) || length(X)<2){
    slope= NA
    return(slope)
  }
  X = X - X[1]
  s<- summary(lm( Y ~ X))
  slope <- s$coefficients[2,1]
  return(slope)
}

get_slope_rr <- function(subj_id){
  func_name<-'estimate_slope'

  
  d<-get_RRs_conditions(subj_id)
  slope_task1<-list()
  slope_task1[[1]]<- estimate_slope(Y =  d$task1$RR[[1]],X =  d$task1$tt[[1]])
  slope_task1[[2]]<- estimate_slope(Y =  d$task1$RR[[2]],X =  d$task1$tt[[2]])
  
  slope_bl<- estimate_slope(Y =  d$baseline$RR[[1]],X =  d$baseline$tt[[1]])
  
  slope_task2<-list()
  for (i in c(1:5)) {
    t<-estimate_slope(Y =  d$task2$RR[[i]],X =  d$task2$tt[[i]])
    slope_task2[[i]]<- t
  }
  res<-list("slope_task1"=slope_task1, "slope_task2"=slope_task2, "slope_bl"=slope_bl)
  return(res)
  
  
}
