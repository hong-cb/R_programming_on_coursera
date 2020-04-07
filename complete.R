source('pollutantmean.R')

complete<-function(directory, id=1:332){
    nobs<-c()
    for(i in id){
        data<-read.csv(paste(directory,'/',int2str(i),'.csv',sep = ""))
        data<-data[!(is.na(data['sulfate'])|is.na(data['nitrate'])),]
        nobs<-c(nobs,nrow(data))
    }
    ans<-data.frame(id,nobs)
    colnames(ans)<-c('id','nobs')
    ans
}