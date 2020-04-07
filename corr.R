source('complete.R')

corr<-function(directory, threshold = 0){
    obs<-complete(directory)
    id<-obs[obs['nobs']>threshold,]$id
    ans<-c()
    for(i in id){
        data<-read.csv(paste(directory,'/',int2str(i),'.csv',sep = ""))
        data<-data[!(is.na(data['sulfate'])|is.na(data['nitrate'])),]
        sulfate<-data['sulfate']
        nitrate<-data['nitrate']
        ans<-c(ans,cor(sulfate,nitrate))
    }
    ans
}