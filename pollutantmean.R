pollutantmean<-function(directory, pollutant, id=1:332){
    total<-0
    cnt<-0
    for(i in id){
        data<-read.csv(paste(directory,'/',int2str(i),'.csv',sep = ""))
        data<-data[pollutant]
        data<-data[!is.na(data)]
        total<-total+sum(data)
        cnt<-cnt+length(data)
    }
    total/cnt
}

int2str<-function(x){
    ans<-as.character(x)
    if(x/10<1){
        ans<-paste("0",ans,sep = "")
    }
    if(x/100<1){
        ans<-paste("0",ans,sep = "")
    }
    ans
}