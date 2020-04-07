######################################   Plot the 30-days mortality rates for heart attack    #############################
outcome<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[,11]<-as.numeric(outcome[,11])
# windows()
# hist(outcome[,11])

#####################################   Finding the best hospital in a state    ##########################################
options(warn=-1)
best<-function(state, outcome){
    # Read outcome data
    data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
    
    # Check the state and the outcome are valid
    stateCheck<-FALSE
    stateName<-unique(data$State)
    for(s in stateName){
        if(s==state){
            stateCheck<-TRUE
        }
    }
    if(!stateCheck){
        stop("invalid state")
    }
    
    outcomeName<-colnames(data)
    name1<-"Hospital.30.Day.Death..Mortality..Rates.from"
    name2<-strsplit(outcome," ")[[1]]
    outName<-paste(c(name1,name2),collapse = ".")
    name<-""
    for(n in outcomeName){
        if(tolower(n)==tolower(outName)){
            name<-n
        }
    }
    if(name==""){
        stop("invalid outcome")
    }
    
    # Return hospital name in that state with lowest 30-day death rate
    target<-data[data['State']==state,c("Hospital.Name",name)]
    target[,name]<-as.numeric(target[,name])
    target<-target[!is.na(target[name]),]
    lowestRate<-min(target[name])
    target[target[name]==lowestRate,"Hospital.Name"]
}

###################################     Ranking hospitals by outcome in a state     ####################################
rankhospital<-function(state, outcome, num = "best"){
    # Read outcome data
    data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
    
    # Check the state and the outcome are valid
    stateCheck<-FALSE
    stateName<-unique(data$State)
    for(s in stateName){
        if(s==state){
            stateCheck<-TRUE
        }
    }
    if(!stateCheck){
        stop("invalid state")
    }
    
    outcomeName<-colnames(data)
    name1<-"Hospital.30.Day.Death..Mortality..Rates.from"
    name2<-strsplit(outcome," ")[[1]]
    outName<-paste(c(name1,name2),collapse = ".")
    name<-""
    for(n in outcomeName){
        if(tolower(n)==tolower(outName)){
            name<-n
        }
    }
    if(name==""){
        stop("invalid outcome")
    }
    
    # Return hospital name in that state with the given rank 30-day death rate
    target<-data[data['State']==state,c("Hospital.Name",name)]
    target[,name]<-as.numeric(target[,name])
    target<-target[!is.na(target[name]),]
    target<-target[order(target[name],target["Hospital.Name"]),]
    if(num=="best"){
        num<-1
    }
    if(num=="worst"){
        num<-nrow(target)
    }
    target[num,"Hospital.Name"]
}

########################################    Ranking hospitals in all states     ########################################

rankall<-function(outcome, num = "best"){
    # Read outcome data
    data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
    
    # Check the outcome are valid
    outcomeName<-colnames(data)
    name1<-"Hospital.30.Day.Death..Mortality..Rates.from"
    name2<-strsplit(outcome," ")[[1]]
    outName<-paste(c(name1,name2),collapse = ".")
    name<-""
    for(n in outcomeName){
        if(tolower(n)==tolower(outName)){
            name<-n
        }
    }
    if(name==""){
        stop("invalid outcome")
    }
    
    # For each state, find the hospital of the given rank
    data<-data[,c(name,"Hospital.Name","State")]
    data[,name]<-as.numeric(data[,name])
    data<-data[!is.na(data[name]),]
    d<-split(data,data["State"])
    pick<-function(x){
        if(num=="best"){
            num<-1
        }
        if(num=="worst"){
            num<-nrow(x)
        }
        x<-x[order(x[name],x["Hospital.Name"]),]
        x[num,"Hospital.Name"]
    }
    hos<-sapply(d, pick)
    ans<-data.frame(hospital=hos,state=names(d))
    rownames(ans)<-names(d)
    ans
}
