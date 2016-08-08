library(dplyr)
library(e1071) 

`|.character` = function(e1,e2){grep(e2,e1,value=T)}
`|.default` =.Primitive("|")
`|` = function (e1, e2) UseMethod('|')

`-.character` = function(e1,e2){ifelse(e1 == "",e2, c(e1,e2))}
`-.default` =.Primitive("-")
`-` = function (e1, e2) UseMethod('-')


amplitude=function(vetor){max(vetor)-min(vetor)}
getmodule=function(dataset){
        quantities=c("accel","gyros","magnet")
        sensors=c("belt","arm","forearm","dumbbell")
        merged=c()
        for(q in quantities){
                for(s in sensors){        
                        prefix=paste(q,s,sep="_")
                        print(prefix)
                        fmla=as.formula(paste0("sqrt(",prefix,"_x^2+",prefix,"_y^2+",prefix,"_z^2) ~ num_window"))
                        lista=lapply(c("mean","sd","var","max","min","amplitude"),function(func){
                                dt=aggregate(fmla,data=dataset,FUN=func)
                                colnames(dt)[2]=paste(func,prefix,sep="_");dt})
                        if(length(merged)==0) merged=select(lista[[1]],num_window)
                        for(dt in lista){merged=merge(merged,dt,by="num_window",all=TRUE)}
                }
        }
        quantities=c("roll","pitch","yaw")
        flist=c("mean","sd","var","max","min","kurtosis","skewness","sum","amplitude")
        for(q in quantities){
                for(s in sensors){        
                        prefix=paste(q,s,sep="_")
                        print(prefix)
                        fmla=as.formula(paste0(prefix," ~ num_window"))
                        lista=lapply(flist,function(func){
                                dt=aggregate(fmla,data=dataset,FUN=func)
                                colnames(dt)[2]=paste(func,prefix,sep="_");dt})
                        if(length(merged)==0) merged=select(lista[[1]],num_window)
                        for(dt in lista){merged=merge(merged,dt,by="num_window",all=TRUE)}
                }
        }
        merged
}


#aincompleteness=unlist(lapply(1:dim(training)[2],function(col){sum(is.na(training[[col]]))}))/dim(training)[1]*100
#aincomplete.dt=data.table(percentage=aincompleteness,description=colnames(training))
#ahighly.incomplete.columns=sum(aincomplete.dt$percentage >1)
#acomplete.columns=sum(aincomplete.dt$percentage == 0)
