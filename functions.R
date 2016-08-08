suppressPackageMessages(library(dplyr))
suppressPackageStartupMessages(library(e1071) )
# Easy usefull operators to make R become bash like.

`|.character` = function(e1,e2){grep(e2,e1,value=T)}
`|.default` =.Primitive("|")
`|` = function (e1, e2) UseMethod('|')

`-.character` = function(e1,e2){grep(e2,e1,value=T,invert=T)}
`-.default` =.Primitive("-")
`-` = function (e1, e2) UseMethod('-')


`+.character` = function(e1,e2){r=e2;if(min(nchar(e1)) > 0) r=c(e1,e2);r}
`+.default` =.Primitive("+")
`+` = function (e1, e2) UseMethod('+')

#Imput value function
amplitude=function(vetor){max(vetor)-min(vetor)}
imput.values=function(dataset){
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

plot.confusion.matrix=function(c){
        print(c)
        ctable=c$table
        con=as.matrix(rbindlist(lapply(1:5,function(i){data.table(t(ctable[i,1:5]/sum(ctable[i,1:5])))})))
        rownames(con)=colnames(con)
        confusion.test=melt(con)
        colnames(confusion.test)=c("Predicted","Reference","value")
        setorder(confusion.test,-Reference)
        setorder(confusion.test,Predicted)
        confusion.test=within(confusion.test,Reference <- factor(ordered(Reference, levels = rev(sort(unique(Reference))))))
        rgb.pal=colorRampPalette(c("white","yellow","red","black"))
        p2 <- ggplot(confusion.test, aes(Predicted, Reference)) %+% geom_tile(aes(fill = (value)) ) %+% 
                scale_fill_gradientn(limits=c(0,1),colours=rgb.pal(11),breaks=0:10/10)%+% 
                geom_text(aes(label=as.integer(value*100)/100),color="blue")
        p2
}

