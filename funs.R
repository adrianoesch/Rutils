# utility function collection
aggregate2 = function(formula,df,fun=mean,new.names=NULL,metrics=c('mean','n','sd','se')){
  x = as.data.frame(aggregate(formula,df,fun))
  ncols = ncol(x)
  if(!is.null(new.names)){
    names(x) = new.names
  }
  if('n'%in%metrics){
    x$n = aggregate(formula,df,length)[[ncols]]
    if('sd' %in% metrics){
      x$sd = aggregate(formula,df,sd)[[ncols]]    
      if('se' %in% metrics){
        x$se = x$sd/sqrt(x$len)     
      }
    }
  }
  return(x)
}

splitInThree = function(variable){
  x = quantile(variable,c(0,0.333,0.666,1))
  x[length(x)] = x[length(x)]*1.0000001
  x2 = cut(variable,breaks=x,labels=c('low','mid','high'),right=F)
  return(x2)
}

splitInTwo = function(variable){
  x = quantile(variable,c(0,.5,1))
  x[length(x)] = x[length(x)]*1.0000001
  x2 = cut(variable,breaks=x,labels=c('low','high'),right=F)
  return(x2)
}

splitInX = function(variable,x,labels=NULL){
  breaks = quantile(variable,seq(0,1,1/x))
  breaks[length(breaks)] = breaks[length(breaks)]*1.0000001
  if(is.null(labels)){
    labels=paste0('g',seq(1,x))
  }
  x2 = factor(cut(variable,breaks=breaks,labels=labels,right=F),ordered=T,labels=labels)
  return(x2)
}