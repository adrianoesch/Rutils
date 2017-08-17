# utility function collection
aggregate2 = function(formula,df,fun=mean,new.names=NULL){
  x = as.data.frame(aggregate(formula,df,fun))
  ncols = ncol(x)
  if(!is.null(new.names)){
    names(x) = new.names
  }
  x$len = aggregate(formula,df,length)[[ncols]]
  x$sd = aggregate(formula,df,sd)[[ncols]]
  x$se = x$sd/sqrt(x$len)
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