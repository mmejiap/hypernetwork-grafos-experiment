numDec2vecBin <- function(n,cantBits){
  bin <- rev(as.numeric(intToBits(n)))
  if(missing(cantBits)){
    return(bin)
  }else{
    bin[-(1:(length(bin)-cantBits))]
  }
}

numVec2str <- function(v){
  dim<-length(v)
  s<-""
  for(i in 1:dim){
    s<-paste(s,v[i],sep = "")
  }
  return(s)
}

