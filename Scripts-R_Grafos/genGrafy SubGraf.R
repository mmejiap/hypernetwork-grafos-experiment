n<-1023
dd<-data.frame(1:n)
dd$bin<-dd$X1.n

for(i in 1:n){
  dd$bin[i]<-numVec2str(numDec2vecBin(i,10))
}

dd$subBin_3nodes<-dd$X1.n

for(i in 1:n){
  dd$subBin_3nodes[i]<-numVec2str(bin2subBinG(dd$bin[i],3))
}

ruta<-"C:/Users/Miguel/Downloads/Compressed/parity1019/10bits_3bits.csv"
write.table(dd[c('bin','subBin_3nodes')],ruta,quote=F,
          row.names = F,col.names = F, sep="\t")


#############################

n2<-2^15 - 1 
dd2<-data.frame(1:n2)
dd2$bin<-dd2$X1.n

for(i in 1:n){
  dd2$bin[i]<-numVec2str(numDec2vecBin(i,10))
}

dd2$subBin_3nodes<-dd2$X1.n

for(i in 1:n2){
  dd2$subBin_3nodes[i]<-numVec2str(bin2subBinG(dd2$bin[i],3))
}


ruta<-"C:/Users/Miguel/Downloads/Compressed/parity1019/15bits_6bits.csv"
write.csv(dd,ruta, col.names = FALSE,sep = '\t',eol='\n')


crearfile<-function(ruta,a,b){
  
  n<-2^a - 1 
  dd<-data.frame(1:n)
  dd$bin<-dd$X1.n
  
  for(i in 1:n){
    dd$bin[i]<-numVec2str(numDec2vecBin(i,a))
  }
  
  dd$subBin<-dd$X1.n
  
  for(i in 1:n){
    dd$subBin[i]<-numVec2str(bin2subBinG(dd$bin[i],b))
  }
  
  
  #ruta<-"C:/Users/Miguel/Downloads/Compressed/parity1019/15bits_6bits.csv"
  
  write.table(dd[c('bin','subBin')],ruta,quote=F,
              row.names = F,col.names = F, sep="\t")
}


ruta_10_3nodos_3<-"C:/Users/Miguel/Downloads/Compressed/parity1019/10bits_3bits_3nodos.csv"
ruta_10_4nodos_6<-"C:/Users/Miguel/Downloads/Compressed/parity1019/10bits_6bits_4nodos.csv"
#ruta_10_4nodos_6<-"C:/Users/Miguel/Downloads/Compressed/parity1019/10bits_10bits.csv"

ruta_15_3nodos_3<-"C:/Users/Miguel/Downloads/Compressed/parity1019/15bits_3bits_3nodos.csv"
ruta_15_4nodos_6<-"C:/Users/Miguel/Downloads/Compressed/parity1019/15bits_6bits_4nodos.csv"
ruta_15_5nodos_10<-"C:/Users/Miguel/Downloads/Compressed/parity1019/15bits_10bits_5nodos.csv"

crearfile(ruta_10_3nodos_3,10,3)
crearfile(ruta_10_4nodos_6,10,4)
crearfile(ruta_15_3nodos_3,15,3)
crearfile(ruta_15_4nodos_6,15,4)
crearfile(ruta_15_5nodos_10,15,5)