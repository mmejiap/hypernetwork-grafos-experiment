#
# Autor: Miguel Jesus Mejia Puma
# Universidad Nacional de Ingenieria - 2019
#

rm(list = ls())


bin2vecFull<-function(nDim){
  n <- nDim
  x_nodo<-1:(n-1)
  y_nodo<-2:n
  vector<-c()
  for (i in y_nodo) {
    j<-i
    while(j<=n){
      vector<-c(vector,x_nodo[i-1],j)
      j<-j+1
    }
  }
  vector
}



bin2vecGraph <- function(strBin){
  
  s<-as.numeric(strsplit(strBin,"")[[1]])
  n <- length(s)
  i<-1
  j<-1
  vector=bin2vecFull(dimG <- ((1+(1+8*n)^0.5))/2)
  v = c()
  while(i<=n){
    if( boolN2N(strBin,vector[j],vector[j+1]) ){
      v<-c(v,vector[j],vector[j+1])
    }
    i<-i+1
    j<-2*i-1
  }
  v
}



boolN2N<-function(strBin,nodo1,nodo2){
  # boolN2N() : retorna TRUE si el nodo1 y nodo2 estan conectados segun la cadena binaria
  
  res=FALSE
  s<-as.numeric(strsplit(strBin,"")[[1]])
  dimG <- length(s)
  dimG <- as.integer(((1+(1+8*dimG)^0.5))/2)
  if(nodo1<=dimG && nodo2<=dimG){
    pas <- nodo2-nodo1
    tn <- (nodo1)*(1-nodo1)/2 +dimG*nodo1 - dimG +1
    res = s[tn+pas-1]==1
    res
  }else{
    res
  }
}


bin2plotGraph<-function(strBin,cantSubGrafo=0){
  # bin2plotGraph() : realiza un ploteo de la cadena binaia en un grafo no dirigido
  # strBin  : cadena binaria que representa el grafo
  s<-as.numeric(strsplit(strBin,"")[[1]])
  dimG <- length(s)
  dimG <- as.integer(((1+(1+8*dimG)^0.5))/2)
  title<-paste("G:",strBin)
  if(as.numeric(strBin)==0){
    g<-graph.empty(n = dimG,directed = F)
  }else{
    g<-bin2ObjectGraph(strBin)
  }
  if(cantSubGrafo>=2 && as.numeric(strBin)>0){
    #subG <- bin2ObjectGraph(bin2subBinG(strBin,cantSubGrafo))
    subG <- induced_subgraph(g,1:cantSubGrafo)
    #E(g)$weight<-ifelse(compareEdgeAenB(g,subG), 2,4)
    E(g)$color<-ifelse(compareEdgeAenB(g,subG), 'lightblue','lightgreen')
    title<-paste(title," --  Sub G:",bin2subBinG(strBin,cantSubGrafo))
  }else{
    E(g)$weight<-rep(4,length(E(g)))
    E(g)$color<-rep('lightgreen',length(E(g)))
  }
  E(g)$weight<-rep(4,length(E(g)))
  V(g)$color <- ifelse(V(g)<=cantSubGrafo,'lightblue','green')
  #E(g)$weight <- ifelse((E(g)<=cantSubGrafo),4,2)
  #E(g)$weight<-ifelse(E(g),4,2)
  plot( g, layout=layout.circle,xlim = c(-1, 1),ylim = c(-1, 1),asp = 1,vertex.size=50,edge.width=E(g)$weight, main= title)
}

#######

bin2ObjectGraph<-function(strBin){
  s<-as.numeric(strsplit(strBin,"")[[1]])
  dimG<-length(s)
  dimG<-as.integer(((1+(1+8*dimG)^0.5))/2)
  if(as.numeric(strBin)==0){
    return(graph.empty(n = dimG,directed = F))
  }else{
    return(graph( edges=bin2vecGraph(strBin), n = dimG,directed=F ))
  }
}

#######




bin2GraphPDF<-function(rutaFile,strBin,nNodosSubGrafo=0){
  # bin2GraphPDF: Exporta una cadena binaria a un grafo en PDF
  # rutaFile: nombre y ruta del archivo a exportar
  # strBin  : cadena binaria que representa el grafo
  pdf(rutaFile,paper='a4',width = 7,height = 7)
  bin2plotGraph(strBin,cantSubGrafo = nNodosSubGrafo)
  dev.off()
}



#############
#bin2subGrafBin<-function(){#(strbin,n_primeros_nodos){
  #s<-as.numeric(strsplit(strBin,"")[[1]])
  #len_s<-length(s)
#  ii<-rep(TRUE,len_s)
#  n<-n_primeros_nodos
#  while()

#}


indexSubG <- function(cantNodosG, cantNodosSubG){
  
  index<-c()
  cN<-cantNodosG
  csN<-cantNodosSubG
  n<-csN-1
  tope<-c()
  f<-cN-csN
  for(i in 1:n){
    tope<-c(tope,(((-i^2+(2*cN-1)*i)/2) - f))
  }
  
  j<-1
  cont<-0
  
  while((j<=(cN*(cN-1)/2)) && (cont<n)){
    index<-c(index,j)
    if(j %in% tope){
      cont<-cont+1
      j<-j+f
    }
    j<-j+1
  }

  return(index)
}

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

bin2subBinG<-function(strBin,nfNodos){
  s<-as.numeric(strsplit(strBin,"")[[1]])
  dimG<-length(s)
  ii<-indexSubG((1+sqrt(1+8*dimG))/2,nfNodos)
  #print(ii)
  return(numVec2str(s[ii]))
}
#############

elementInVector<-function(a,vec_E){
  #m2<-igraph::get.edgelist(g2)
  for(i in 1:length(vec_E)){
    if(a==vec_E[i]){
      return(TRUE)
    }
  }
  return(FALSE)
}

compareVectorAenB<-function(enA,B){
  if(length(enA)==1 && length(B)>1){
    return(elementInVector(enA,B))
  }else if(length(B)==1 && length(A)>1){
    r<-rep(FALSE,length(enA))
    for (i in 1:length(enA)) {
      r[i]<-elementInVector(enA[i],B)
    }
    return(r)
  }else if(length(enA)==1 && length(B)==1){
    return(enA==B)
  }else{
    r<-rep(FALSE,length(enA))
    for(i in 1:length(enA)){
      print(paste("-> ",i,enA[i],"==",B))
      r[i]<-elementInVector(enA[i],B)
    }
    return(r)
  }
  return(FALSE)
}

elementInVector<-function(a,vec_E){
  r<-rep(FALSE,length(vec_E))
  for(i in 1:length(vec_E)){
    b<-FALSE
    j<-0
    if(length(a)==1){
      if(a[j]==vec_E[i]){
        b<-FALSE
      }
    }
    while(j<=length(a) && b==FALSE){
      if(a[j]==vec_E[i]){
        b<-TRUE
      }
      j<-j+1
    }
    r<-c(r,b)
  }
  return(r)
}

rowCompareMatrix<-function(row,m){
  #print(row)
  #print(m)
  for(i in 1:length(m[,1])){
    #print(paste(row[1]==m[i,1],"->",row[2]==m[i,2], " = ",(row[1]==m[i,1])&&(row[2]==m[i,2])))
    #print(paste("---->",row[1],row[2],"<--->",m[i,1],m[i,2]))
    if((row[1]==m[i,1])&&(row[2]==m[i,2]))
      return(TRUE)
  }
  return(FALSE)
}

compareEdgeAenB<-function(enA,B){
  mA<-igraph::get.edgelist(enA)
  mB<-igraph::get.edgelist(B)
  #print(mA)
  #print(mB)
  if(length(E(enA))==0 && length(E(B))>0){
    mA<-matrix(rep(0,2),ncol = 2)
  }else if(length(E(enA))>0 && length(E(B))==0){
    mB<-matrix(rep(0,2),ncol = 2)
  }else{
    r<-rep(FALSE,length(E(enA)))
    for(i in 1:length(E(enA))){
      if(rowCompareMatrix(mA[i,],mB)){
        r[i]<-TRUE
      }
    }
    return(r)
  }
  
}
