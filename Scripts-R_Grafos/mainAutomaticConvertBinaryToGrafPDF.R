
#
# Autor: Miguel Jesus Mejia Puma
# Universidad Nacional de Ingenieria - 2019
#



rutaFile <- "C:/Users/Miguel/Downloads/Compressed/parity1019/parity1019/10bit/data10graph1.csv"

df<-read.csv(file=rutaFile, header=FALSE,sep = "\t",skip=1, colClasses = c(V1="character",V7="numeric"))

borrar <- c("V2","V3","V4","V5","V6")
df<-df[ , !(names(df) %in% borrar)]
names(df)<- c("binGraf","parity")


rFiles = "../Downloads/Compressed/parity1019/parity1019/10bit/"
for(e in 1:40){
  vecBinario <- df[["binGraf"]][e]
  bin2GraphPDF(paste(rFiles,vecBinario,'.pdf',sep = ''),vecBinario)
}

#10_3
rutaFile_10_3 <- "C:/Users/Miguel/Downloads/Compressed/parity1019/10bits_3bits_3nodos.csv"
df_10_3 = read.csv(file = rutaFile_10_3,header=FALSE,sep='\t',colClasses = c(V1="character",V2="character"))
rutaExportPDF <- "C:/Users/Miguel/Downloads/Compressed/parity1019/10b_3b/"
for(e in 1:dim(df_10_3)[1]){
#for(e in 1:1023){
  binGraph <- df_10_3$V1[e]
  binSubGraph<-df_10_3$V2[e]
  f<-paste(rutaExportPDF,binGraph,"_",binSubGraph,'.pdf',sep="")
  print(f)
  bin2GraphPDF(f,binGraph,3)
}


#10_6
rutaFile_10_6 <- "C:/Users/Miguel/Downloads/Compressed/parity1019/10bits_6bits_4nodos.csv"
df_10_6 = read.csv(file = rutaFile_10_6,header=FALSE,sep='\t',colClasses = c(V1="character",V2="character"))
df<-df_10_6
rutaExportPDF <- "C:/Users/Miguel/Downloads/Compressed/parity1019/10b_6b/"
for(e in 1:dim(df)[1]){
  #for(e in 1:1023){
  binGraph <- df$V1[e]
  binSubGraph<-df$V2[e]
  f<-paste(rutaExportPDF,binGraph,"_",binSubGraph,'.pdf',sep="")
  print(f)
  bin2GraphPDF(f,binGraph,3)
}


#15_3
rutaFile_15_3 <- "C:/Users/Miguel/Downloads/Compressed/parity1019/15bits_3bits_3nodos.csv"
df_15_3 = read.csv(file = rutaFile_15_3,header=FALSE,sep='\t',colClasses = c(V1="character",V2="character"))
df<-df_15_3
rutaExportPDF <- "C:/Users/Miguel/Downloads/Compressed/parity1019/15b_3b/"
for(e in 1:dim(df)[1]){
  #for(e in 1:1023){
  binGraph <- df$V1[e]
  binSubGraph<-df$V2[e]
  f<-paste(rutaExportPDF,binGraph,"_",binSubGraph,'.pdf',sep="")
  #print(f)
  bin2GraphPDF(f,binGraph,3)
}


#15_6
rutaFile_15_6 <- "C:/Users/Miguel/Downloads/Compressed/parity1019/15bits_6bits_4nodos.csv"
df_15_6 = read.csv(file = rutaFile_15_6,header=FALSE,sep='\t',colClasses = c(V1="character",V2="character"))
df<-df_15_6
rutaExportPDF <- "C:/Users/Miguel/Downloads/Compressed/parity1019/15b_6b/"
for(e in 1:dim(df)[1]){
  #for(e in 1:1023){
  binGraph <- df$V1[e]
  binSubGraph<-df$V2[e]
  f<-paste(rutaExportPDF,binGraph,"_",binSubGraph,'.pdf',sep="")
  #print(f)
  bin2GraphPDF(f,binGraph,3)
}


#15_10
rutaFile_15_10 <- "C:/Users/Miguel/Downloads/Compressed/parity1019/15bits_10bits_5nodos.csv"
df_15_10 = read.csv(file = rutaFile_15_10,header=FALSE,sep='\t',colClasses = c(V1="character",V2="character"))
df<-df_15_10
rutaExportPDF <- "C:/Users/Miguel/Downloads/Compressed/parity1019/15b_10b/"
for(e in 1:dim(df)[1]){
  #for(e in 1:1023){
  binGraph <- df$V1[e]
  binSubGraph<-df$V2[e]
  f<-paste(rutaExportPDF,binGraph,"_",binSubGraph,'.pdf',sep="")
  #print(f)
  bin2GraphPDF(f,binGraph,3)
}


genFullGrafosPDF<-function(rutaFileCSV,rutaExportFiles,cantBitG,cantBitSubG){
  df<-read.csv(file = rutaFileCSV,header=FALSE,sep='\t',colClasses = c(V1='character',V2='character'))
  nNodos<-(1+sqrt(1+8*cantBitSubG))/2
  for(e in 1:(dim(df)[1])){
    binG<-df$V1[e]
    binSubG<-df$V2[e]
    f<-paste(rutaExportFiles,binG,"_",binSubG,".pdf",sep="")
    bin2GraphPDF(f,binG,nNodos)
  }
}

rutaFile_10_3 <- "C:/Users/Miguel/Downloads/Compressed/parity1019/10bits_3bits_3nodos.csv"
rutaFilesExport_10_3 <- "C:/Users/Miguel/Downloads/Compressed/parity1019/10b_3b/"
genFullGrafosPDF(rutaFileCSV = rutaFile_10_3,rutaExportFiles = rutaFilesExport_10_3,cantBitG = 10,cantBitSubG = 3)


rutaFile_10_6 <- "C:/Users/Miguel/Downloads/Compressed/parity1019/10bits_6bits_4nodos.csv"
rutaFilesExport_10_6 <- "C:/Users/Miguel/Downloads/Compressed/parity1019/10b_6b/"
genFullGrafosPDF(rutaFileCSV = rutaFile_10_6,rutaExportFiles = rutaFilesExport_10_6,cantBitG = 10,cantBitSubG = 6)



rutaFile_15_3 <- "C:/Users/Miguel/Downloads/Compressed/parity1019/15bits_3bits_3nodos.csv"
rutaFilesExport_15_3 <- "C:/Users/Miguel/Downloads/Compressed/parity1019/15b_3b/"
genFullGrafosPDF(rutaFileCSV = rutaFile_15_3,rutaExportFiles = rutaFilesExport_15_3,cantBitG = 15,cantBitSubG = 3)


rutaFile_15_6 <- "C:/Users/Miguel/Downloads/Compressed/parity1019/15bits_6bits_4nodos.csv"
rutaFilesExport_15_6 <- "C:/Users/Miguel/Downloads/Compressed/parity1019/15b_6b/"
genFullGrafosPDF(rutaFileCSV = rutaFile_15_6,rutaExportFiles = rutaFilesExport_15_6,cantBitG = 15,cantBitSubG = 6)


rutaFile_15_10 <- "C:/Users/Miguel/Downloads/Compressed/parity1019/15bits_10bits_5nodos.csv"
rutaFilesExport_15_10 <- "C:/Users/Miguel/Downloads/Compressed/parity1019/15b_10b/"
genFullGrafosPDF(rutaFileCSV = rutaFile_15_10,rutaExportFiles = rutaFilesExport_15_10,cantBitG = 15,cantBitSubG = 10)