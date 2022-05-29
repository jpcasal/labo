#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


#Aqui comienza el programa
setwd("D:\\gdrive\\ITBA2022A\\")

datasetA  <- fread( "./datasets/paquete_premium_202011.csv" )
datasetB  <- fread( "./datasets/paquete_premium_202101.csv" )


campos_buenos  <- setdiff(  colnames( datasetA),  c("numero_de_cliente","foto_mes","clase_ternaria" ) )


#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/ST2351/", showWarnings = FALSE )
archivo_salida  <- "./labo/exp/ST2351/drifting_001.pdf"

pdf(archivo_salida)

for( campo in  campos_buenos )
{
  cat( campo, "  " )
  distA  <- quantile(  datasetA[ , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  distB  <- quantile(  datasetB[ , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )

  a1  <- pmin( distA[[1]], distB[[1]] )
  a2  <- pmax( distA[[2]], distB[[2]] )

  densidadA  <- density( datasetA[ , get(campo) ] , kernel="gaussian", na.rm=TRUE)
  densidadB  <- density( datasetB[ , get(campo) ] , kernel="gaussian", na.rm=TRUE)

  plot(densidadA, 
       col="blue",
       xlim= c( a1, a2),
       main= paste0("Densidades    ",  campo), )

  lines(densidadB, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("202011", "202101"),
           col=c("blue", "red"), lty=c(1,2))

}
dev.off()
