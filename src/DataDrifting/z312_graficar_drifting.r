#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


#cargo los datasets que voy a comparar
setwd("D:\\gdrive\\ITBA2022A\\" )  #establezco la carpeta donde voy a trabajar


datasetA  <- fread( "./datasets/paquete_premium_202011.csv" )
datasetB  <- fread( "./datasets/paquete_premium_202101.csv" )

#los campos sobre los que voy a trabajar
campos_buenos <-  setdiff(  colnames( datasetA),  c("numero_de_cliente","foto_mes","clase_ternaria" ) )

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/ST2091/", showWarnings = FALSE )
archivo_salida  <- "./exp/ST2091/DataDrifting_01.pdf"

pdf(archivo_salida)

for( campo in  campos_buenos )
{
  cat( campo, "  " )

  tbl  <- datasetA[ , c("numero_de_cliente", campo),   with=FALSE ]
  tbl[  datasetB, on="numero_de_cliente",  futuro := get(paste0("i.",campo)) ]
  tbl[ , delta :=  futuro - get(campo)  ]

  drift  <- density( tbl[ !is.na(delta), delta ] , kernel="gaussian", na.rm=TRUE)

  qdrift  <- quantile(  tbl[ , delta ] , prob= c(0.05, 0.95), na.rm=TRUE )

  a1  <- qdrift[[1]]
  a2  <- qdrift[[2]]


  plot(drift, 
       col= "blue",
       main= paste0("DELTA    ",  campo),
       xlim= c(a1, a2),
       lwd= 3
      )
 
  abline( v=0, col="darkgreen" )

  legend(  "topright",  
           legend=c("A", "B"),
           col=c("blue", "darkgreen"), lty=c(1,2))

}

dev.off()  #dejo de graficar


