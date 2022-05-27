rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")
require("parallel")

ksemillas  <- c(104233, 102523, 102451, 101573, 101531) #reemplazar por las propias semillas

#Escribo los titulos al archivo donde van a quedar los resultados
#atencion que si ya existe el archivo, esta instruccion LO SOBREESCRIBE, y lo que estaba antes se pierde
#la forma que no suceda lo anterior es con append=TRUE
#cat( file="C:\\Users\\juanp\\OneDrive\\Escritorio\\ECD\\MD\\TareasHogar\\salida24H_a",
#     sep= "",
 #    "max_depth", "\t",
  #   "min_split", "\t",
   #  "cp","\t",
    # "minbucket","\t",
     #"ganancia_promedio", "\n")

#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
          by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia  <- function( semilla, param_basicos )
{
  #particiono estratificadamente el dataset
  particionar( dataset, division=c(70,30), agrupa="clase_ternaria", seed= semilla )  #Cambiar por la primer semilla de cada uno !

  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .",     #quiero predecir clase_ternaria a partir del resto
                   data= dataset[ fold==1],  #fold==1  es training,  el 70% de los datos
                   xval= 0,
                   control= param_basicos )  #aqui van los parametros del arbol

  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo,   #el modelo que genere recien
                          dataset[ fold==2],  #fold==2  es testing, el 30% de los datos
                          type= "prob") #type= "prob"  es que devuelva la probabilidad

  #prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  #cada columna es el vector de probabilidades 


  #calculo la ganancia en testing  qu es fold==2
  ganancia_test  <- dataset[ fold==2, 
                             sum( ifelse( prediccion[, "BAJA+2"]  >  1/60,
                                         ifelse( clase_ternaria=="BAJA+2", 59000, -1000 ),
                                         0 ) )]

  #escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada  <-  ganancia_test / 0.3

  return( ganancia_test_normalizada )
}
#------------------------------------------------------------------------------

#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Users\\juanp\\OneDrive\\Escritorio\\ECD\\MD")   #Establezco el Working Directory
#cargo los datos

dataset  <- fread("./datasets/paquete_premium_202011.csv")

                    for( vmax_depth  in  c(7)  )
                    {
                      for( minbucket  in  seq(380,440,10)  )
                        {
                        for( vmin_split  in  seq(1,600,30) )
                        {
                          for( cp  in  c( -0.5)  )
                            {
                    param_basicos  <- list( "cp"=         cp,  #complejidad minima
                                            "minsplit"=  vmin_split+2*minbucket,  #minima cantidad de registros en un nodo para hacer el split
                                            "minbucket"=  minbucket,  #minima cantidad de registros en una hoja
                                            "maxdepth"=  vmax_depth ) #profundidad máxima del arbol
                    
                    #Un solo llamado, con la semilla 17- Lo quito para la prueba de 24 hs
                    #ArbolEstimarGanancia( 17, param_basicos )   
                    
                    
                    #la funcion mcmapply  llama a la funcion ArbolEstimarGanancia  tantas veces como valores tenga el vector  ksemillas
                    ganancias  <- mcmapply( ArbolEstimarGanancia, 
                                            ksemillas,   #paso el vector de semillas, que debe ser el primer parametro de la funcion ArbolEstimarGanancia
                                            MoreArgs= list( param_basicos),  #aqui paso el segundo parametro
                                            SIMPLIFY= FALSE,
                                            mc.cores= 1 )  #se puede subir a 5 si posee Linux o Mac OS
                    
                    #muestro la lista de las ganancias en testing para la particion realizada con cada semilla -saco para prueba 24 hs
                    #ganancias
                    
                    #paso la lista a vector-saco para pruega 24hs
                    #unlist(ganancias)
                    
                    #finalmente calculo la media (promedio)  de las ganancias
                    ganancia_promedio<-mean( unlist(ganancias) )
                    
                        
                        #escribo los resultados al archivo de salida
                        cat(  file="C:\\Users\\juanp\\OneDrive\\Escritorio\\ECD\\MD\\TareasHogar\\salida24H_a",
                              append= TRUE,
                              sep= "",
                              vmax_depth, "\t",
                              vmin_split+2*minbucket, "\t",
                              cp, "\t",
                              minbucket, "\t",
                              ganancia_promedio, "\n"  )
                    
                      }
                      }
                      }
                    }
