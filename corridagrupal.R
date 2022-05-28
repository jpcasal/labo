Skip to content
Search or jump to…
Pull requests
Issues
Marketplace
Explore

@jpcasal 
jpcasal
/
  itba-mineria-de-datos-labo
Public
forked from itba-ecd/labo
Code
Pull requests
Actions
Projects
Wiki
Security
Insights
itba-mineria-de-datos-labo/src/ranger/433_ranger_BO.r
@jpcasal
jpcasal BO paralel clases
Latest commit 83d65ef 2 minutes ago
History
1 contributor
223 lines (163 sloc)  6.74 KB

# Optimización Bayesiana de hiperparámetros de  ranger  (Random Forest)

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


library("data.table")
library("rlist")
library("yaml")

library("ranger")
library("randomForest") # solo se usa para imputar nulos
library("parallel")

# paquetes necesarios para la Bayesian Optimization
library("DiceKriging")
library("mlrMBO")



kBO_iter <- 100 # cantidad de iteraciones de la Optimización Bayesiana


# Estructura que define los hiperparámetros y sus rangos
hs <- makeParamSet(
  makeIntegerParam("num.trees", lower = 2000L, upper = 4000L), # la letra L al final significa ENTERO
  makeIntegerParam("max.depth", lower = 7L, upper = 20L), # 0 significa profundidad infinita
  makeIntegerParam("min.node.size", lower = 400L, upper = 5000L),
  makeIntegerParam("mtry", lower = 7L, upper = 30L)
)


ksemilla_azar <- 100003 # Aquí poner la propia semilla

#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los títulos

loguear <- function(reg, arch = NA, folder = "./work/", ext = ".txt", verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)
  
  if (!file.exists(archivo)) # Escribo los títulos
  {
    linea <- paste0(
      "fecha\t",
      paste(list.names(reg), collapse = "\t"), "\n"
    )
    
    cat(linea, file = archivo)
  }
  
  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t", # la fecha y hora
    gsub(", ", "\t", toString(reg)), "\n"
  )
  
  cat(linea, file = archivo, append = TRUE) # grabo al archivo
  
  if (verbose) cat(linea) # imprimo por pantalla
}
#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset que consiste en una partición estratificada según agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una partición 70, 30
# particionar( data=dataset, division=c(1,1,1,1,1), agrupa=clase_ternaria, seed=semilla)   divide  en 5 particiones

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  
  bloque <- unlist(mapply(
    function(x, y) {
      rep(y, x)
    },
    division, seq(from = start, length.out = length(division))
  ))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa
  ]
}
#------------------------------------------------------------------------------

ranger_Simple <- function(fold_test, pdata, param) {
  # genero el modelo
  
  set.seed(ksemilla_azar)
  
  modelo <- ranger(
    formula = "clase_binaria ~ .",
    data = pdata[fold != fold_test],
    probability = TRUE, # para que devuelva las probabilidades
    num.trees = param$num.trees,
    mtry = param$mtry,
    min.node.size = param$min.node.size,
    max.depth = param$max.depth
  )
  
  prediccion <- predict(modelo, pdata[fold == fold_test])
  
  ganancia_testing <- pdata[
    fold == fold_test,
    sum((prediccion$predictions[, "POS"] > 1 / 60) *
          ifelse(clase_binaria == "POS", 59000, -1000))
  ]
  
  return(ganancia_testing)
}
#------------------------------------------------------------------------------

ranger_CrossValidation <- function(data, param, pcampos_buenos, qfolds, pagrupa, semilla) {
  divi <- rep(1, qfolds)
  particionar(data, divi, seed = semilla, agrupa = pagrupa)
  
  ganancias <- mcmapply(ranger_Simple,
                        seq(qfolds), # 1 2 3 4 5
                        MoreArgs = list(data, param),
                        SIMPLIFY = FALSE,
                        mc.cores = 1
  ) # dejar esto en  1, porque ranger ya corre en paralelo
  
  data[, fold := NULL] # elimino el campo fold
  
  # devuelvo la ganancia promedio normalizada
  ganancia_promedio <- mean(unlist(ganancias))
  ganancia_promedio_normalizada <- ganancia_promedio * qfolds
  
  return(ganancia_promedio_normalizada)
}
#------------------------------------------------------------------------------
# esta funcion solo puede recibir los parámetros que se están optimizando
# el resto de los parámetros se pasan como variables globales

EstimarGanancia_ranger <- function(x) {
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1
  
  xval_folds <- 5 # 5-fold cross validation
  
  ganancia <- ranger_CrossValidation(dataset,
                                     param = x,
                                     qfolds = xval_folds,
                                     pagrupa = "clase_binaria",
                                     semilla = ksemilla_azar
  )
  
  # logueo
  xx <- x
  xx$xval_folds <- xval_folds
  xx$ganancia <- ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear(xx, arch = klog)
  
  return(ganancia)
}
#------------------------------------------------------------------------------
# Aquí comienza el programa

# Aquí se debe poner la carpeta de la computadora local
setwd("/home/tlichtig/buckets/b1") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread("./datasets/paquete_premium_202011.csv.gz", stringsAsFactors = TRUE) # donde entreno


# creo la carpeta donde va el experimento
# HT  representa  Hyperparameter Tuning
dir.create("./exp/HT4330", showWarnings = FALSE)
setwd("/home/tlichtig/buckets/b1/exp/HT4330") # Establezco el Working Directory DEL EXPERIMENTO

# en estos archivos quedan los resultados
kbayesiana <- "HT433-alt.RDATA"
klog <- "HT433-alt.txt"


GLOBAL_iteracion <- 0 # inicializo la variable global

# si ya existe el archivo log, traigo hasta donde llegue
if (file.exists(klog)) {
  tabla_log <- fread(klog)
  GLOBAL_iteracion <- nrow(tabla_log)
}



# paso a trabajar con clase binaria POS={BAJA+2}   NEG={BAJA+1, CONTINUA}
dataset[, clase_binaria := as.factor(ifelse(clase_ternaria == "BAJA+2", "POS", "NEG"))]
dataset[, clase_ternaria := NULL] # elimino la clase_ternaria, ya no la necesito


# imputo los nulos, ya que ranger no acepta nulos
# Leo Breiman, ¿por que le temías a los nulos?
dataset <- na.roughfix(dataset)



# Aquí comienza la configuración de la Bayesian Optimization

configureMlr(show.learner.output = FALSE)

funcion_optimizar <- EstimarGanancia_ranger

# configuro la búsqueda bayesiana,  los hiperparámetros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar,
  minimize = FALSE, # estoy Maximizando la ganancia
  noisy = TRUE,
  par.set = hs,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl(save.on.disk.at.time = 600, save.file.path = kbayesiana)
ctrl <- setMBOControlTermination(ctrl, iters = kBO_iter)
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

surr.km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = TRUE))

# inicio la optimización bayesiana
if (!file.exists(kbayesiana)) {
  run <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  run <- mboContinue(kbayesiana)
} # retomo en caso que ya exista
© 2022 GitHub, Inc.
Terms
Privacy
Security
Status
Docs
Contact GitHub
Pricing
API
Training
Blog
About
