require("rlist")
require("yaml")
require("data.table")
require("mlflow")


options(show.error.locations = TRUE)


options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})


#------------------------------------------------------------------------------
#inicializo el ambiente de mlflow

exp_mlflow_iniciar  <- function()
{
  #leo uri, usuario y password
  MLFLOW  <<- read_yaml( "/media/expshared/mlflow.yml" )

  Sys.setenv( MLFLOW_TRACKING_USERNAME= MLFLOW$tracking_username )
  Sys.setenv( MLFLOW_TRACKING_PASSWORD= MLFLOW$tracking_password )
  mlflow_set_tracking_uri( MLFLOW$tracking_uri )

  Sys.setenv(MLFLOW_BIN= Sys.which("mlflow") )
  Sys.setenv(MLFLOW_PYTHON_BIN= Sys.which("python3") )
  Sys.setenv(MLFLOW_TRACKING_URI= MLFLOW$tracking_uri, intern= TRUE )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

exp_mlflow_iniciar()


res  <- read_yaml( "run.yml" )

mlflow_log_param( run_id= res$run_uuid, 
                  key= "SH_END", 
                  value= format(Sys.time(), "%Y%m%d %H%M%S") )

#finalizo el experimento
mlflow_end_run( run_id= res$run_uuid )

quit( save= "no" )
