# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("dplyr")

# Poner la carpeta de la materia de SU computadora local
setwd("/home/santiago/Documents/Maestría/EyF/")
# Poner sus semillas
semillas <- c(66607,66637,66647,66667,66697)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

# Nos quedamos solo con el 202101
dapply <-  dataset[foto_mes == 202103]
dataset <- dataset[foto_mes == 202101]

# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  clase_ternaria == "CONTINUA",
  "noevento",
  "evento"
)]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]
dapply[, clase_ternaria := NULL]

set.seed(semillas[1])


################ FEATURE ENGINEERING  #####################
###### Features que se pueden calcular en train y en test de forma conjunta
# Deberían ser aquellos que no usan valores globales o de otras filas.######

sufijos_visa_master <- c(  "_delinquency",  "_mfinanciacion_limite",
                           "_msaldototal",  "_msaldopesos",  "_msaldodolares",
                           "_mconsumospesos",  "_mconsumosdolares","_mlimitecompra",
                           "_madelantopesos",  "_madelantodolares")

# Creo los campos suma de visa y master
for (suf in sufijos_visa_master){
  n_visa = paste0("Visa",suf)
  n_master = paste0("Master",suf)
  n_nuevo = paste0("Visa_plus_Master",suf)
  dataset <- dataset[,(n_nuevo) := ifelse(is.na(get(n_visa)),0,get(n_visa)) + 
                       ifelse(is.na(get(n_master)),0,get(n_master))]
  
  dapply <- dapply[,(n_nuevo) := ifelse(is.na(get(n_visa)),0,get(n_visa)) + 
                     ifelse(is.na(get(n_master)),0,get(n_master))]
}

# Hay otros campos que no son sufijos, pero también son sumables.
dataset[,c_tarjeta_visa_master := ctarjeta_visa+ctarjeta_master]
dataset[,c_tarjeta_visa_master_transacciones := ctarjeta_visa_transacciones+ctarjeta_master_transacciones]
dataset[,m_tarjeta_visa_master_consumo := mtarjeta_visa_consumo+mtarjeta_master_consumo]

dapply[,c_tarjeta_visa_master := ctarjeta_visa+ctarjeta_master]
dapply[,c_tarjeta_visa_master_transacciones := ctarjeta_visa_transacciones+ctarjeta_master_transacciones]
dapply[,m_tarjeta_visa_master_consumo := mtarjeta_visa_consumo+mtarjeta_master_consumo]


# PROPUESTOS POR GUSTAVO 

dataset[ , campo1 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales <2 ) ]
dataset[ , campo2 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales>=2 ) ]
dataset[ , campo3 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro <2601.1 ) ]
dataset[ , campo4 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro>=2601.1 ) ]
dataset[ , campo5 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status>=8 | is.na(Master_status) ) ) ]
dataset[ , campo6 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status <8 & !is.na(Master_status) ) ) ]
dataset[ , campo7 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter <38 ) ]
dataset[ , campo8 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter>=38 ) ]

dapply[ , campo1 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales <2 ) ]
dapply[ , campo2 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales>=2 ) ]
dapply[ , campo3 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro <2601.1 ) ]
dapply[ , campo4 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro>=2601.1 ) ]
dapply[ , campo5 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status>=8 | is.na(Master_status) ) ) ]
dapply[ , campo6 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status <8 & !is.na(Master_status) ) ) ]
dapply[ , campo7 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter <38 ) ]
dapply[ , campo8 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter>=38 ) ]

# Creo campos consumo / limite como medida de actividad
dataset[,"Visa_mconsumospesos_sobre_mlimitecompra":=Visa_mconsumospesos / Visa_mlimitecompra]
dataset[,"Master_mconsumospesos_sobre_mlimitecompra":=Master_mconsumospesos / Master_mlimitecompra]

dapply[,"Visa_mconsumospesos_sobre_mlimitecompra":=Visa_mconsumospesos / Visa_mlimitecompra]
dapply[,"Master_mconsumospesos_sobre_mlimitecompra":=Master_mconsumospesos / Master_mlimitecompra]


# antiguedad sobre edad (proporción de su vida como cliente de banco)
dataset[,"antiguedad_proporcion_edad":=(cliente_antiguedad/12) / cliente_edad]
dapply[,"antiguedad_proporcion_edad":=(cliente_antiguedad/12) / cliente_edad]

# Rankeo variables.
prefix <- "r_"

quantiles <- list("cliente_edad"=10,
                  "cliente_antiguedad"=4,
                  "mpayroll"=10)

for (var in names(quantiles)) {
  dataset[, (paste(prefix, var,quantiles[[var]], sep = "")) := ntile(get(var), quantiles[[var]])]
  dapply[, (paste(prefix, var,quantiles[[var]], sep = "")) := ntile(get(var), quantiles[[var]])]
  
}


############################################################################
###### Features que NO pueden calcular en train y en test de forma conjunta ########
# Particionamos de forma estratificada
# ##################### OPTIMIZACION BAYESIANA ##################################
# # funcion ganancia
# ganancia <- function(probabilidades, clase) {
#   return(sum(
#     (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000))
#   )
# }
# 
# # Armamos una función para modelar con el fin de simplificar el código futuro
# modelo_rpart <- function(train, test, cp =  0, ms = 20, mb = 1, md = 10) {
#   modelo <- rpart(clase_binaria ~ ., data = train,
#                   xval = 0,
#                   cp = cp,
#                   minsplit = ms,
#                   minbucket = mb,
#                   maxdepth = md)
#   
#   test_prediccion <- predict(modelo, test, type = "prob")
#   ganancia(test_prediccion[, "evento"], test$clase_binaria) / 0.3
#   
# }
# 
# # Una función auxiliar para los experimentos
# experimento_rpart <- function(ds, semillas, cp = 0, ms = 20, mb = 1, md = 10) {
#   gan <- c()
#   for (s in semillas) {
#     set.seed(s)
#     in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70,
#                                               list = FALSE)
#     train  <-  ds[in_training, ]
#     test   <-  ds[-in_training, ]
#     #train_sample <- tomar_muestra(train)
#     r <- modelo_rpart(train, test, 
#                       cp = cp, ms = ms, mb = mb, md = md)
#     gan <- c(gan, r)
#   }
#   mean(gan)
# }
# 
# obj_fun_md_ms <- function(x) {
#   experimento_rpart(dataset, semillas
#                     , md = x$maxdepth
#                     , ms = x$minsplit
#                     , mb = floor(x$minbucket*x$minsplit))
# }
# 
# obj_fun <- makeSingleObjectiveFunction(
#   minimize = FALSE,
#   fn = obj_fun_md_ms,
#   par.set = makeParamSet(
#     makeIntegerParam("maxdepth",  lower = 4L, upper = 30L),
#     makeIntegerParam("minsplit",  lower = 1L, upper = 200L),
#     makeNumericParam("minbucket",  lower = 0L, upper = 1L)),
#   has.simple.signature = FALSE
# )
# 
# ctrl <- makeMBOControl()
# ctrl <- setMBOControlTermination(ctrl, iters = 100L)
# ctrl <- setMBOControlInfill(
#   ctrl,
#   crit = makeMBOInfillCritEI(),
#   opt = "focussearch",
#   opt.focussearch.points = 20
# )
# 
# lrn <- makeMBOLearner(ctrl, obj_fun)
# 
# surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
# 
# run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )
# print(run_md_ms)

#Recommended parameters:
#maxdepth=11; minsplit=10; minbucket=0.284
# Particionamos de forma estratificada


################## MODELO ################################
#agrego 30% de canaritos
for( i in 1:floor(0.3*length(dataset)) ){
  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]
  dapply[ , paste0("canarito", i ) :=  runif( nrow(dapply)) ]
} 

dtrain <- dataset[ foto_mes==202101 ]
#dapply <- dataset[ foto_mes==202103 ]

#Primero  veo como quedan mis arboles
modelo_original <- rpart(
  formula= "clase_binaria ~ . -mcomisiones_mantenimiento -Visa_mpagado",
  data= dtrain,
  model= TRUE,
  xval= 0,
  cp= -1,
  minsplit= 2, # dejo que crezca y corte todo lo que quiera
  minbucket= 1,
  maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )

prediccion  <- predict( modelo_pruned, dapply, type = "prob")[,"evento"]

for (corte in c(0.01,0.025,0.03,0.05,0.075,0.09, 0.1)){
  entrega  <-  as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente,
                                    "Predicted"= as.integer(  prediccion > corte ) ) )
  fwrite( entrega, paste0( "./canaritos_binaria_",corte,".csv"), sep="," )
}



#pdf(file = "./stopping_at_canaritos.pdf", width=28, height=4)
#prp(modelo_pruned, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
#dev.off()

