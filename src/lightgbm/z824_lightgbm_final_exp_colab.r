# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml

PARAM <- list()
PARAM$experimento <- "KA8240"
# setwd("/home/aleb/dmeyf23/datasets")
PARAM$input$dataset <- "./datasets/competencia_03_exp_col.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(202102, 202103, 202104, 202105)
PARAM$input$future <- c(202107) # meses donde se aplica el modelo

PARAM$finalmodel$semilla <- 135977

# hiperparametros intencionalmente NO optimos
PARAM$finalmodel$optim$num_iterations <- 100
PARAM$finalmodel$optim$learning_rate <- 0.5
PARAM$finalmodel$optim$feature_fraction <- 0.4
PARAM$finalmodel$optim$min_data_in_leaf <- 5000
PARAM$finalmodel$optim$num_leaves <- 40


# Hiperparametros FIJOS de  lightgbm
PARAM$finalmodel$lgb_basicos <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO

  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0

  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

  extra_trees = FALSE # Magic Sauce
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


# Catastrophe Analysis  -------------------------------------------------------
# deben ir cosas de este estilo
#   dataset[foto_mes == 202006, active_quarter := NA]

# Data Drifting
# por ahora, no hago nada


# Feature Engineering Historico  ----------------------------------------------
#   aqui deben calcularse los  lags y  lag_delta
#   Sin lags no hay paraiso ! corta la bocha
#   https://rdrr.io/cran/data.table/man/shift.html


#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dapply <- dataset[foto_mes %in% PARAM$input$future]
dataset <- dataset[foto_mes %in% PARAM$input$training]
gc() # garbage collection




# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[, campos_buenos, with = FALSE]),
  label = dataset[, clase01]
)
gc() # garbage collection

# genero el modelo
param_completo <- c(
  PARAM$finalmodel$lgb_basicos,
  PARAM$finalmodel$optim
)

# create an empty list to store the models
models <- list()

seeds <- c(2, 17, 31, 53, 73, 97)

# define a function to build a model with a given seed
build_model <- function(seed) {
  modelo <- lgb.train(
    data = dtrain,
    param = c(param_completo, seed = seed)
  )
  print(paste0("Modelo con semilla ", seed, " terminado"))
  gc()
  return(modelo)
}

# use lapply to build a list of models with different seeds
models <- lapply(seeds, build_model)

# aplico el modelo a los datos sin clase


# create an empty list to store the predictions
predictions <- list()

for (i in seq_along(models)) {
  pred <- predict(
    models[[i]],
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  predictions <- c(predictions, list(pred))
}

# take the mean of the predictions across all models
row_means <- rowMeans(do.call(cbind, predictions))
row_means[1:20]
predictions <- c(predictions, list(row_means))

for (i in seq_along(predictions)) {
  print(predictions[[i]][1])
}
# genero la tabla de entrega
tb_entrega <- dapply[, list(numero_de_cliente, foto_mes, clase_ternaria)]
tb_entrega[, prob := prediccion]

for (p in predictions) {
  tb_entrega <- dapply[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  tb_entrega[, prob := p]
  setorder(tb_entrega, -prob)
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:15000, Predicted := 1L]
  score <- sum(tb_entrega$Predicted * ifelse(tb_entrega$clase_ternaria == "BAJA+2", 273000, -7000))
  print(score)
}





# tb_entrega[, Predicted := ifelse(prob > 0.025, 1L, 0L)]
cortes <- seq(000, 10000, by = 1)
scores <- c()
for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]
  score <- sum(tb_entrega$Predicted * ifelse(tb_entrega$clase_ternaria == "BAJA+2", 273000, -7000))
  print(score)
  scores <- c(scores, score)
}
plot(cortes, scores, type = "l", xlab = "Corte", ylab = "Score")
fwrite(tb_entrega[, list(numero_de_cliente, clase_ternaria)],
  file = paste0(PARAM$experimento, "_", sum(tb_entrega$Predicted), ".csv"),
  sep = ","
)

sum(dataset$foto_mes == PARAM$input$future)

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
