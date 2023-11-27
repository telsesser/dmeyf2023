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

PARAM$input$dataset <- "./datasets/competencia_03_process_2.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(
  201901, 201902, 201903, 201904, 201905, 201906,
  201907, 201908, 201909, 201910, 201911, 201912, 202001, 202008,
  202009, 202010, 202011, 202012, 202101, 202102, 202103, 202104,
  202105, 202106
)
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

  extra_trees = TRUE # Magic Sauce
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dapply <- dataset[foto_mes %in% PARAM$input$future]
dataset <- dataset[foto_mes %in% PARAM$input$training]

dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]
#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[, campos_buenos, with = FALSE]),
  label = dataset[, clase01]
)


# genero el modelo
param_completo <- c(
  PARAM$finalmodel$lgb_basicos,
  PARAM$finalmodel$optim
)
models <- list()
seeds <- c(2, 104147, 41947) # , 59809, 70507, 82073, 15649, 28403, 14879, 13523)
lista_ganancias_semillas <- list()
ganancias_semilla <- data.table()

build_model <- function(seed) {
  modelo <- lgb.train(
    data = dtrain,
    param = c(param_completo, seed = seed)
  )
  print(paste0("Modelo con semilla ", seed, " terminado"))
  gc()
  return(modelo)
}

models <- lapply(seeds, build_model)

predictions <- list()
for (i in seq_along(models)) {
  pred <- predict(
    models[[i]],
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )

  print(paste0("Modelo con semilla ", i, " terminado"))
  predictions <- c(predictions, list(pred))
}
row_means <- rowMeans(do.call(cbind, predictions))
predictions <- c(predictions, list(row_means))


# genero la tabla de entrega
tb_entrega <- dapply[, list(numero_de_cliente, foto_mes, clase_ternaria)]

# agrego las predicciones de cada modelo
for (i in seq_along(predictions)) {
  tb_entrega[, paste0("pred_", i) := predictions[[i]]]
}

# grabo las probabilidad del modelo
fwrite(tb_entrega,
  file = paste0(PARAM$experimento, "_predicciones", ".csv"),
  sep = ","
)
