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
PARAM$experimento <- "Exp_col"
# setwd("/home/aleb/dmeyf23/datasets")
PARAM$input$dataset0 <- "./datasets/competencia_03_exp_col.csv.gz"
PARAM$input$dataset1 <- "./datasets/competencia_03_exp_col_lags_1.csv.gz"
PARAM$input$dataset2 <- "./datasets/competencia_03_exp_col_lags_2.csv.gz"

setwd("~/buckets/b1")
dataset0 <- fread(PARAM$input$dataset0, stringsAsFactors = TRUE)
dataset1 <- fread(PARAM$input$dataset1, stringsAsFactors = TRUE)
dataset2 <- fread(PARAM$input$dataset2, stringsAsFactors = TRUE)

dataset <- cbind(dataset0, dataset1, dataset2)

# Obtener los nombres de las columnas del dataset
nombres_columnas <- colnames(dataset)

lista_ct_0 <- nombres_columnas[grep("^ct_0_|^lag_ct_0_", nombres_columnas)]
lista_ct_it <- nombres_columnas[grep("^ct_it_|^lag_ct_it_", nombres_columnas)]
lista_ct_median_6m <- nombres_columnas[grep("^ct_median_6m_|^lag_ct_median_6m_", nombres_columnas)]
lista_ct_mean_6m <- nombres_columnas[grep("^ct_mean_6m|^lag_ct_mean_6m", nombres_columnas)]
lista_ct_median <- nombres_columnas[grep("^ct_median|^lag_ct_median", nombres_columnas)]
lista_ct_mean <- nombres_columnas[grep("^ct_mean|^lag_ct_mean", nombres_columnas)]


# meses donde se entrena el modelo
PARAM$input$training <- c(201905, 201908, 201910, 202006, 202012, 202106)
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


dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------
nombres_columnas <- colnames(dataset)


columnas_ct_lag <- nombres_columnas[grep("^ct_|^lag_ct", nombres_columnas)]
columnas_no_ct_lag <- setdiff(nombres_columnas, columnas_ct_lag)

lista_no_train <- c("clase_ternaria", "clase01", "(mes + 1)", "(mes + 2)", "foto_mes", "mes")

campos_genericos <- setdiff(columnas_no_ct_lag, lista_no_train)

# Filtrar las columnas que comienzan con "ct_0"
columnas_ct_0 <- nombres_columnas[grep("^ct_0_", nombres_columnas)]
columnas_sin_prefijo <- sub("^ct_0_", "", columnas_ct_0)
columnas_ct_0 <- nombres_columnas[grep("^ct_0_", nombres_columnas)]



dapply <- dataset[foto_mes %in% PARAM$input$future]
dataset <- dataset[foto_mes %in% PARAM$input$training]
gc() # garbage collection
# Iniciar el ciclo para recorrer diferentes valores de i
resultados <- data.table()

datasets <- c("lista_ct_0", "lista_ct_it", "lista_ct_median_6m", "lista_ct_mean_6m", "lista_ct_median", "lista_ct_mean")
setwd(paste0("./exp/", PARAM$experimento, "/"))
for (j in datasets) {
  columnas_asignadas <- get(j)
  print(paste0("dataset ", j, " con ", length(columnas_asignadas), " columnas y ", nrow(dataset), " filas"))
  campos_buenos <- Reduce(union, c(
    columnas_asignadas,
    campos_genericos
  ))

  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[, campos_buenos, with = FALSE]),
    label = dataset[, clase01]
  )
  gc()

  param_completo <- c(
    PARAM$finalmodel$lgb_basicos,
    PARAM$finalmodel$optim
  )

  # create an empty list to store the models
  models <- list()

  seeds <- c(2, 104147, 41947, 59809, 70507, 82073, 15649, 28403)

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

  predictions <- list()
  for (i in seq_along(models)) {
    pred <- predict(
      models[[i]],
      data.matrix(dapply[, campos_buenos, with = FALSE])
    )

    print(paste0("Modelo con semilla ", i, " terminado"))
    predictions <- c(predictions, list(pred))
  }

  # take the mean of the predictions across all models
  row_means <- rowMeans(do.call(cbind, predictions))
  predictions <- c(predictions, list(row_means))


  # genero la tabla de entrega
  scores <- c()
  tb_entrega <- dapply[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  for (i in seq_along(predictions)) {
    col_name <- paste0("pred_", i)
    # tb_entrega[, (col_name) := predictions[[i]]]
    tb_entrega[, (col_name) := ifelse(predictions[[i]] > 0.025, 1L, 0L)]
    score <- sum(tb_entrega[, (col_name), with = FALSE] * ifelse(tb_entrega$clase_ternaria == "BAJA+2", 273000L, -7000L))
    # score <- tb_entrega[, sum(.SD * ifelse(clase_ternaria == "BAJA+2", 273000, -7000)), .SDcols = patterns("^pred_")]
    scores <- c(scores, score)
    print(paste("dataset ", j, ": ", col_name, " - score: ", score))
  }
  fwrite(tb_entrega, paste0("tb_entrega_", j, ".csv.gz"), row.names = FALSE, compression = "gzip")
}
