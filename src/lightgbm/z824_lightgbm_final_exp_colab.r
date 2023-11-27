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
<<<<<<< HEAD

=======
PARAM$experimento <- "Exp_col"
>>>>>>> d528dd844a6c3e6986851022ca26b9f3e0497a43
# setwd("/home/aleb/dmeyf23/datasets")
PARAM$input$dataset0 <- "./datasets/competencia_03_exp_col.csv.gz"
PARAM$input$dataset1 <- "./datasets/competencia_03_exp_col_lags_1.csv.gz"
PARAM$input$dataset2 <- "./datasets/competencia_03_exp_col_lags_2.csv.gz"
<<<<<<< HEAD
PARAM$input$dataset3 <- "./datasets/competencia_03_exp_col_4.csv.gz"


dataset0 <- fread(PARAM$input$dataset0, stringsAsFactors = TRUE)
dataset1 <- fread(PARAM$input$dataset1, stringsAsFactors = TRUE)
dataset2 <- fread(PARAM$input$dataset2, stringsAsFactors = TRUE)
datasetnulls <- fread(PARAM$input$dataset3, stringsAsFactors = TRUE)



dataset <- cbind(dataset0, dataset1, dataset2, datasetnulls)
setwd("~/buckets/b1")
# Obtener los nombres de las columnas del dataset
PARAM$experimento <- "exp_col_2_total"
nombres_columnas <- colnames(dataset)

lista_ct_0 <- nombres_columnas[grep("^ct_0_|^lag_ct_0_", nombres_columnas)]
lista_ct_it <- nombres_columnas[grep("^ct_it_|^lag_ct_it_", nombres_columnas)]
lista_ct_median_6m <- nombres_columnas[grep("^ct_median_6m_|^lag_ct_median_6m_", nombres_columnas)]
lista_ct_mean_6m <- nombres_columnas[grep("^ct_mean_6m|^lag_ct_mean_6m", nombres_columnas)]
lista_ct_median <- nombres_columnas[grep("^ct_median|^lag_ct_median", nombres_columnas)]
lista_ct_mean <- nombres_columnas[grep("^ct_mean|^lag_ct_mean", nombres_columnas)]
lista_ct_nulls <- nombres_columnas[grep("^ct_null_|^lag_ct_null_", nombres_columnas)]


a_sacar <- c(
  "ct_null_mrentabilidad",
  "ct_null_mrentabilidad_annual",
  "ct_null_mcomisiones",
  "ct_null_mactivos_margen",
  "ct_null_mpasivos_margen",
  "ct_null_ctarjeta_visa_debitos_automaticos",
  "ct_null_mttarjeta_visa_debitos_automaticos",
  "ct_null_ccomisiones_otras",
  "ct_null_mcomisiones_otras",
  "ct_null_chomebanking_transacciones", "lag_ct_null_mrentabilidad_6", "lag_ct_null_mrentabilidad_12", "lag_ct_null_mrentabilidad_3",
  "lag_ct_null_mrentabilidad_annual_6", "lag_ct_null_mrentabilidad_annual_12", "lag_ct_null_mrentabilidad_annual_3",
  "lag_ct_null_mcomisiones_6", "lag_ct_null_mcomisiones_12", "lag_ct_null_mcomisiones_3",
  "lag_ct_null_mactivos_margen_6", "lag_ct_null_mactivos_margen_12", "lag_ct_null_mactivos_margen_3",
  "lag_ct_null_mpasivos_margen_6", "lag_ct_null_mpasivos_margen_12", "lag_ct_null_mpasivos_margen_3",
  "lag_ct_null_ctarjeta_visa_debitos_automaticos_6", "lag_ct_null_ctarjeta_visa_debitos_automaticos_12", "lag_ct_null_ctarjeta_visa_debitos_automaticos_3",
  "lag_ct_null_mttarjeta_visa_debitos_automaticos_6", "lag_ct_null_mttarjeta_visa_debitos_automaticos_12", "lag_ct_null_mttarjeta_visa_debitos_automaticos_3",
  "lag_ct_null_ccomisiones_otras_6", "lag_ct_null_ccomisiones_otras_12", "lag_ct_null_ccomisiones_otras_3",
  "lag_ct_null_mcomisiones_otras_6", "lag_ct_null_mcomisiones_otras_12", "lag_ct_null_mcomisiones_otras_3",
  "lag_ct_null_chomebanking_transacciones_6", "lag_ct_null_chomebanking_transacciones_12", "lag_ct_null_chomebanking_transacciones_3"
)

a_poner <- c(
  "ct_it_mrentabilidad",
  "ct_it_mrentabilidad_annual",
  "ct_it_mcomisiones",
  "ct_it_mactivos_margen",
  "ct_it_mpasivos_margen",
  "ct_it_ctarjeta_visa_debitos_automaticos",
  "ct_it_mttarjeta_visa_debitos_automaticos",
  "ct_it_ccomisiones_otras",
  "ct_it_chomebanking_transacciones", "lag_ct_it_mrentabilidad_6", "lag_ct_it_mrentabilidad_12", "lag_ct_it_mrentabilidad_3",
  "lag_ct_it_mrentabilidad_annual_6", "lag_ct_it_mrentabilidad_annual_12", "lag_ct_it_mrentabilidad_annual_3",
  "lag_ct_it_mcomisiones_6", "lag_ct_it_mcomisiones_12", "lag_ct_it_mcomisiones_3",
  "lag_ct_it_mactivos_margen_6", "lag_ct_it_mactivos_margen_12", "lag_ct_it_mactivos_margen_3",
  "lag_ct_it_mpasivos_margen_6", "lag_ct_it_mpasivos_margen_12", "lag_ct_it_mpasivos_margen_3",
  "lag_ct_it_ctarjeta_visa_debitos_automaticos_6", "lag_ct_it_ctarjeta_visa_debitos_automaticos_12", "lag_ct_it_ctarjeta_visa_debitos_automaticos_3",
  "lag_ct_it_mttarjeta_visa_debitos_automaticos_6", "lag_ct_it_mttarjeta_visa_debitos_automaticos_12", "lag_ct_it_mttarjeta_visa_debitos_automaticos_3",
  "lag_ct_it_ccomisiones_otras_6", "lag_ct_it_ccomisiones_otras_12", "lag_ct_it_ccomisiones_otras_3",
  "lag_ct_it_mcomisiones_otras_6", "lag_ct_it_mcomisiones_otras_12", "lag_ct_it_mcomisiones_otras_3",
  "lag_ct_it_chomebanking_transacciones_6", "lag_ct_it_chomebanking_transacciones_12", "lag_ct_it_chomebanking_transacciones_3"
)

lista_ct_arbitraria <- setdiff(lista_ct_nulls, a_sacar)
lista_ct_arbitraria <- Reduce(union, c(lista_ct_arbitraria, a_poner))
print(lista_ct_arbitraria)
=======

setwd("~/buckets/b1")
dataset0 <- fread(PARAM$input$dataset0, stringsAsFactors = TRUE)
dataset1 <- fread(PARAM$input$dataset1, stringsAsFactors = TRUE)
dataset2 <- fread(PARAM$input$dataset2, stringsAsFactors = TRUE)

dataset <- cbind(dataset0, dataset1, dataset2)
>>>>>>> d528dd844a6c3e6986851022ca26b9f3e0497a43

# meses donde se entrena el modelo
PARAM$input$training <- c(
  201901,
  201902,
  201903,
  201904,
  201905,
  201906,
  201907,
  201908,
  201909,
  201910,
  201911,
  201912,
  202001,
  202007,
  202008,
  202009,
  202010,
  202011,
  202012,
  202101,
  202102,
  202103,
  202104,
  202105,
  202106
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

  extra_trees = FALSE # Magic Sauce
)


<<<<<<< HEAD
=======
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

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
>>>>>>> d528dd844a6c3e6986851022ca26b9f3e0497a43
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

datasets <- c("lista_ct_nulls", "lista_ct_0", "lista_ct_it", "lista_ct_median_6m", "lista_ct_mean_6m", "lista_ct_arbitraria")
setwd("~/buckets/b1")
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
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
  lista_ganancias_semillas <- list()
  ganancias_semilla <- data.table()

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
  # scores <- c()
  # tb_entrega <- dapply[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  for (i in seq_along(predictions)) {
    # col_name <- paste0("pred_", i)
    # tb_entrega[, (col_name) := predictions[[i]]]
    # tb_entrega[, (col_name) := ifelse(predictions[[i]] > 0.025, 1L, 0L)]
    # score <- sum(tb_entrega[, (col_name), with = FALSE] * ifelse(tb_entrega$clase_ternaria == "BAJA+2", 273000L, -7000L))
    # score <- tb_entrega[, sum(.SD * ifelse(clase_ternaria == "BAJA+2", 273000, -7000)), .SDcols = patterns("^pred_")]
    # scores <- c(scores, score)
    # print(paste("dataset ", j, ": ", col_name, " - score: ", score))

    tbl <- dapply[, list(numero_de_cliente, foto_mes, clase_ternaria)]
    tbl[, "gan" := ifelse(clase_ternaria == "BAJA+2",
      273000L,
      -7000L
    )]
    tbl[, prob := predictions[[i]]] # le agregamos 1 columna con las predicciones
    setorder(tbl, -prob)

    tbl[, gan_acum := cumsum(gan)] # obtenemos la ganacia acumulada
    cortes <- seq(8000, 16000, by = 10) # generamos cortes de 10 en 10 desde 8mil a 16mil
    tbl <- tbl[cortes] # nos quedamos con la ganancia acumulada en esos cortes
    tbl <- tbl[, `:=`(Envios = cortes, Modelo = i)]

    ganancias_semilla <- rbindlist(list(ganancias_semilla, tbl[, .(Modelo, Envios, gan_acum)]))
  }
  fwrite(ganancias_semilla,
    file = paste0("tb_entrega_", j, "_GananciasSemillas.csv")
  )
}
