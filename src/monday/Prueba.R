##
## Sobre el Azar
##
## ---------------------------
## Step 1: El simple y viejo Train / Test
## ---------------------------
##
## If you torture the data long enough, it will confess.
## --- Ronald Coase
##

# nolint: indentation_linter.

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ROCR")
require("ggplot2")
require("foreach")
require("doParallel")

registerDoParallel(7)

# Poner la carpeta de la materia de SU computadora local
setwd("/home/tomi/Escritorio/Maestria/2 - DMEyF/")

# Poner sus semillas
semillas <- c(135977, 209173, 329891, 563011, 58246091)

# Cargamos el dataset
dataset <- fread("./datasets/resultados.csv")

# Nos quedamos solo con el 202101
dataset <- dataset[foto_mes == 202103]
# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
    clase_ternaria == "BAJA+2",
    "evento",
    "noevento"
)]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]
ganancia <- function(probabilidades, clase) {
    return(sum(
        (probabilidades >= 0.025) * ifelse(clase == "evento", 273000, -7000)
    ))
}
resultados_grid_search <- data.table()

# Complete los valores que se van a combinar para cada parámetro a explorar


for (cp in c(-1)) {
    for (md in c(6)) {
        for (ms in c(20)) {
            for (mb in c(3)) {
                t0 <- Sys.time()
                gan_semillas <- c()
                # for (s in semillas) {
                gan_semillas <- foreach(s = semillas, .combine = "c") %dopar% {
                    set.seed(s)
                    in_training <- caret::createDataPartition(
                        dataset[
                            ,
                            get("clase_binaria")
                        ],
                        p = 0.70, list = FALSE
                    )
                    dtrain <- dataset[in_training, ]
                    dtest <- dataset[-in_training, ]

                    modelo <- rpart(clase_binaria ~ .,
                        data = dtrain,
                        xval = 0,
                        cp = cp,
                        minsplit = ms,
                        minbucket = mb,
                        maxdepth = md
                    )

                    pred_testing <- predict(modelo, dtest, type = "prob")
                    gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3

                    gan_semillas <- c(gan_semillas, gan)
                    return(gan_semillas)
                }
                tiempo <- as.numeric(Sys.time() - t0, units = "secs")

                resultados_grid_search <- rbindlist(list(
                    resultados_grid_search,
                    data.table(
                        tiempo = tiempo,
                        cp = cp,
                        mb = mb,
                        ms = ms,
                        md = md,
                        gan = mean(gan_semillas)
                    )
                ))
                print(resultados_grid_search)
                stopImplicitCluster()
            }
        }
    }
}

# Visualizo los parámetros de los mejores parámetros
print(resultados_grid_search[gan == max(gan), ])

## TAREA:
## Una vez que tenga sus mejores parámetros, haga una copia del script
## rpart/z101_PrimerModelo.R, cambie los parámetros dentro del script,
## ejecutelo y suba a Kaggle su modelo.

## Preguntas
## - ¿Cuál es la diferencia entre **test** y **validation**?
## - ¿Cuántas veces podemos usar el conjunto de **test** sin
##   convertirlo en **validation**?
##
## La GRAN pregunta:
## - ¿Qué otra cosita de la materia tiene una partición 70 / 30?
## - Todo lo que hemos visto ¿Va a afectar a esa cosita?.
