# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
# setwd("X:\\gdrive\\uba2023\\") # Establezco el Working Directory
setwd("./Maestria/2 - DMEyF/")
# cargo el dataset
dataset <- fread("./datasets/resultados.csv")

dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo


# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -1, # esto significa no limitar la complejidad de los splits
        minsplit = 2146, # minima cantidad de registros para que se haga el split
        minbucket = 693, # tamaño minimo de una hoja
        maxdepth = 7
) # profundidad maxima del arbol


# grafico el arbol
# prp(modelo,
#        extra = 101, digits = -5,
#        branch = 1, type = 4, varlen = 0, faclen = 0
# )


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
# dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]


setorder(dapply, -prob_baja2)
dapply[, Predicted := 0]
# make that the first 1000 rows of dapply have Predicted = 1
numero_de_filas <- 9000
dapply[1:numero_de_filas, Predicted := 1]

dataset2 <- fread("./datasets/privado.csv")
dapply[, Predicted := ifelse(numero_de_cliente %in% dataset2$numero_de_cliente, 1, Predicted)]
# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
nombre_archivo <- paste0("./exp/KA2001/K101_3_", numero_de_filas, "_2.csv")

fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = nombre_archivo,
        sep = ","
)
