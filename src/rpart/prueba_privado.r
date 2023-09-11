library(data.table)
setwd("/home/tomi/Escritorio/Maestria/2 - DMEyF/exp/KA2001/")

# Cargar los dos conjuntos de datos CSV
dataset1 <- fread("./K101_3_10000.csv")
dataset2 <- fread("./K101_3_9000_2.csv")
# dataset3 <- fread("./K101_8000.csv")

result <- dataset1[dataset2, on = "numero_de_cliente"]
# result <- result[dataset3, on = "numero_de_cliente"]

print(result)
# Filtrar las filas donde "Predicted" es 0 en el primer conjunto y 1 en el segundo conjunto
result <- result[Predicted == 0 & i.Predicted == 1]

# Seleccionar las columnas deseadas para el resultado
# fwrite(result[, .(numero_de_cliente)],
#    file = "privado.csv",
#    col.names = TRUE,
#    sep = ",",
#    quote = FALSE
# )
# Mostrar el resultado
print(result)
