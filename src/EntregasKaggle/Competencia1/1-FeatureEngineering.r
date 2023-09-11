library(data.table)

setwd("./Maestria/2 - DMEyF/")
datos <- fread("./datasets/competencia_01.csv")

print(colnames(datos))

datos[, A := mrentabilidad / mrentabilidad_annual]
datos[, sum_cseguro := rowSums(.SD[, c("cseguro_vida", "cseguro_auto", "cseguro_vivienda", "cseguro_accidentes_personales"), with = FALSE])]
datos[, sum_ctarjeta := rowSums(.SD[, c("ctarjeta_visa_debitos_automaticos", "ctarjeta_master_debitos_automaticos", "ccuenta_debitos_automaticos"), with = FALSE])]
datos[, sum_mtarjeta := rowSums(.SD[, c("mttarjeta_visa_debitos_automaticos", "mttarjeta_master_debitos_automaticos", "mcuenta_debitos_automaticos"), with = FALSE])]
datos[, sum_cpagos := rowSums(.SD[, c("cpagodeservicios", "cpagomiscuentas"), with = FALSE])]
datos[, sum_mpagos := rowSums(.SD[, c("mpagodeservicios", "mpagomiscuentas"), with = FALSE])]
datos[, sum_cpayroll := rowSums(.SD[, c("cpayroll_trx", "cpayroll2_trx"), with = FALSE])]
datos[, sum_mpayroll := rowSums(.SD[, c("mpayroll", "mpayroll2"), with = FALSE])]
datos[, sum_mcuenta := rowSums(.SD[, c("mcuenta_corriente_adicional", "mcuenta_corriente", "mcaja_ahorro", "mcaja_ahorro_adicional"), with = FALSE])]
datos[, sum_ctarjeta := rowSums(.SD[, c("ctarjeta_visa", "ctarjeta_master"), with = FALSE])]
datos[, sum_transacciones := rowSums(.SD[, c("ctarjeta_visa_transacciones", "ctarjeta_master_transacciones"), with = FALSE])]
datos[, sum_consumo := rowSums(.SD[, c("mtarjeta_visa_consumo", "mtarjeta_master_consumo"), with = FALSE])]
datos[, sum_inversion := rowSums(.SD[, c("cplazo_fijo", "cinversion1", "cinversion2"), with = FALSE])]
datos[, sum_inversion_dolares := rowSums(.SD[, c("mplazo_fijo_dolares", "minversion1_dolares"), with = FALSE])]
datos[, sum_inversion_pesos := rowSums(.SD[, c("mplazo_fijo_pesos", "minversion1_pesos", "minversion2"), with = FALSE])]
datos[, sum_descuentos := rowSums(.SD[, c("ccajeros_propios_descuentos", "ctarjeta_visa_descuentos", "ctarjeta_master_descuentos"), with = FALSE])]
datos[, sum_descuentos_m := rowSums(.SD[, c("mcajeros_propios_descuentos", "mtarjeta_visa_descuentos", "mtarjeta_master_descuentos"), with = FALSE])]
datos[, sum_comisiones := rowSums(.SD[, c("ccomisiones_mantenimiento", "ccomisiones_otras"), with = FALSE])]
datos[, sum_comisiones_m := rowSums(.SD[, c("mcomisiones_mantenimiento", "mcomisiones_otras"), with = FALSE])]
datos[, mtransferencias := mtransferencias_recibidas / mtransferencias_emitidas]
datos[, cajas_balance := ccajas_depositos - ccajas_extracciones]
datos[, sum_atm := rowSums(.SD[, c("matm", "matm_other"), with = FALSE])]
datos[, sum_transacciones_cajas := rowSums(.SD[, c("chomebanking_transacciones", "ccajas_transacciones"), with = FALSE])]
datos[, sum_transacciones_otras := rowSums(.SD[, c("ctarjeta_debito_transacciones", "ccallcenter_transacciones", "chomebanking_transacciones", "ccajas_transacciones", "catm_trx", "catm_trx_other", "cmobile_app_trx"), with = FALSE])]
datos[, p_ctrx := sum_transacciones_otras / ctrx_quarter]

columnas_a_eliminar <- unique(c(
    "mrentabilidad", "mrentabilidad_annual",
    "cseguro_vida", "cseguro_auto", "cseguro_vivienda", "cseguro_accidentes_personales",
    "ctarjeta_visa_debitos_automaticos", "ctarjeta_master_debitos_automaticos", "ccuenta_debitos_automaticos",
    "mttarjeta_visa_debitos_automaticos", "mttarjeta_master_debitos_automaticos", "mcuenta_debitos_automaticos",
    "cpagodeservicios", "cpagomiscuentas",
    "mpagodeservicios", "mpagomiscuentas",
    "cpayroll_trx", "cpayroll2_trx",
    "mpayroll", "mpayroll2",
    "mcuenta_corriente_adicional", "mcuenta_corriente", "mcaja_ahorro", "mcaja_ahorro_adicional",
    "ctarjeta_visa", "ctarjeta_master",
    "ctarjeta_visa_transacciones", "ctarjeta_master_transacciones",
    "mtarjeta_visa_consumo", "mtarjeta_master_consumo",
    "cplazo_fijo", "cinversion1", "cinversion2",
    "mplazo_fijo_dolares", "minversion1_dolares",
    "mplazo_fijo_pesos", "minversion1_pesos", "minversion2",
    "ccajeros_propios_descuentos", "ctarjeta_visa_descuentos", "ctarjeta_master_descuentos",
    "mcajeros_propios_descuentos", "mtarjeta_visa_descuentos", "mtarjeta_master_descuentos",
    "ccomisiones_mantenimiento", "ccomisiones_otras",
    "mcomisiones_mantenimiento", "mcomisiones_otras",
    "mtransferencias_recibidas", "mtransferencias_emitidas",
    "ccajas_depositos", "ccajas_extracciones",
    "matm", "matm_other",
    "chomebanking_transacciones", "ccajas_transacciones",
    "ctarjeta_debito_transacciones", "ccallcenter_transacciones", "chomebanking_transacciones", "ccajas_transacciones", "catm_trx", "catm_trx_other", "cmobile_app_trx",
    "ctrx_quarter",
    "Master_mlimitecompra", "Visa_mlimitecompra"
))

datos[, (columnas_a_eliminar) := NULL]

write.csv(datos, "./Maestria/2 - DMEyF/datasets/resultados.csv", row.names = FALSE)
