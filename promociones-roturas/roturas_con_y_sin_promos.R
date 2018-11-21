#Roturas con y sin promociones.

library(dplyr)
library(readxl)

setwd("C:/Users/maria.purificacion/Documents/promociones-roturas/21_nov")

datos_promo <- read.csv("hdr_promociones_2018.csv", header=FALSE, sep=";", stringsAsFactors=FALSE) 

colnames(datos_promo) <- c("id_servicio", "id_promocion", "codArticulo", "nombreArticulo", "id_promocion_centro",
                           "id_centro_cliente", "codEnsena", "codCentro", "activada", "activada2", "roturaLineal",
                           "roturaAlmacen", "fecha_promocion")

datos_totales <- read.csv("hdr_servicios_totales_2018.csv", header=FALSE, sep=";", stringsAsFactors=FALSE)
colnames(datos_totales) <- c("id_servicio", "id_centro_cliente", "codArticulo", "nombreArticulo", "roturaLineal",
                            "roturaAlmacen", "fechaCalc", "idVisita", "codCentro")


datos_no_promo <- anti_join(datos_totales, datos_promo, by = "id_servicio")


servicios_por_centro_articulo_no_promo <- datos_no_promo %>%
                                          group_by(codCentro, codArticulo) %>%
                                          count(codCentro) %>%
                                          arrange(codCentro)

colnames(servicios_por_centro_articulo_no_promo) <- c("codCentro", "codArticulo", "num_servicios_no_promo")


servicios_por_centro_articulo_promo <- datos_promo %>%
                                      group_by(codCentro, codArticulo) %>%
                                      count(codCentro) %>%
                                      arrange(codCentro)

colnames(servicios_por_centro_articulo_promo) <- c("codCentro", "codArticulo", "num_servicios_promo")


roturas_no_promo <- datos_no_promo %>%
                    arrange(codCentro, codArticulo, fechaCalc) %>%
                    filter(roturaLineal == 1 | roturaAlmacen == 1) %>%
                    select(codCentro, codArticulo, fechaCalc, roturaLineal, roturaAlmacen) 



roturas_promo <- datos_promo %>%
                arrange(codEnsena, codCentro, codArticulo, fecha_promocion) %>%
                filter(roturaLineal == 1 | roturaAlmacen == 1) %>%
                select(codEnsena, codCentro, codArticulo, fecha_promocion, roturaLineal, roturaAlmacen) 


#Frecuencia de roturas.
frecuencia_roturas_no_promo <- roturas_no_promo %>%
                        count(codCentro, codArticulo) %>%
                        arrange(codCentro)

colnames(frecuencia_roturas_no_promo) <- c("codCentro", "codArticulo", "num_roturas_no_promo")


frecuencia_roturas_promo <- roturas_promo %>%
                            count(codCentro, codArticulo) %>%
                            arrange(codCentro)

                  
colnames(frecuencia_roturas_promo) <- c("codCentro", "codArticulo", "num_roturas_promo")



roturas_por_servicios_no_promo <- merge(frecuencia_roturas_no_promo, servicios_por_centro_articulo_no_promo, by = c("codCentro", "codArticulo"))

colnames(roturas_por_servicios_no_promo) <- c("codCentro", "codArticulo", "num_roturas_no_promo", "num_servicios_no_promo")


roturas_por_servicios_promo <- merge(frecuencia_roturas_promo, servicios_por_centro_articulo_promo, by = c("codCentro", "codArticulo"))

colnames(roturas_por_servicios_promo) <- c("codCentro", "codArticulo", "num_roturas_promo", "num_servicios_promo")


roturas_por_servicios_no_promo$porcent_roturas_no_promo <- 
                  round((roturas_por_servicios_no_promo$num_roturas_no_promo / roturas_por_servicios_no_promo$num_servicios_no_promo)*100, 2)

roturas_por_servicios_promo$porcent_roturas_promo <- round((roturas_por_servicios_promo$num_roturas / roturas_por_servicios_promo$num_servicios_promo)*100, 2)


roturas_totales <- merge(roturas_por_servicios_no_promo, roturas_por_servicios_promo, by = c("codCentro", "codArticulo"))


# View(roturas_totales)

write.csv(roturas_totales, "roturas_totales_21_nov.csv")




