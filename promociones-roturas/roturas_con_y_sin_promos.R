#Roturas con y sin promociones.

library(dplyr)

library(readxl)

setwd("C:/Users/maria.purificacion/Documents/promociones-roturas/14_nov")

# datos_promo <- read_excel("hr_promociones_2018.xlsx") 

datos_promo <- read_excel("Excel_promociones_hdr_2018_14_nov.xlsx")

#  colnames(datos_promo_2) <- c("id_servicio", "id_centro_cliente", "id_promocion", "codArticulo", "nombreArticulo", "roturaLineal",
#                                "roturaAlmacen", "fechaCalc", "idVisita", "codCentro", "idLectFacing", "pasillo", "modulo", "balda",
#                                "posicion", "facing", "alto", "tumbado", "activada", "activada2", "fecha_promocion", "id_promocion_centro",
#                              "codEnsena", "fecha_inicio", "fecha_fin")

# datos_promo_2[1,1] <- "3768900"

# str(datos_promo_2)

datos_totales <- read.csv("servicios_totales_hdr_2018_14_nov.csv", header=FALSE, sep=";", stringsAsFactors=FALSE)

# datos_totales_2 <- read.csv("~/promociones-roturas/servicios_totales_hdr_2018_12_nov.csv", header=FALSE, sep=";", stringsAsFactors=FALSE)

colnames(datos_totales) <- c("id_servicio", "id_centro_cliente", "codArticulo", "nombreArticulo", "roturaLineal",
                              "roturaAlmacen", "fechaCalc", "idVisita", "codCentro", "idLectFacing", "pasillo", "modulo", "balda",
                              "posicion", "facing", "alto", "tumbado")

datos_totales[1,1] <- "3619408"
datos_totales$id_servicio <- as.numeric(datos_totales$id_servicio)

# str(datos_totales_2)

# datos_totales <- datos_totales_2
# datos_totales$id_servicio <- as.numeric(datos_totales$id_servicio)
# 
# datos_promo <- datos_promo_2
# datos_promo$id_servicio <- as.numeric(datos_promo$id_servicio)


#We obtain the number of unique id_servicio from datos_promo.
# datos_promo %>%
#             summarise(n_distinct(id_servicio))



datos_no_promo <- anti_join(datos_totales, datos_promo, by = "id_servicio", "idLectFacing")


servicios_por_centro_articulo_no_promo <- datos_no_promo %>%
                                          group_by(codCentro, codArticulo) %>%
                                          count(codCentro) %>%
                                          arrange(codCentro)

colnames(servicios_por_centro_articulo_no_promo) <- c("codCentro", "codArticulo", "num_servicios_no_promo")

# View(servicios_por_centro_articulo_no_promo)

# sum(servicios_por_centro_articulo_no_promo$num_servicios_no_promo)

str(datos_promo)

servicios_por_centro_articulo_promo <- datos_promo %>%
                                      group_by(codCentro, codArticulo) %>%
                                       # group_by(codCentro) %>%
                                      count(codCentro) %>%
                                      arrange(codCentro)

colnames(servicios_por_centro_articulo_promo) <- c("codCentro", "codArticulo", "num_servicios_promo")

# View(servicios_por_centro_articulo_promo)


# servicios_por_ensena <- datos %>%
#                         group_by(codEnsena) %>%
#                         count(codEnsena) %>%
#                         arrange(codEnsena)
# 
# 
# View(servicios_por_ensena)

roturas_no_promo <- datos_no_promo %>%
                    arrange(codCentro, codArticulo, fechaCalc) %>%
                    filter(roturaLineal == 1 | roturaAlmacen == 1) %>%
                    select(codCentro, codArticulo, fechaCalc, roturaLineal, roturaAlmacen) 


roturas_promo <- datos_promo %>%
                arrange(codEnsena, codCentro, codArticulo, fecha_promocion) %>%
                filter(roturaLineal == 1 | roturaAlmacen == 1) %>%
                select(codEnsena, codCentro, codArticulo, fecha_promocion, roturaLineal, roturaAlmacen) 

# str(roturas)
# 
# 
# View(roturas)

# nrow(roturas)
# 
# table(roturas$codCentro)
# View(transform(table(roturas$codCentro)))


roturas_fq_no_promo <- roturas_no_promo %>%
                        count(codCentro, codArticulo) %>%
                        # arrange(desc(n))
                        arrange(codCentro)

colnames(roturas_fq_no_promo) <- c("codCentro", "codArticulo", "num_roturas_no_promo")


roturas_fq_promo <- roturas_promo %>%
                  count(codCentro, codArticulo) %>%
                  # arrange(desc(n))
                  arrange(codCentro)
                  
colnames(roturas_fq_promo) <- c("codCentro", "codArticulo", "num_roturas_promo")


# sum(roturas_fq_promo$num_roturas_promo)

# View(roturas_fq_promo)


roturas_por_servicios_no_promo <- merge(roturas_fq_no_promo, servicios_por_centro_articulo_no_promo, by = c("codCentro", "codArticulo"))

colnames(roturas_por_servicios_no_promo) <- c("codCentro", "codArticulo", "num_roturas_no_promo", "num_servicios_no_promo")

# View(roturas_por_servicios_no_promo)

roturas_por_servicios_promo <- merge(roturas_fq_promo, servicios_por_centro_articulo_promo, by = c("codCentro", "codArticulo"))

colnames(roturas_por_servicios_promo) <- c("codCentro", "codArticulo", "num_roturas_promo", "num_servicios_promo")

roturas_por_servicios_promo$porcent_roturas <- round((roturas_por_servicios_promo$num_roturas / roturas_por_servicios_promo$num_servicios_promo)*100, 2)

# View(roturas_por_servicios_promo)


roturas_totales <- merge(roturas_por_servicios_no_promo, roturas_por_servicios_promo, by = c("codCentro", "codArticulo"))

colnames(roturas_totales) <- c("st_codCentro", "st_codArticulo", "num_roturas_no_promo", "num_servicios_no_promo",
                               "num_roturas_promo", "num_servicios_promo", "porcent_roturas")

View(roturas_totales)



write.csv(roturas_totales, "roturas_totales_12_nov.csv")

































#Code chunks.

# View(roturas_por_servicios[order(roturas_por_servicios$porcent_roturas, decreasing = TRUE),])

roturas_tidy <- roturas_por_servicios[order(roturas_por_servicios$porcent_roturas, decreasing = TRUE),]

# nrow(roturas_tidy)

write.csv(roturas_tidy, "roturas_hr_17_tidy.csv")

# View(roturas_fq)


roturas_por_servicios_mas_de_20 <- roturas_por_servicios %>%
  filter(num_servicios >= 20)


View(roturas_por_servicios_mas_de_20)

