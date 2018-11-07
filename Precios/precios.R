
#Test with rate 1/4

setwd("C:/Users/maria.purificacion/Documents/Precios")

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)


datos_original <- read_excel("listado_precios_1_3_extendido.xlsx")

datos <- datos_original


#We convert in character the number to manipulating.
datos$precio_servicio <- as.character(datos$precio_servicio_original)
#We remove the comma or point if they have it, (Excel changes the symbol):
datos$precio_servicio <- gsub("[,]","", datos$precio_servicio)

#We convert in numeric the field "precio_articulos", (first we replace comma by point like decimal separator).
datos$precio_articulos <- gsub(",", ".", datos$precio_articulos)
datos$precio_articulos <- as.numeric(datos$precio_articulos)


#A) Integer part of prices articles is 0.
subset_start_with_0 <- subset(datos, subset = floor(datos$precio_articulos) == 0)


#We add a 0 to the number in precio_servicio field.
subset_start_with_0_df <- subset_start_with_0 %>%
                          mutate(new_precio_servicio = paste0("0.",precio_servicio))


#B) Integer part of prices articles is 1.
subset_starts_with_1 <- subset(datos, subset = floor(datos$precio_articulos) == 1)

#We are going to think that if the price service is greater or equal than 3 we paste a 0 before the number, and if it is smaller we add decimal point after the first
#digit.
#First of all, we remove the decimal point in the "precio_servicio" field.
# subset_starts_with_1$precio_servicio <- gsub("[.]","", subset_starts_with_1$precio_servicio)
#1º
subset_starts_with_1_greater_than_3 <- subset_starts_with_1 %>%
                                            filter(substr(precio_servicio,1,1) >= 3) %>%
                                            mutate(new_precio_servicio = paste0("0.",precio_servicio))

#2º  
subset_starts_with_1_smaller_than_3 <- subset_starts_with_1 %>%  
                                            filter(substr(precio_servicio,1,1) < 3) %>%
                                            mutate(new_precio_servicio = as.numeric(precio_servicio)/10^(nchar(precio_servicio)-1))



#C) Integer part of prices articles is different of 0 and 1 but it has one character only.
subset_no_starts_with_0_and_1 <- subset(datos, subset = floor(datos$precio_articulos) != 0 & floor(datos$precio_articulos) != 1)

#We select the prices articles with 1 digit before the decimal point.
subset_no_starts_with_0_and_1_one_integer_digit <- subset_no_starts_with_0_and_1 %>%
                                                    filter(floor(log10(subset_no_starts_with_0_and_1$precio_articulos)) + 1 == 1) %>%
                                                    mutate(new_precio_servicio = as.numeric(precio_servicio)/10^(nchar(precio_servicio)-1))



#D) Integer part of prices articles is different of 0 and 1, and it has more than one character.
#We select the prices articles with more than 1 digit before the decimal point.
subset_no_starts_with_0_and_1_more_one_integer_digit <- subset_no_starts_with_0_and_1 %>%
                                                        filter(floor(log10(subset_no_starts_with_0_and_1$precio_articulos)) + 1 > 1)

#We remove the decimal point in services prices.
# subset_no_starts_with_0_and_1_more_one_integer_digit$precio_servicio <- gsub("[.]","", subset_no_starts_with_0_and_1_more_one_integer_digit$precio_servicio)

#1º Prices articles between 10 and 19.
subset_no_starts_with_0_and_1_more_one_integer_digit_1x <- subset_no_starts_with_0_and_1_more_one_integer_digit %>%
                                                            filter(precio_articulos >= 10 & precio_articulos < 20)
                                                           
#a) We filter prices services which start by 9.  
subset_no_starts_with_0_and_1_more_one_integer_digit_1x_and_start_by_9 <- subset_no_starts_with_0_and_1_more_one_integer_digit_1x  %>%
                                                                          filter(grepl("^9",precio_servicio)) %>% 
                                                                          mutate(new_precio_servicio = as.numeric(precio_servicio)/10^(nchar(precio_servicio)-1))

#b) Prices services which don't start by 9.
subset_no_starts_with_0_and_1_more_one_integer_digit_1x_and_non_start_by_9 <- subset_no_starts_with_0_and_1_more_one_integer_digit_1x  %>%
                                                                              filter(!grepl("^9",precio_servicio)) %>% 
                                                                              mutate(new_precio_servicio = as.numeric(precio_servicio)/10^(nchar(precio_servicio)-2))


#2º In the remaining cases.
subset_no_starts_with_0_and_1_more_one_integer_digit_more_than_20 <- subset_no_starts_with_0_and_1_more_one_integer_digit %>%
                                                                      filter(precio_articulos > 20) %>%
                                                                      mutate(new_precio_servicio = as.numeric(precio_servicio)/10^(nchar(precio_servicio)-2))







#We join the results.

result <- rbind(subset_start_with_0_df, 
                subset_starts_with_1_greater_than_3, 
                subset_starts_with_1_smaller_than_3, 
                subset_no_starts_with_0_and_1_one_integer_digit, 
                subset_no_starts_with_0_and_1_more_one_integer_digit_1x_and_start_by_9, 
                subset_no_starts_with_0_and_1_more_one_integer_digit_1x_and_non_start_by_9,
                subset_no_starts_with_0_and_1_more_one_integer_digit_more_than_20)




#We are goint to change the precio_servicio by the original data.
#First of all, we order the data frame by the id field.
# result[order(result$id),]
# datos_original[order(datos_original$id),]
result$new_precio_servicio <- as.numeric(result$new_precio_servicio)
result$precio_servicio <- as.numeric(result$precio_servicio)

merge(result, datos_original, by="id")

#We remove the column not necessary (price modified and others).
end_result <- result[,-c(4,10)]

View(end_result)

write.csv(end_result, "results_07_11_1.csv")


#Until here if we don't want plots or statistical calculus.




#We are going to draw the charts with the differences:
#We create two fields to represent the variation between them.
end_result$diferencia_inicial <- end_result$precio_servicio_original - end_result$precio_articulos
end_result$diferencia_final <- end_result$new_precio_servicio - end_result$precio_articulos
end_result$porcentaje_diferencia <- abs(100 - (end_result$new_precio_servicio / end_result$precio_articulos)*100)
#We round the porcentaje_diferencia field.
end_result$porcentaje_diferencia <- round(end_result$porcentaje_diferencia, 2)


ggplot() +
  geom_line(data = end_result, aes(x = precio_articulos, y = diferencia_final, colour = "darkblue")) +
  geom_line(data = end_result, aes(x = precio_articulos, y = diferencia_inicial, colour = "red")) +
  xlab("Precios artículos") +
  ylab('Diferencia precios artículo y servicio') +
  scale_color_discrete(name = "", labels = c("Diferencia final", "Diferencia inicial")) +
  labs(title="Resultado ejecución script para diferencias entre precios de artículos y de servicios")



# write.csv(end_result, "results1_3_extendido_25_10_13h.csv")

#Statistical measures.
quantile(end_result$codEmpleado)
summary(end_result$codEmpleado)


#Distribution frequencyes by employee.
table(end_result$codEmpleado)
View(transform(table(end_result$codEmpleado)))

employee_frequency <- transform(table(end_result$codEmpleado))

#Plots
pie(table(end_result$codEmpleado))
barplot(table(end_result$codEmpleado))



write.csv(employee_frequency, "employee_frequency_1_3_25_10.csv")

#To view data like SPSS.
install.packages("sjPlot")
library(sjPlot)
library(sjmisc)

# sjt.frq(end_result$codEmpleado) <- deprecated, instead:
output <- sjmisc::frq(end_result$codEmpleado)
write.csv(output, "output_employee_1_4_25_10.csv")










#=========================================================================================================================
#Tests


no_starts_with_0 <- which(floor(datos$precio_articulos) != 0)
subset_no_start_with_0 <- subset(datos, subset = floor(datos$precio_articulos) != 0)

#Number of integer digits, before the decimal point, (this is valid for numbers they are not equal to 0).
#We save the result in a new field.
subset_no_start_with_0$integer_part_precios_articulos <- floor(log10(subset_no_start_with_0$precio_articulos)) + 1
which(subset_no_start_with_0$integer_part_precios_articulos == 1 & floor(subset_no_start_with_0$precio_articulos) == 1)






precio_servicio <- "253"
precio_servicio <- as.numeric(precio_servicio)

new_precio_servicio = precio_servicio/10^(nchar(precio_servicio)-1)

new_number <- 25/10

sprintf("%.", 101)
  
sprintf("%s %d", "test", 1:3)
  
  
substr(605,1,1)  



View(subset_no_start_with_0)

fix(subset_no_start_with_0)

