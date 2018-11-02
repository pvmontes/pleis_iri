#In this file we are going to work with the Iri's elements that are not in Pleis, and with the Pleis' elements that are not in Iri.
#Executing after pleis_iri.R
#For this reason we create two Excel files from the files "pleis_not_common_elements.csv" and "iri_not_common_elements.csv"; these files are created at the end of data_analysis_v11.R


#install.packages("readxl")
#install.packages("dplyr")

# library(readxl)
# library(dplyr)
# # #
# setwd("C:/Users/maria.purificacion/Documents/Datos_pleis_IRI/Trabajo")
#getwd()

#==================================================================================================



# pleis_v12 <- read.csv("pleis_not_common_elements_24_09.csv") 
# iri_v12 <- read.csv("iri_not_common_elements_24_09.csv")

pleis_v12 <- pleis_not_common_elements
iri_v12 <- iri_not_common_elements





#====================================================================================================
#The next is the same code that we use in pleis_v12_iri.R, to manipulate the centers and addresses.



#We create a function to remove the articles and spaces on left each time we delete a string at the begining of the sentence.
from <- c("^[[:space:]]", "^LA+[[:space:]]", "^LO+[[:space:]]", "^EL+[[:space:]]", "^LAS+[[:space:]]", "^LOS+[[:space:]]", 
          "^A+[[:space:]]", "^O+[[:space:]]", "^AS+[[:space:]]", "^OS+[[:space:]]", "^LES+[[:space:]]", "^ELS+[[:space:]]")
to <- c("", "", "", "", "", "", "", "", "", "", "", "")

mi_gsub <- function(pattern, replacement, x, ...) {
  for(i in 1:length(pattern))
    x <<- gsub(pattern[i], replacement[i], x, ...)
  x
}


#We convert iri$Direccion1 and iri$Enseña to upper case and remove tildes, (just in case).
iri_v12$Direccion1 <- toupper(iri_v12$Direccion1)
iri_v12$Enseña <- toupper(iri_v12$Enseña)
iri_v12$Rótulo <- toupper(iri_v12$Rótulo)
iri_v12$Provincia <- toupper(iri_v12$Provincia)
iri_v12$Municipio <- toupper(iri_v12$Municipio)
pleis_v12$direccion <- toupper(pleis_v12$direccion)
pleis_v12$provincia <- toupper(pleis_v12$provincia)
pleis_v12$poblacion <- toupper(pleis_v12$poblacion)


#We remove all the commas from the addresses.
iri_v12$Direccion1 <- gsub(",","",iri_v12$Direccion1)
pleis_v12$direccion <- gsub(",","",pleis_v12$direccion)


#We remove the bars.
iri_v12$Direccion1 <- gsub("[[:space:]]+/+[[:space:]]+/+[[:space:]]+/$","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("/+[[:space:]]+/+[[:space:]]+/$","",iri_v12$Direccion1) 
pleis_v12$direccion <- gsub("[[:space:]]+/+[[:space:]]+/+[[:space:]]+/$","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("/+[[:space:]]+/+[[:space:]]+/$","",pleis_v12$direccion) 

#We remove the accents.
iri_v12$Enseña <- gsub("Á", "A", iri_v12$Enseña)
iri_v12$Enseña <- gsub("É", "E", iri_v12$Enseña)
iri_v12$Enseña <- gsub("Í", "I", iri_v12$Enseña)
iri_v12$Enseña <- gsub("Ó", "O", iri_v12$Enseña)
iri_v12$Enseña <- gsub("Ú", "U", iri_v12$Enseña)
iri_v12$Enseña <- gsub("Ü", "U", iri_v12$Enseña)
iri_v12$Rótulo <- gsub("Á", "A", iri_v12$Rótulo)
iri_v12$Rótulo <- gsub("É", "E", iri_v12$Rótulo)
iri_v12$Rótulo <- gsub("Í", "I", iri_v12$Rótulo)
iri_v12$Rótulo <- gsub("Ó", "O", iri_v12$Rótulo)
iri_v12$Rótulo <- gsub("Ú", "U", iri_v12$Rótulo)
iri_v12$Rótulo <- gsub("Ü", "U", iri_v12$Rótulo)
pleis_v12$ensena <- gsub("Á", "A", pleis_v12$ensena)
pleis_v12$ensena <- gsub("É", "E", pleis_v12$ensena)
pleis_v12$ensena <- gsub("Í", "I", pleis_v12$ensena)
pleis_v12$ensena <- gsub("Ó", "O", pleis_v12$ensena)
pleis_v12$ensena <- gsub("Ú", "U", pleis_v12$ensena)
pleis_v12$ensena <- gsub("Ü", "U", pleis_v12$ensena)

#Acento invertido
iri_v12$Enseña <- gsub("À", "A", iri_v12$Enseña)
iri_v12$Enseña <- gsub("È", "E", iri_v12$Enseña)
iri_v12$Enseña <- gsub("Ì", "I", iri_v12$Enseña)
iri_v12$Enseña <- gsub("Ò", "O", iri_v12$Enseña)
iri_v12$Enseña <- gsub("Ù", "U", iri_v12$Enseña)
iri_v12$Rótulo <- gsub("À", "A", iri_v12$Rótulo)
iri_v12$Rótulo <- gsub("È", "E", iri_v12$Rótulo)
iri_v12$Rótulo <- gsub("Ì", "I", iri_v12$Rótulo)
iri_v12$Rótulo <- gsub("Ò", "O", iri_v12$Rótulo)
iri_v12$Rótulo <- gsub("Ù", "U", iri_v12$Rótulo)
pleis_v12$ensena <- gsub("À", "A", pleis_v12$ensena)
pleis_v12$ensena <- gsub("È", "E", pleis_v12$ensena)
pleis_v12$ensena <- gsub("Ì", "I", pleis_v12$ensena)
pleis_v12$ensena <- gsub("Ò", "O", pleis_v12$ensena)
pleis_v12$ensena <- gsub("Ù", "U", pleis_v12$ensena)



#======================================================================================================
#Functions
function_replace_string_ensena <- function(m_vector, string) { 
  pleis_v12[m_vector,"ensena"] <<- c(string) 
}

function_rotulo <- function(m_vector) {
  iri_v12[m_vector,"Enseña"] <<- iri_v12[m_vector,"Rótulo"] 
}

function_cadena_pleis_v12 <- function(p_vector) {
  pleis_v12[p_vector,"ensena"] <<- pleis_v12[p_vector,"cadena"]
}

function_unide <- function(m_vector) {
  pleis_v12[m_vector,"ensena"] <<- c("UNIDE, S. COOP.")
}

function_la_despensa <- function(m_vector) {
  pleis_v12[m_vector,"ensena"] <<- c("ECO-MORA")
}

function_leclerc <- function(m_vector){
  pleis_v12[m_vector, "ensena"] <<- c("LECLERC")
}

function_lider_pleis_v12 <- function(m_vector){
  pleis_v12[m_vector, "ensena"] <<- c("LIDER-ALIMENT")
}

function_lider_iri_v12 <- function(m_vector){
  iri_v12[m_vector, "Enseña"] <<- c("LIDER-ALIMENT")
}


#========================================================================================================
#Eroski
iri_v12$Enseña <- gsub("EROSKI+[[:space:]]+[A-Z]+$","EROSKI", iri_v12$Enseña)
iri_v12$Enseña <- gsub("[A-Z]+EROSKI+$","EROSKI", iri_v12$Enseña)
pleis_v12$ensena <- gsub("EROSKI+[[:space:]]+[A-Z]+$","EROSKI", pleis_v12$ensena)
pleis_v12$ensena <- gsub("EROSKI+\\-+[A-Z]+","EROSKI", pleis_v12$ensena)
# pleis_v12$ensena <- gsub("ALIPROX","EROSKI", pleis_v12$ensena)
index_ensena_cadena_eroski <- which(pleis_v12$cadena == "GRUPO EROSKI" & pleis_v12$ensena == "FAMILIA") #To replace the function change_some_fields()

function_replace_string_ensena(index_ensena_cadena_eroski, "EROSKI") 
index_franquicia_eroski <- which(iri_v12$Enseña == "FRANQUICIA EROSKI")
#=======================================================================================================
#El Corte Ingles
pleis_v12$ensena <- gsub("TIENDAS ECI","EL CORTE INGLES", pleis_v12$ensena)
pleis_v12$ensena <- gsub("CONVENIENCE STORE","EL CORTE INGLES", pleis_v12$ensena)
pleis_v12$ensena <- gsub("HIPERCOR","EL CORTE INGLES", pleis_v12$ensena)
pleis_v12$ensena <- gsub("OPENCOR","EL CORTE INGLES", pleis_v12$ensena)
pleis_v12$ensena <- gsub("SUPERCOR EXPRES","EL CORTE INGLES", pleis_v12$ensena)
pleis_v12$ensena <- gsub("SUPERCOR","EL CORTE INGLES", pleis_v12$ensena)
pleis_v12$ensena <- gsub("REPSOL SUPERCOR","EL CORTE INGLES", pleis_v12$ensena)
iri_v12$Enseña <- gsub("HIPERCOR","EL CORTE INGLES", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SUPERCOR","EL CORTE INGLES", iri_v12$Enseña)
#=======================================================================================================
#GADISA
iri_v12$Enseña <- gsub("GADISA","GADIS", iri_v12$Enseña)
# pleis_v12$ensena <- gsub("CLAUDIO","GADIS",pleis_v12$ensena) 
index_claudio <- which(iri_v12$Rótulo == "CLAUDIO")
#=======================================================================================================
#Carrefour
pleis_v12$ensena <- gsub("CARREFOUR+[[:space:]]+[A-Z]+$","CARREFOUR", pleis_v12$ensena)
pleis_v12$ensena <- gsub("SUPECO","CARREFOUR", pleis_v12$ensena)
iri_v12$Enseña <- gsub("CARREFOUR SUPER","CARREFOUR", iri_v12$Enseña)
#=======================================================================================================
#Dia
iri_v12$Enseña <- gsub("^[A-Z]+[[:space:]]+DIA","DIA",iri_v12$Enseña) #We remove MAXI from MAXI DIA in iri_v12/Enseña.
pleis_v12$ensena <- gsub("DIA+[[:space:]]+[A-Z]+$","DIA", pleis_v12$ensena)
pleis_v12$ensena <- gsub("^[A-Z]+[[:space:]]+DIA","DIA",pleis_v12$ensena) #We remove MAXI from MAXI DIA in iri_v12/Enseña.
pleis_v12$ensena <- gsub("LA PLAZA DE DIA","DIA", pleis_v12$ensena)
#=======================================================================================================
#El Arbol
index_arbol <- which(iri_v12$Rótulo == "EL ARBOL")
#Test
pleis_v12$ensena <- gsub("EL ARBOL","DIA", pleis_v12$ensena)
#=======================================================================================================
#Alcampo
pleis_v12$ensena <- gsub("ARO ROJO","SABECO",pleis_v12$ensena)
pleis_v12$ensena <- gsub("HIPER SIMPLY","SABECO",pleis_v12$ensena)
pleis_v12$ensena <- gsub("SIMPLY BASIC","SABECO",pleis_v12$ensena)
pleis_v12$ensena <- gsub("SIMPLY CITY","SABECO",pleis_v12$ensena)
pleis_v12$ensena <- gsub("SIMPLY MARKET","SABECO",pleis_v12$ensena)
pleis_v12$ensena <- gsub("SIMPLY STORE","SABECO",pleis_v12$ensena)
iri_v12$Enseña <- gsub("ALCAMPO CITY", "ALCAMPO", iri_v12$Enseña)
#=======================================================================================================
#7 alimentacion 7, S.A., Supermercados Ribetans
pleis_v12$ensena <- gsub("COALIMENT","COVALCO-COALIMENT",pleis_v12$ensena)
pleis_v12$ensena <- gsub("COALIMENT COMPRA SALUDABLE","COVALCO-COALIMENT",pleis_v12$ensena)
#index_coaliment <- which(iri_v12$Rótulo == "COALIMENT")
#index_coaliment_compra_saludable <- which(iri_v12$Rótulo == "COALIMENT COMPRA SALUDABLE")
#=======================================================================================================
#Ahorraves
index_ahorraves <- which(iri_v12$Rótulo == "AHORRAVES")
#=======================================================================================================
#Salinas
index_salinas <- which(iri_v12$Rótulo == "SALINAS")
#=======================================================================================================
#Pincha precios
index_pincha_precios <- which(iri_v12$Rótulo == "PINCHA PRECIOS")
#=======================================================================================================
#Super Ecozar
index_ecozar <- which(iri_v12$Rótulo == "SUPERMERCADOS ECOZAR")
function_rotulo(index_ecozar)
pleis_v12$ensena <- gsub("SUP.ECOZAR","ECOZAR",pleis_v12$ensena)
#=======================================================================================================
#KOMO-KOMO
index_komo_komo <- which(iri_v12$Rótulo == "KOMO-KOMO")
#=======================================================================================================
#La compra
index_la_compra <- which(iri_v12$Rótulo == "LA COMPRA")
#=======================================================================================================
#Vivo
index_vivo <- which(iri_v12$Rótulo == "VIVO - MARTINEZ")
function_rotulo(index_vivo)
iri_v12$Enseña <- gsub("VIVO - MARTINEZ", "VIVO", iri_v12$Enseña)
#=======================================================================================================
#Darvi, Iberplus
index_darvi <- which(iri_v12$Rótulo == "DARVI")
index_iberplus <- which(iri_v12$Rótulo == "IBERPLUS")
#=======================================================================================================
#Autoservicio El 66
index_el_66 <- which(iri_v12$Rótulo == "AUTOSERVICIO EL 66")
function_rotulo(index_el_66)
pleis_v12$ensena <- gsub("AUTOSERVIC 66","AUTOSERVICIO EL 66",pleis_v12$ensena)
#=======================================================================================================
#Todo Todo
index_todo_todo_romen <- which(iri_v12$Rótulo == "TODO-TODO ROMEN") #| iri_v12$Rótulo == "TODO-TODO OTROS")
function_rotulo(index_todo_todo_romen)
iri_v12$Enseña <- gsub("TODO-TODO ROMEN", "TODO-TODO", iri_v12$Enseña)
index_todo_todo_otros <- which(iri_v12$Rótulo == "TODO-TODO OTROS") #| iri_v12$Rótulo == "TODO-TODO OTROS")
function_rotulo(index_todo_todo_otros)
iri_v12$Enseña <- gsub("TODO-TODO OTROS", "TODO-TODO", iri_v12$Enseña)
pleis_v12$ensena <- gsub("TODO TODO","TODO-TODO",pleis_v12$ensena)
pleis_v12$ensena <- gsub("SPAR TODO-TODO","TODO-TODO",pleis_v12$ensena)
pleis_v12$ensena <- gsub("VIVO-TODOTODO","TODO-TODO",pleis_v12$ensena)   #I don't know if there are match with these centers. I see that not.
#=======================================================================================================
#La lonja
index_la_lonja <- which(iri_v12$Rótulo == "LA LONJA")
#=======================================================================================================
#Cabrero e hijos
index_cabrero_e_hijos <- which(pleis_v12$cadena == "CABRERO E HIJOS, S.A.")
function_cadena_pleis_v12(index_cabrero_e_hijos)
pleis_v12$ensena <- gsub("CABRERO E HIJOS, S.A.","CABRERO E HIJOS",pleis_v12$ensena)
#=======================================================================================================
#Maxcoop
# index_maxcoop <- which(iri_v12$Rótulo == "MAXCOOP")
#=======================================================================================================
#El tostadero
index_el_tostadero <- which(iri_v12$Rótulo == "SUPERMERCADOS EL TOSTADERO")
function_rotulo(index_el_tostadero)
iri_v12$Enseña <- gsub("SUPERMERCADOS EL TOSTADERO", "EL_TOSTADERO", iri_v12$Enseña)
pleis_v12$ensena <- gsub("EL TOSTADERO","EL_TOSTADERO",pleis_v12$ensena)
#=======================================================================================================
#Supersur y Don Market
index_supersur <- which(iri_v12$Rótulo == "SUPER SUR")
function_rotulo(index_supersur)
iri_v12$Enseña <- gsub("SUPER SUR", "SUPERSUR", iri_v12$Enseña)
index_don_market <- which(iri_v12$Rótulo == "DON MARKET")
#=======================================================================================================
#Cash Basauri
index_basauri <- which(iri_v12$Rótulo == "SUPERMERCADO BASAURI")
function_rotulo(index_basauri)
iri_v12$Enseña <- gsub("SUPERMERCADO BASAURI", "BASAURI", iri_v12$Enseña)
#=======================================================================================================
#Cash Lepe
index_cash_lepe <- which(iri_v12$Rótulo == "EL JAMON")
#=======================================================================================================
#Eko ama
# index_eko_ama <- which(iri_v12$Rótulo == "EKO AMA") 
# function_rotulo(index_eko_ama)
# iri_v12$Enseña <- gsub("EKO AMA", "EKO-AMA", iri_v12$Enseña)
#=======================================================================================================
#Froiz
#v9
# index_super_froiz <- which(iri_v12$Rótulo == "SUPERMERCADOS FROIZ")
# function_rotulo(index_super_froiz)
# iri_v12$Enseña <- gsub("SUPERMERCADOS FROIZ", "SUPERMERCADOS-FROIZ")
# pleis_v12$ensena <- gsub("SUPER FROIZ","SUPERMERCADOS-FROIZ",pleis_v12$ensena)
# index_tandy <- which(iri_v12$Rótulo == "TANDY-FROIZ")
# function_rotulo(index_tandy)


# pleis_v12$ensena <- gsub("TANDY","TANDY-FROIZ",pleis_v12$ensena)

#v8 , (lo comento el 22/08/2018)
# index_super_froiz <- which(iri_v12$Rótulo == "SUPERMERCADOS FROIZ")
# function_rotulo(index_super_froiz)
# pleis_v12$ensena <- gsub("SUPER FROIZ","SUPERMERCADOS FROIZ",pleis_v12$ensena)
# index_tandy <- which(iri_v12$Rótulo == "TANDY-FROIZ")
# function_rotulo(index_tandy)
# #v9 new (lo comento el 22/08/2018)
# iri_v12$Enseña <- gsub("TANDY-FROIZ", "TANDY", iri_v12$Enseña)

#new version: 22/08/2018
index_froiz_pleis_v12 <- which(pleis_v12$cadena == "GRUPO FROIZ")
function_cadena_pleis_v12(index_froiz_pleis_v12)
pleis_v12$ensena <- gsub("GRUPO FROIZ", "FROIZ", pleis_v12$ensena)
iri_v12$Enseña <- gsub("S. FROIZ", "FROIZ", iri_v12$Enseña)


#=======================================================================================================
#Gigante
index_gigante <- which(iri_v12$Rótulo == "GIGANTE")
#=======================================================================================================
#Tradys
index_tradys <- which(iri_v12$Rótulo == "TRADY'S")
function_rotulo(index_tradys)
iri_v12$Enseña <- gsub("TRADY'S", "TRADYS", iri_v12$Enseña)
#=======================================================================================================
#Comerco ---- PRUEBA
#pleis_v12$ensena <- gsub("COMERCO", "COVALCO-COALIMENT", pleis_v12$ensena)  #No modifica nada.
#=======================================================================================================
#Novavenda
index_novavenda <- which(iri_v12$Rótulo == "NOVAVENDA")
#=======================================================================================================
#Petit preu
index_petit_preu <- which(iri_v12$Rótulo == "PETIT PREU")
function_rotulo(index_petit_preu)
iri_v12$Enseña <- gsub("PETIT PREU", "PETIT-PREU", iri_v12$Enseña)
pleis_v12$ensena <- gsub("PETIT PREU", "PETIT-PREU", pleis_v12$ensena)
#=======================================================================================================
#Supermercados Piedra
index_prieda <- which(iri_v12$Rótulo == "SUPERMERCADOS PIEDRA")
function_rotulo(index_prieda)
iri_v12$Enseña <- gsub("SUPERMERCADOS PIEDRA", "SUPER PIEDRA", iri_v12$Enseña)
#=======================================================================================================
#JR Supermercats
index_jr_supermercats <- which(iri_v12$Rótulo == "J.R. SUPERMERCATS")   #J.R. Supermercats
function_rotulo(index_jr_supermercats)
iri_v12$Enseña <- gsub("J.R. SUPERMERCATS", "JR-SUPERMERCAT", iri_v12$Enseña)
pleis_v12$ensena <- gsub("JR SUPERMERCAT", "JR-SUPERMERCAT", pleis_v12$ensena)
#======================================================================================================
#Super Alba
index_super_alba <- which(iri_v12$Rótulo == "SUPER ALBA")
function_rotulo(index_super_alba)
iri_v12$Enseña <- gsub("SUPER ALBA", "SUPER-ALBA", iri_v12$Enseña)
pleis_v12$ensena <- gsub("SUPER ALBA", "SUPER-ALBA", pleis_v12$ensena)
#======================================================================================================
#Unico
index_unico <- which(iri_v12$Rótulo == "UNICO")
#======================================================================================================
#CONSUM
index_consum <- which(pleis_v12$cadena == "CONSUM, SDAD.COOP.")
function_replace_string_ensena(index_consum, "CONSUM-SC")
iri_v12$Enseña <- gsub("CONSUM SC", "CONSUM-SC", iri_v12$Enseña)
#======================================================================================================
#Alsara
index_alsara <- which(iri_v12$Rótulo == "ALSARA")
#======================================================================================================
#Coop-Consumo
index_coop_consumo <- which(iri_v12$Rótulo == "COOP. CONSUMO")
function_rotulo(index_coop_consumo)
iri_v12$Enseña <- gsub("COOP. CONSUMO", "COOP-CONSUMO", iri_v12$Enseña)
pleis_v12$ensena <- gsub("COOP.CONSUMO", "COOP-CONSUMO", pleis_v12$ensena)
#======================================================================================================
#Disara
index_disara <- which(iri_v12$Rótulo == "DISARA")
#======================================================================================================
#Super siete villas
index_siete_villas <- which(iri_v12$Rótulo == "SUPER SIETE VILLAS")
function_rotulo(index_siete_villas)
iri_v12$Enseña <- gsub("SUPER SIETE VILLAS", "SUPER-SIETE-VILLAS", iri_v12$Enseña)
pleis_v12$ensena <- gsub("SUPER 7 VILLAS", "SUPER-SIETE-VILLAS", pleis_v12$ensena)
#======================================================================================================
#Super Alcoop
index_alcoop <- which(iri_v12$Rótulo == "SUPER ALCOOP")
function_rotulo(index_alcoop)
iri_v12$Enseña <- gsub("SUPER ALCOOP", "SUPER-ALCOOP", iri_v12$Enseña)
pleis_v12$ensena <- gsub("SUPER ALCOOP", "SUPER-ALCOOP", pleis_v12$ensena)
#======================================================================================================
#Urbasa
index_urbasa <- which(iri_v12$Rótulo == "URBASA")
#======================================================================================================
#Santa Mª Magdalena
index_m_magdalena <- which(iri_v12$Rótulo == "SANTA MARIA MAGDALENA")
function_rotulo(index_m_magdalena)
iri_v12$Enseña <- gsub("SANTA MARIA MAGDALENA", "SANTA-MARIA-MAGDALENA", iri_v12$Enseña)
pleis_v12$ensena <- gsub("STA[.]M[*] MAGDALE", "SANTA-MARIA-MAGDALENA", pleis_v12$ensena)   
#======================================================================================================
#Super Cordoba
index_super_cordoba <- which(iri_v12$Rótulo == "SUPERSERVICIO CORDOBA")
function_rotulo(index_super_cordoba)
iri_v12$Enseña <- gsub("SUPERSERVICIO CORDOBA", "SUPERSERVICIO-CORDOBA", iri_v12$Enseña)
pleis_v12$ensena <- gsub("SUPER[.] CORDOBA", "SUPERSERVICIO-CORDOBA", pleis_v12$ensena)   
#======================================================================================================
#Super Navarrete
index_super_navarrete <- which(iri_v12$Rótulo == "SUPERMERCADO NAVARRETE")
function_rotulo(index_super_navarrete)
iri_v12$Enseña <- gsub("SUPERMERCADO NAVARRETE", "SUPERMERCADO-NAVARRETE", iri_v12$Enseña)
pleis_v12$ensena <- gsub("SUP[.]NAVARRETE", "SUPERMERCADO-NAVARRETE", pleis_v12$ensena)   
#======================================================================================================
#Super Bonarea
index_super_bonarea <- which(iri_v12$Rótulo == "SUPER BONAREA")
function_rotulo(index_super_bonarea)
iri_v12$Enseña <- gsub("SUPER BONAREA", "SUPER-BONAREA", iri_v12$Enseña)
pleis_v12$ensena <- gsub("SUPER BONAREA", "SUPER-BONAREA", pleis_v12$ensena) 
#======================================================================================================
#Coviran
index_coviran <- which(pleis_v12$cadena == "COVIRAN, S.C.A.")   
function_cadena_pleis_v12(index_coviran)
pleis_v12$ensena <- gsub("COVIRAN, S.C.A.", "COVIRAN", pleis_v12$ensena)   
#======================================================================================================
#Del Rio
index_del_rio <- which(iri_v12$Rótulo == "SUPERMERCADOS DEL RIO")
function_rotulo(index_del_rio)
iri_v12$Enseña <- gsub("SUPERMERCADOS DEL RIO", "SUPERMERCADOS-DEL-RIO", iri_v12$Enseña)
pleis_v12$ensena <- gsub("DEL RIO", "SUPERMERCADOS-DEL-RIO", pleis_v12$ensena)
#======================================================================================================
#Super Huracan
# index_huracan <- which(iri_v12$Rótulo == "SUPER HURACAN")
# function_rotulo(index_huracan)
# iri_v12$Enseña <- gsub("SUPER HURACAN", "SUPER-HURACAN", iri_v12$Enseña)
# pleis_v12

#======================================================================================================
#SPAR
index_spar <- which(iri_v12$Rótulo == "ANTIGÜA SPAR - POMARES" | iri_v12$Rótulo == "SPAR - DOMINGO" | iri_v12$Rótulo == "SPAR - EXCLUIB" |
                      iri_v12$Rótulo == "SPAR - FRAGADIS" | iri_v12$Rótulo == "SPAR - INSULAR" | 
                      iri_v12$Rótulo == "SPAR - LIDER AL" |
                      iri_v12$Rótulo == "SPAR - MIQUEL" | iri_v12$Rótulo == "SPAR - MOLDES" | iri_v12$Rótulo == "SPAR - OTROS" |
                      iri_v12$Rótulo == "SPAR - POMARES" | iri_v12$Rótulo == "SPAR - ROMEN" | iri_v12$Rótulo == "SPAR - UPPER" |
                      iri_v12$Rótulo == "SPAR - VALVI" | iri_v12$Rótulo == "SPAR EXPRESS - LIDER AL" | iri_v12$Rótulo == "SPAR EXPRESS - UPPER" |
                      iri_v12$Rótulo == "SPAR OTROS" | iri_v12$Rótulo == "SUPER SPAR - OTROS")

function_rotulo(index_spar)
iri_v12$Enseña <- gsub("ANTIGÜA SPAR - POMARES" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SPAR - DOMINGO" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SPAR - EXCLUIB" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SPAR - FRAGADIS" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SPAR - INSULAR" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SPAR - LIDER AL" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SPAR - MIQUEL" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SPAR - MOLDES" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SPAR - OTROS" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SPAR - POMARES" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SPAR - ROMEN" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SPAR - UPPER" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SPAR - VALVI" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SPAR EXPRESS - LIDER AL" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SPAR EXPRESS - UPPER" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SPAR OTROS" , "SPAR", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SUPER SPAR - OTROS" , "SPAR", iri_v12$Enseña)

pleis_v12$ensena <- gsub("SPAR EXPRESS" , "SPAR", pleis_v12$ensena)
pleis_v12$ensena <- gsub("SPAR TODO-TODO" , "SPAR", pleis_v12$ensena)
pleis_v12$ensena <- gsub("SUPER SPAR" , "SPAR", pleis_v12$ensena)
pleis_v12$ensena <- gsub("SUPERSPAR" , "SPAR", pleis_v12$ensena)
pleis_v12$ensena <- gsub("VALVI-EUROSPAR" , "SPAR", pleis_v12$ensena)

#====================================================================================================== 
#Deza
iri_v12$Enseña <- gsub("DEZA ALIMENTACION", "DEZA", iri_v12$Enseña)
#======================================================================================================  
#Binipreu
index_binipreu <- which(iri_v12$Rótulo == "BINIPREU MERCAT" | iri_v12$Rótulo == "BINIPREU")
function_rotulo(index_binipreu)   
iri_v12$Enseña <- gsub("BINIPREU MERCAT" , "BINIPREU", iri_v12$Enseña)
#======================================================================================================
#Disbo
index_disbo <- which(iri_v12$Rótulo == "DISBO")
#====================================================================================================== 
#Eco mora
index_ecomora <- which(pleis_v12$cadena == "ECO MORA, S.A.")
function_cadena_pleis_v12(index_ecomora)
pleis_v12$ensena <- gsub("ECO MORA, S.A.", "ECO-MORA", pleis_v12$ensena)

iri_v12$Enseña <- gsub("ECO MORA, S.A. \\(HIPER MANACOR\\)", "ECO-MORA", iri_v12$Enseña)  #Eco Mora, S.A. (Hiper Manacor)   #ECO MORA, S.A. (HIPER MANACOR)




#Alternativa para La Despensa, (obtenemos mejores resultados con el código de arriba):
# index_la_despensa <- which(iri_v12$Rótulo == "LA DESPENSA" | iri_v12$Rótulo == "LA DESPENSA EXPRESS")
# function_rotulo(index_la_despensa)
# iri_v12$Enseña <- gsub("LA DESPENSA", "LA-DESPENSA", iri_v12$Enseña)
# iri_v12$Enseña <- gsub("LA DESPENSA EXPRESS", "LA-DESPENSA", iri_v12$Enseña)
# pleis_v12$ensena <- gsub("LA DESPENSA", "LA-DESPENSA", pleis_v12$ensena)
# pleis_v12$ensena <- gsub("LA DESPENSA EX", "LA-DESPENSA", pleis_v12$ensena)

#Para los centros cuya enseña es La Despensa pero la cadena no es Eco Mora.
# index_san_rafael <- which(pleis_v12$cadena == "COOPERATIVA SAN RAFAEL S." & pleis_v12$ensena == "LA DESPENSA")
# index_despensa_castilla_mancha <- which(pleis_v12$cadena == "LA DESPENSA DE CASTILLA LA MANCHA, S.L." & pleis_v12$ensena == "LA DESPENSA")
# index_independiente <- which(pleis_v12$cadena == "INDEPENDIENTE" & pleis_v12$ensena == "LA DESPENSA")
#Lo mismo pero en una línea.
index_no_eco_mora <- which(pleis_v12$cadena != "ECO MORA, S.A." & pleis_v12$ensena == "LA DESPENSA")
function_la_despensa(index_no_eco_mora)
#======================================================================================================
#HUNOSA
index_hunosa <- which(iri_v12$Rótulo == "ECONOMATOS DE HUNOSA")
function_rotulo(index_hunosa)
pleis_v12$ensena <- gsub("ECO. HUNOSA", "HUNOSA", pleis_v12$ensena)
iri_v12$Enseña <- gsub("ECONOMATOS DE HUNOSA", "HUNOSA", iri_v12$Enseña)
#======================================================================================================
#Bip bip
pleis_v12$ensena <- gsub("BIP BIP", "BIP-BIP", pleis_v12$ensena)
index_bip_bip_ecos <- which(iri_v12$Rótulo == "BIP-BIP - ECOS")
function_rotulo(index_bip_bip_ecos)
iri_v12$Enseña <- gsub("BIP-BIP - ECOS", "BIP-BIP", iri_v12$Enseña)

index_bip_bip_garcia_romo <- which(iri_v12$Rótulo == "BIP-BIP - GARCIA ROMO")
function_rotulo(index_bip_bip_garcia_romo)
iri_v12$Enseña <- gsub("BIP-BIP - GARCIA ROMO", "BIP-BIP", iri_v12$Enseña)

index_bip_bip_moya <- which(iri_v12$Rótulo == "BIP-BIP MOYA")
function_rotulo(index_bip_bip_moya)
iri_v12$Enseña <- gsub("BIP-BIP MOYA", "BIP-BIP", iri_v12$Enseña)
#======================================================================================================
#Insular General Alimentaria -> INCLUIDO EN EL GRUPO DE SPAR
# index_iga_pleis_v12 <- which(pleis_v12$cadena == "INSULAR GENERAL ALIMENTAR")
# function_cadena_pleis_v12(index_iga_pleis_v12)
# pleis_v12$ensena <- gsub("INSULAR GENERAL ALIMENTAR", "IGA", pleis_v12$ensena)
# iri_v12$Enseña <- gsub("INSULAR GENERAL ALIMENTARIA, S.A.", "IGA", iri_v12$Enseña)
# # index_iga_iri_v12 <- which(iri_v12$Rótulo == "SPAR - INSULAR")
# # function_rotulo(index_iga_iri_v12)
# # iri_v12$Enseña <- 
# 
# 
# 
# 
#  d <- gsub("INSULAR GENERAL ALIMENTARIA, S.A.", "IGA", iri_v12$Enseña)
#  which(d == "IGA")
#  which(iri_v12$Enseña == "INSULAR GENERAL ALIMENTARIA, S.A.")   toupper("Insular General Alimentaria, S.A.")
# # 
# # grep("INSULAR GENERAL", iri_v12)
# # 
# # which(iri_v12$Rótulo == "SPAR - INSULAR")

#======================================================================================================
#J. Marquez
index_j_marquez <- which(iri_v12$Rótulo == "J. MARQUEZ")
function_rotulo(index_j_marquez)
iri_v12$Enseña <- gsub("J. MARQUEZ", "J-MARQUEZ", iri_v12$Enseña)
pleis_v12$ensena <- gsub("J.MARQUEZ", "J-MARQUEZ", pleis_v12$ensena)
#======================================================================================================
#Xaloc
index_xaloc <- which(iri_v12$Rótulo == "XALOC")
pleis_v12$ensena <- gsub("SUPER XALOC", "XALOC", pleis_v12$ensena)
#======================================================================================================
#Cyp sela
index_cyp_sela <- which(iri_v12$Rótulo == "CYP SELA")
#======================================================================================================
#La siesta
index_la_siesta <- which(iri_v12$Rótulo == "LA SIESTA")
#======================================================================================================
#Els Masos
index_els_masos <- which(iri_v12$Rótulo == "ELS MASOS")
function_rotulo(index_els_masos)
iri_v12$Enseña <- gsub("ELS MASOS", "MASOS", iri_v12$Enseña)
pleis_v12$ensena <- gsub("ELS MASOS", "MASOS", pleis_v12$ensena)
#======================================================================================================
#Playa Brava
index_playa_brava <- which(iri_v12$Rótulo == "PLAYA BRAVA")
#======================================================================================================
#El Golfet
index_el_golfet <- which(iri_v12$Rótulo == "EL GOLFET")
function_rotulo(index_el_golfet)
iri_v12$Enseña <- gsub("EL GOLFET", "GOLFET", iri_v12$Enseña)
pleis_v12$ensena <- gsub("EL GOLFET", "GOLFET", pleis_v12$ensena)
#======================================================================================================
#Calisol
index_calisol <- which(iri_v12$Rótulo == "CALISOL")
#======================================================================================================
#MAS Y MAS
index_mas_y_mas <- which(iri_v12$Rótulo == "MAS Y MAS - SORIANO" | iri_v12$Rótulo == "MAS Y MAS - LUIS RODRIGUEZ" | 
                           iri_v12$Rótulo == "MAS Y MAS - FORNES" | iri_v12$Rótulo == "MAS Y MAS - PIÑA")
function_rotulo(index_mas_y_mas)
iri_v12$Enseña <- gsub("MAS Y MAS - SORIANO", "MAS-Y-MAS", iri_v12$Enseña)
iri_v12$Enseña <- gsub("MAS Y MAS - LUIS RODRIGUEZ", "MAS-Y-MAS", iri_v12$Enseña)
iri_v12$Enseña <- gsub("MAS Y MAS - FORNES", "MAS-Y-MAS", iri_v12$Enseña)
iri_v12$Enseña <- gsub("MAS Y MAS - PIÑA", "MAS-Y-MAS", iri_v12$Enseña)
pleis_v12$ensena <- gsub("MAS Y MAS", "MAS-Y-MAS", pleis_v12$ensena)
#======================================================================================================
#D y D
index_d_y_d <- which(iri_v12$Rótulo == "D Y D")
function_rotulo(index_d_y_d)
iri_v12$Enseña <- gsub("D Y D", "D-Y-D", iri_v12$Enseña)
pleis_v12$ensena <- gsub("D Y D", "D-Y-D", pleis_v12$ensena)
#======================================================================================================
#Juma
index_juma <- which(iri_v12$Rótulo == "SUPERMERCADO JUMA")
function_rotulo(index_juma)
iri_v12$Enseña <- gsub("SUPERMERCADO JUMA", "JUMA", iri_v12$Enseña)
#======================================================================================================
#Leclerc
index_leclerc <- which(pleis_v12$ensena == "LECLERC EXPRESS")
function_leclerc(index_leclerc)
#======================================================================================================
#Lider Aliment
index_lider <- which(pleis_v12$cadena == "LIDER ALIMENT, S.A.")
function_cadena_pleis_v12(index_lider)
pleis_v12$ensena <- gsub("LIDER ALIMENT, S.A.", "LIDER-ALIMENT", pleis_v12$ensena)
iri_v12$Enseña <- gsub("LIDER ALIMENT", "LIDER-ALIMENT", iri_v12$Enseña)

#nv of 07_09_18
# index_lider_iri_v12 <- which(iri_v12$Enseña == "LIDER ALIMENT" & (iri_v12$Rótulo == 'AL LADO' | iri_v12$Rótulo == 'MIKRO' | iri_v12$Rótulo == 'SPAR-LIDER AL' | 
#                                                             iri_v12$Rótulo == 'SPAR EXPRESS-LIDER AL' | iri_v12$Rótulo == 'TANDY-LIDER AL' ))
# function_lider_iri_v12(index_lider_iri_v12)
# index_lider_pleis_v12 <- which(pleis_v12$cadena == "LIDER ALIMENT, S.A." & (pleis_v12$ensena == "AL LADO" | pleis_v12$ensena == "EUROCASH" | pleis_v12$ensena == "EUROSPAR" |
#                                                                       pleis_v12$ensena == "MIKRO" | pleis_v12$ensena == "SPAR" | pleis_v12$ensena == "SPAR EXPRESS" |
#                                                                       pleis_v12$ensena == "TANDY"))
# function_lider_pleis_v12(index_lider_pleis_v12)


#======================================================================================================
#Mikro
#En iri_v12, el rótulo Mikro solo pertenece a la cadena Lider Aliment, S.A.; en pleis_v12 la enseña Mikro pertenece
#a las cadenas Caro Ruiz, S.A., Independiente, Lider o Luis Piña.
#Para hacer la comparación con iri_v12, considero que todos los Mikro de pleis_v12 son de Lider.
#I comment the next lines on 07_09 and replace with the new system of lines 584.
index_mikro <- which(pleis_v12$ensena == "MIKRO" | pleis_v12$ensena == "MIKROSUR")
function_lider_pleis_v12(index_mikro)

#Otra posibilidad: considerar los supermercados no desde la enseña de iri_v12, sino desde el rótulo de iri_v12 y sus coincidencias
#con la enseña de pleis_v12. La posibilidad anterior ofrece más coincidencias.
# pleis_v12$ensena <- gsub("MIKROSUR", "MIKRO", pleis_v12$ensena)
# index_mikro <- which(iri_v12$Rótulo == "MIKRO")
#======================================================================================================
#======================================================================================================
#Minymas
index_minymas <- which(iri_v12$Rótulo == "MINYMAS - PIÑA" | iri_v12$Rótulo == "MINYMAS - LUIS RODRIGUEZ")
function_rotulo(index_minymas)
iri_v12$Enseña <- gsub("MINYMAS - PIÑA", "MINYMAS", iri_v12$Enseña)
iri_v12$Enseña <- gsub("MINYMAS - LUIS RODRIGUEZ", "MINYMAS", iri_v12$Enseña)
#======================================================================================================
#Tandy que no pertenecen a Lider Aliment ni a Froiz
index_tandy_otros <- which(iri_v12$Rótulo == "TANDY - BALDEVA" | iri_v12$Rótulo == "TANDY OTROS") # | iri_v12$Rótulo == "TANDY-FROIZ")
function_rotulo(index_tandy_otros)
iri_v12$Enseña <- gsub("TANDY - BALDEVA", "TANDY", iri_v12$Enseña)
iri_v12$Enseña <- gsub("TANDY OTROS", "TANDY", iri_v12$Enseña)
# iri_v12$Enseña <- gsub("TANDY-FROIZ", "TANDY", iri_v12$Enseña)
pleis_v12$cadena <- gsub("TANDI", "TANDY", pleis_v12$cadena)
#======================================================================================================
#RELINQUE
pleis_v12$ensena <- gsub("AUTOS RELINQUE", "RELINQUE", pleis_v12$ensena)
index_relinque <- which(iri_v12$Rótulo == "AUTOSERVICIO RELINQUE")
function_rotulo(index_relinque)
iri_v12$Enseña <- gsub("AUTOSERVICIO RELINQUE", "RELINQUE", iri_v12$Enseña)
#======================================================================================================
#MASKOMO
pleis_v12$ensena <- gsub("MASCERKA", "MASKOM", pleis_v12$ensena)
pleis_v12$ensena <- gsub("MASKOMPRA", "MASKOM", pleis_v12$ensena)
index_maskomo <- which(iri_v12$Rótulo == "MASKOM" | iri_v12$Rótulo == "SUPERMERCADO CAYETANO")
function_rotulo(index_maskomo)
iri_v12$Enseña <- gsub("SUPERMERCADO CAYETANO", "MASKOM", iri_v12$Enseña)
#======================================================================================================
#Albeyco
pleis_v12$ensena <- gsub("PROMO-CASH", "ALBEYCO", pleis_v12$ensena)
index_albeyco <- which(iri_v12$Rótulo == "MI SUPER ALBEYCO")
function_rotulo(index_albeyco)
iri_v12$Enseña <- gsub("MI SUPER ALBEYCO", "ALBEYCO", iri_v12$Enseña)
#======================================================================================================
#Proxim y Suma, (de Miquel)
index_proxim <- which(iri_v12$Rótulo == "PROXIM")
index_suma <- which(iri_v12$Rótulo == "SUMA")
index_suma_express <- which(iri_v12$Rótulo == "SUMA EXPRESS")
function_rotulo(index_suma_express)
iri_v12$Enseña <- gsub("SUMA EXPRESS", "SUMA", iri_v12$Enseña)
#======================================================================================================
#Michelangelo
index_michelangelo <- which(iri_v12$Rótulo == "MICHELANGELO")
#======================================================================================================
#Eurocop
index_eurocop <- which(iri_v12$Rótulo == "EUROCOP")
#======================================================================================================
#Preti
index_preti <- which(iri_v12$Rótulo == "PRETI")
#======================================================================================================
#Musgrave
##Dialprix
index_dialprix <- which(iri_v12$Rótulo == "DIALPRIX")
#Dicost
index_dicost <- which(iri_v12$Rótulo == "DICOST")
#Super Valu
index_super_valu <- which(iri_v12$Rótulo == "SUPER VALU")
function_rotulo(index_super_valu)
iri_v12$Enseña <- gsub("SUPER VALU", "SUPER-VALU", iri_v12$Enseña)
pleis_v12$ensena <- gsub("SUPER VALU", "SUPER-VALU", pleis_v12$ensena)
#======================================================================================================
#Super Dumbo
index_super_dumbo <- which(iri_v12$Rótulo == "SUPER DUMBO")
function_rotulo(index_super_dumbo)
iri_v12$Enseña <- gsub("SUPER DUMBO", "SUPER-DUMBO", iri_v12$Enseña)
pleis_v12$ensena <- gsub("SUPER DUMBO", "SUPER-DUMBO", pleis_v12$ensena)
#======================================================================================================
#La Kompra
index_la_kompra <- which(iri_v12$Rótulo == "LA KOMPRA")
function_rotulo(index_la_kompra)
iri_v12$Enseña <- gsub("LA KOMPRA", "LA-KOMPRA", iri_v12$Enseña)
pleis_v12$ensena <- gsub("LA KOMPRA", "LA-KOMPRA", pleis_v12$ensena)
#======================================================================================================
#Salinas
index_salinas <- which(iri_v12$Rótulo == "SALINAS")
#======================================================================================================
#Pepe La sal
index_pepe_la_sal <- which(iri_v12$Rótulo == "PEPE LA SAL" | iri_v12$Rótulo == "PEPE LA SAL (DISCOUNT)")  
function_rotulo(index_pepe_la_sal)
iri_v12$Enseña <- gsub("PEPE LA SAL [(]DISCOUNT[)]", "PEPE-LA-SAL", iri_v12$Enseña)
iri_v12$Enseña <- gsub("PEPE LA SAL", "PEPE-LA-SAL", iri_v12$Enseña)
pleis_v12$ensena <- gsub("PEPE LA SAL", "PEPE-LA-SAL", pleis_v12$ensena)
#======================================================================================================
#Super Patri
index_super_patri <- which(iri_v12$Rótulo == "SUPER PATRI")
function_rotulo(index_super_patri)
iri_v12$Enseña <- gsub("SUPER PATRI", "SUPER-PATRI", iri_v12$Enseña)
pleis_v12$ensena <- gsub("SUPER PATRI", "SUPER-PATRI", pleis_v12$ensena)
pleis_v12$ensena <- gsub("CASH PATRI", "SUPER-PATRI", pleis_v12$ensena)
#======================================================================================================
#Super Montgo
index_super_montgo <- which(iri_v12$Rótulo == "SUPER MONTGO")
function_rotulo(index_super_montgo)
iri_v12$Enseña <- gsub("SUPER MONTGO", "SUPER-MONTGO", iri_v12$Enseña)
pleis_v12$ensena <- gsub("SUPER MONTGO", "SUPER-MONTGO", pleis_v12$ensena)
#======================================================================================================
#Super El Pinell
index_el_pinell <- which(iri_v12$Rótulo == "SUPER EL PINELL")
function_rotulo(index_el_pinell)
iri_v12$Enseña <- gsub("SUPER EL PINELL", "SUPER-EL-PINELL", iri_v12$Enseña)
pleis_v12$ensena <- gsub("SUPERMERCAT EL PINELL", "SUPER-EL-PINELL", pleis_v12$ensena)
#======================================================================================================
#Ruiz Galan
index_ruiz_galan <- which(iri_v12$Rótulo == "SUPERMERCADOS RUIZ GALAN")
function_rotulo(index_ruiz_galan)
iri_v12$Enseña <- gsub("SUPERMERCADOS RUIZ GALAN", "SUPER-RUIZ-GALAN", iri_v12$Enseña)
pleis_v12$ensena <- gsub("S.RUIZ GALAN", "SUPER-RUIZ-GALAN", pleis_v12$ensena)
#======================================================================================================
#Saavedra hermanos
index_saavedra_hermanos <- which(iri_v12$Rótulo == "SAAVEDRA HERMANOS")
function_rotulo(index_saavedra_hermanos)

iri_v12$Enseña <- gsub("SAAVEDRA HERMANOS", "SAAVEDRA-HERMANOS", iri_v12$Enseña)
pleis_v12$ensena <- gsub("SAAVEDRA HNOS.", "SAAVEDRA-HERMANOS", pleis_v12$ensena)
pleis_v12$ensena <- gsub("SUP. SAAVEDRA", "SAAVEDRA-HERMANOS", pleis_v12$ensena)
#======================================================================================================
#Alvimar
index_alvimar <- which(iri_v12$Rótulo == "ALVIMAR")
#======================================================================================================
#Lupa
index_lupa <- which(iri_v12$Rótulo == "LUPA")
#======================================================================================================
#Telco
index_telco <- which(iri_v12$Rótulo == "TELCO")
#======================================================================================================
#Tifer
index_tifer <- which(iri_v12$Rótulo == "TIFER")
#======================================================================================================
#Hermanos Martin
# iri_v12$Enseña <- gsub("GRUPO HERMANOS MARTIN", "HERMANOS-MARTIN", iri_v12$Enseña)
# pleis_v12$ensena <- gsub("HNOS[.] MARTIN", "HERMANOS-MARTIN", pleis_v12$ensena)

#Grupo Hermanos Martin
index_hermanos_martin_pleis_v12 <- which(pleis_v12$cadena == "GRUPO HERMANOS MARTIN, S.")
function_cadena_pleis_v12(index_hermanos_martin_pleis_v12)
pleis_v12$ensena <- gsub("GRUPO HERMANOS MARTIN, S.", "HERMANOS-MARTIN", pleis_v12$ensena)
iri_v12$Enseña <- gsub("GRUPO HERMANOS MARTIN", "HERMANOS-MARTIN", iri_v12$Enseña)
#======================================================================================================
#Los Duendes
# index_los_duendes <- which(iri_v12$Rótulo == "LOS DUENDES")
# function_rotulo(index_los_duendes)
# iri_v12$Enseña <- gsub("LOS DUENDES", "LOS-DUENDES", iri_v12$Enseña)
# pleis_v12$ensena <- gsub("LOS DUENDES", "LOS-DUENDES", pleis_v12$ensena)
# #======================================================================================================
# #Super Carmela
# index_super_carmela <- which(iri_v12$Rótulo == "SUPER CARMELA")
# function_rotulo(index_super_carmela)
# iri_v12$Enseña <- gsub("SUPER CARMELA", "SUPER-CARMELA", iri_v12$Enseña)
# pleis_v12$ensena <- gsub("SUPER CARMELA", "SUPER-CARMELA", pleis_v12$ensena)
# #======================================================================================================
# #El Trebol
# index_el_trebol <- which(iri_v12$Rótulo == "SUPER TREBOL")
# function_rotulo(index_el_trebol)
# iri_v12$Enseña <- gsub("SUPER TREBOL", "SUPER-TREBOL", iri_v12$Enseña)
# pleis_v12$ensena <- gsub("SUPER TREBOL", "SUPER-TREBOL", pleis_v12$ensena)
#======================================================================================================
#Super Carmela, (we use the generic name instead the small names because there are centers which own to the enseña but don't match between iri_v12 and pleis_v12)
iri_v12$Enseña <- gsub("SUPER CARMELA, S.L.", "SUPER-CARMELA", iri_v12$Enseña)  #Super Carmela, S.L.
index_super_carmela_pleis_v12 <- which(pleis_v12$cadena == "SUPER CARMELA, S.L.")
function_cadena_pleis_v12(index_super_carmela_pleis_v12)
pleis_v12$ensena <- gsub("SUPER CARMELA, S.L.", "SUPER-CARMELA", pleis_v12$ensena)
#======================================================================================================
#Super G
pleis_v12$ensena <- gsub("SUPER G", "SUPER-G", pleis_v12$ensena)
index_super_g <- which(iri_v12$Rótulo == "SUPER \"G\"")
function_rotulo(index_super_g)
iri_v12$Enseña <- gsub("SUPER \"G\"", "SUPER-G", iri_v12$Enseña)
#======================================================================================================
#Casa Evaristo
pleis_v12$ensena <- gsub("CASA EVARISTO", "CASA-EVARISTO", pleis_v12$ensena)
index_casa_evaristo <- which(iri_v12$Rótulo == "CASA EVARISTO")
function_rotulo(index_casa_evaristo)
iri_v12$Enseña <- gsub("CASA EVARISTO", "CASA-EVARISTO", iri_v12$Enseña)
#======================================================================================================
#Super Tallo
pleis_v12$ensena <- gsub("SUPER TALLO", "SUPER-TALLO", pleis_v12$ensena)
index_super_tallo <- which(iri_v12$Rótulo == "SUPER TALLO")
function_rotulo(index_super_tallo)
iri_v12$Enseña <- gsub("SUPER TALLO", "SUPER-TALLO", iri_v12$Enseña)
#======================================================================================================
#Super Arcos
pleis_v12$ensena <- gsub("SUPER ARCOS", "SUPER-ARCOS", pleis_v12$ensena)
index_super_arcos <- which(iri_v12$Rótulo == "SUPER ARCOS")
function_rotulo(index_super_arcos)
iri_v12$Enseña <- gsub("SUPER ARCOS", "SUPER-ARCOS", iri_v12$Enseña)
#======================================================================================================
#Super Enrique
pleis_v12$ensena <- gsub("SAN ENRIQUE", "SAN-ENRIQUE", pleis_v12$ensena)
index_super_enrique <- which(iri_v12$Rótulo == "SAN ENRIQUE")
function_rotulo(index_super_enrique)
iri_v12$Enseña <- gsub("SAN ENRIQUE", "SAN-ENRIQUE", iri_v12$Enseña)
#======================================================================================================
#Super Montserrat
index_super_montserrat_pleis_v12 <- which(pleis_v12$cadena == "SUPER MONTSERRAT, S.L.")
function_cadena_pleis_v12(index_super_montserrat_pleis_v12)
pleis_v12$ensena <- gsub("SUPER MONTSERRAT, S.L.", "SUPER-MONTSERRAT", pleis_v12$ensena)
index_super_montserrat_iri_v12 <- which(iri_v12$Rótulo == "HIPER MONTSERRAT" | iri_v12$Rótulo == "SUPER MONTSERR" | iri_v12$Rótulo == "SUPER MONTSERRAT")
function_rotulo(index_super_montserrat_iri_v12)
iri_v12$Enseña <- gsub("HIPER MONTSERRAT", "SUPER-MONTSERRAT", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SUPER MONTSERR", "SUPER-MONTSERRAT", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SUPER MONTSERRAT", "SUPER-MONTSERRAT", iri_v12$Enseña)
#======================================================================================================
#Superalca
index_superalca_iri_v12 <- which(iri_v12$Rótulo == "SUPERALCA")
index_superalca_pleis_v12 <- which(pleis_v12$cadena == "SUPERALCA, S.L.")
function_cadena_pleis_v12(index_superalca_pleis_v12)
pleis_v12$ensena <- gsub("SUPERALCA, S.L.", "SUPERALCA", pleis_v12$ensena)
#======================================================================================================
#Sorli Discau
pleis_v12$ensena <- gsub("SORLI DISCAU", "SORLI-DISCAU", pleis_v12$ensena)
index_sorli_discau <- which(iri_v12$Rótulo == "SORLI-DISCAU")
#======================================================================================================
#Baly
index_baly <- which(iri_v12$Rótulo == "SUPERMERCADO BALY")
function_rotulo(index_baly)
iri_v12$Enseña <- gsub("SUPERMERCADO BALY", "BALY", iri_v12$Enseña)
#======================================================================================================
#Costa Blanca
index_costa_blanca <- which(iri_v12$Rótulo == "SUPERMERCADOS COSTA BLANCA")
function_rotulo(index_costa_blanca)
iri_v12$Enseña <- gsub("SUPERMERCADOS COSTA BLANCA", "COSTA BLANCA", iri_v12$Enseña)
#======================================================================================================
#Egea
pleis_v12$ensena <- gsub("SUPER VIA-EGEA", "EGEA", pleis_v12$ensena)
pleis_v12$ensena <- gsub("SUPER EGEA", "EGEA", pleis_v12$ensena)
index_egea <- which(iri_v12$Rótulo == "ALIMENTACION EGEA")
function_rotulo(index_egea)
iri_v12$Enseña <- gsub("ALIMENTACION EGEA", "EGEA", iri_v12$Enseña)
#======================================================================================================
#Supermercados Cadiz
index_super_cadiz_pleis_v12 <- which(pleis_v12$cadena == "SUPERMERCADOS CADIZ, S.L.")
function_cadena_pleis_v12(index_super_cadiz_pleis_v12)
pleis_v12$ensena <- gsub("SUPERMERCADOS CADIZ, S.L.", "SUPERMERCADOS-CADIZ", pleis_v12$ensena)
iri_v12$Enseña <- gsub("SUPERMERCADOS CADIZ, S.L.", "SUPERMERCADOS-CADIZ", iri_v12$Enseña)   
#======================================================================================================
#Vismac
index_vismac <- which(iri_v12$Rótulo == "VISMAC")
#======================================================================================================
#Codi
index_codi <- which(iri_v12$Rótulo == "SUPERMERCADOS CODI" | iri_v12$Rótulo == "CODI SUPERMERCADOS")
function_rotulo(index_codi)
iri_v12$Enseña <- gsub("SUPERMERCADOS CODI", "CODI", iri_v12$Enseña)
iri_v12$Enseña <- gsub("CODI SUPERMERCADOS", "CODI", iri_v12$Enseña)
#======================================================================================================
#Dani
index_dani_pleis_v12 <- which(pleis_v12$cadena == "SUPERMERCADOS DANI, S.A.")
function_cadena_pleis_v12(index_dani_pleis_v12)
pleis_v12$ensena <- gsub("SUPERMERCADOS DANI, S.A.", "DANI", pleis_v12$ensena)
index_dani_iri_v12 <- which(iri_v12$Rótulo == "DANI")
#======================================================================================================
#Super Hiber
pleis_v12$ensena <- gsub("SUPER HIBER", "SUPER-HIBER", pleis_v12$ensena)
iri_v12$Enseña <- gsub("SUPERMERCADOS HIBER", "SUPER-HIBER", iri_v12$Enseña)
#======================================================================================================
#La Salve
index_la_salve_pleis_v12 <- which(pleis_v12$cadena == "SUPERMERCADOS LA SALVE")
function_cadena_pleis_v12(index_la_salve_pleis_v12)
pleis_v12$ensena <- gsub("SUPERMERCADOS LA SALVE", "SUPERMERCADOS-LA-SALVE", pleis_v12$ensena)
index_la_salve_iri_v12 <- which(iri_v12$Rótulo == "SUPERMERCADOS LA SALVE")
function_rotulo(index_la_salve_iri_v12)
iri_v12$Enseña <- gsub("SUPERMERCADOS LA SALVE", "SUPERMERCADOS-LA-SALVE", iri_v12$Enseña)
#======================================================================================================
#Los Alpes
index_los_alpes <- which(iri_v12$Rótulo == "SUPERMERCADOS LOS ALPES")
function_rotulo(index_los_alpes)
iri_v12$Enseña <- gsub("SUPERMERCADOS LOS ALPES", "LOS ALPES", iri_v12$Enseña)
#======================================================================================================
#Ros
pleis_v12$ensena <- gsub("SUPER ROS", "SUPER-ROS", pleis_v12$ensena)
# index_prueba <- which(iri_v12$Rótulo == "SUPERMERCADO ROS (ANGELINO ROS) (A)")
index_ros <- which(iri_v12$Rótulo == "SUPERMERCADOS ROS, S.L." | iri_v12$Rótulo == "SUPERMERCADO ROS (ANGELINO ROS) (A)")  #Supermercado Ros (Angelino Ros) (A)
function_rotulo(index_ros)
iri_v12$Enseña <- gsub("SUPERMERCADOS ROS, S.L.", "SUPER-ROS", iri_v12$Enseña)
iri_v12$Enseña <- gsub("SUPERMERCADO ROS [(]ANGELINO ROS[)] [(]A[)]", "SUPER-ROS", iri_v12$Enseña)
#======================================================================================================
#Sanchez Romero
index_sanchez_romero <- which(iri_v12$Rótulo == "SANCHEZ ROMERO")
function_rotulo(index_sanchez_romero)
iri_v12$Enseña <- gsub("SANCHEZ ROMERO", "SANCHEZ-ROMERO", iri_v12$Enseña)
pleis_v12$ensena <- gsub("SANCHEZ ROMERO", "SANCHEZ-ROMERO", pleis_v12$ensena)
#======================================================================================================
#Super Tantiña
pleis_v12$ensena <- gsub("SUPER TANTIÑA", "SUPER-TANTIÑA", pleis_v12$ensena)
index_super_tantina <- which(iri_v12$Rótulo == "SUPERMERCADOS TANTIÑA")
function_rotulo(index_super_tantina)
iri_v12$Enseña <- gsub("SUPERMERCADOS TANTIÑA", "SUPER-TANTIÑA", iri_v12$Enseña)
#======================================================================================================
#Super Touriño
pleis_v12$ensena <- gsub("SUPER TOURIÑO", "SUPER-TOURIÑO", pleis_v12$ensena)
index_super_tourino <- which(iri_v12$Rótulo == "SUPERMERCADO TOURIÑO")
function_rotulo(index_super_tourino)
iri_v12$Enseña <- gsub("SUPERMERCADO TOURIÑO", "SUPER-TOURIÑO", iri_v12$Enseña)
#======================================================================================================
#Supermertcats Llobet
index_llobet_pleis_v12 <- which(pleis_v12$cadena == "SUPERMERCATS LLOBET, S.A.")
function_cadena_pleis_v12(index_llobet_pleis_v12)
pleis_v12$ensena <- gsub("SUPERMERCATS LLOBET, S.A.", "SUPER-LLOBET", pleis_v12$ensena)
iri_v12$Enseña <- gsub("SUPERMERCATS LLOBET, S.A.", "SUPER-LLOBET", iri_v12$Enseña)
#======================================================================================================
#Supermas
index_supermas <- which(iri_v12$Rótulo == "SUPERMERCADOS SUPERMAS")
function_rotulo(index_supermas)
iri_v12$Enseña <- gsub("SUPERMERCADOS SUPERMAS", "SUPERMAS", iri_v12$Enseña)
#======================================================================================================
#Superservis
index_superservis <- which(iri_v12$Rótulo == "SUPERSERVIS")
#======================================================================================================
#Supersol
index_supersol <- which(iri_v12$Rótulo == "SUPERSOL")
#======================================================================================================
#Supertauro
pleis_v12$ensena <- gsub("SUPER TAURO", "SUPER-TAURO", pleis_v12$ensena)
index_super_tauro <- which(iri_v12$Rótulo == "SUPER TAURO")
function_rotulo(index_super_tauro)
iri_v12$Enseña <- gsub("SUPER TAURO", "SUPER-TAURO", iri_v12$Enseña)
#======================================================================================================
#Supermercats Pujol
index_pujol_pleis_v12 <- which(pleis_v12$cadena == "SUPSA SUPERMERCATS PUJOL")
function_cadena_pleis_v12(index_pujol_pleis_v12)
pleis_v12$ensena <- gsub("SUPSA SUPERMERCATS PUJOL", "PUJOL", pleis_v12$ensena)
iri_v12$Enseña <- gsub("SUPERMERCADOS PUJOL", "PUJOL", iri_v12$Enseña)
#======================================================================================================
#Sutega
index_sutega <- which(pleis_v12$cadena == "SUTEGA, S.L.")
function_cadena_pleis_v12(index_sutega)
#======================================================================================================
#Alvica
pleis_v12$ensena <- gsub("SUPER ALVICA", "ALVICA", pleis_v12$ensena)
index_alvica <- which(iri_v12$Rótulo == "SUPERMERCADOS ALVICA")
function_rotulo(index_alvica)
iri_v12$Enseña <- gsub("SUPERMERCADOS ALVICA", "ALVICA", iri_v12$Enseña)
#======================================================================================================
#Udaco
# index_udaco <- which(iri_v12$Rótulo == "UDACO")
#======================================================================================================


#Functions to replace the ensena names that aren't in iri_v12.
#Alternative function to increase the performance.
# index_gama_unide <- which(pleis_v12$ensena == "GAMA" & pleis_v12$cadena == "UNIDE, S.COOP.") #To replace the function change_some_fields()   
index_gama_unide <- which(pleis_v12$ensena == "GAMA") 
index_giro_unide <- which(pleis_v12$ensena == "AUTOS.GIRO")
index_cash_unide <- which(pleis_v12$ensena == "CASH UNIDE")
index_maxcoop_unide <- which(pleis_v12$ensena == "MAXCOOP")
index_ana_unide <- which(pleis_v12$ensena == "SUPER ANA")
index_udaco <- which(pleis_v12$ensena == "UDACO")

indexes_unide <- list(index_gama_unide, index_giro_unide, index_cash_unide, index_maxcoop_unide, 
                      index_ana_unide, index_udaco
)
lapply(indexes_unide, function_unide)

# function_unide(index_gama_unide)
#=====================================================================================================
#Super BM
pleis_v12$ensena <- gsub("SUPER BM-GELSA","UVESCO",pleis_v12$ensena)
pleis_v12$ensena <- gsub("SUPER BM","UVESCO",pleis_v12$ensena) #Lo que estaba


#Con esto obtenemos peores resultados.
# pleis_v12$ensena <- gsub("SUPER BM", "SUPER-BM", pleis_v12$ensena)
# index_super_bm <- which(iri_v12$Rótulo == "SUPER BM" )
# function_rotulo(index_super_bm)
# iri_v12$Enseña <- gsub("SUPER BM", "SUPER-BM", iri_v12$Enseña)

#Hacer como en UNIDE.
#=====================================================================================================
#Ercoreca
index_ercoreca <- which(iri_v12$Rótulo == "ERCORECA")
#=====================================================================================================
#Netto
pleis_v12$ensena <- gsub("NETTO S-20", "NETTO", pleis_v12$ensena)
index_netto <- which(iri_v12$Rótulo == "NETTO")
#=====================================================================================================
#Super Amara
pleis_v12$ensena <- gsub("SUPER AMARA", "SUPER-AMARA", pleis_v12$ensena)
index_super_amara <- which(iri_v12$Rótulo == "SUPER AMARA")
function_rotulo(index_super_amara)
iri_v12$Enseña <- gsub("SUPER AMARA", "SUPER-AMARA", iri_v12$Enseña)
#======================================================================================================
#Ugari
index_ugari <- which(iri_v12$Rótulo == "UGARI")
#======================================================================================================
#Berriak
index_berriak <- which(iri_v12$Rótulo == "BERRIAK SUPERMERKATUA")
function_rotulo(index_berriak)
iri_v12$Enseña <- gsub("BERRIAK SUPERMERKATUA", "BERRIAK", iri_v12$Enseña)
#======================================================================================================
#Valvi
index_valvi <- which(iri_v12$Rótulo == "VALVI SUPERMERCATS")
function_rotulo(index_valvi)
iri_v12$Enseña <- gsub("VALVI SUPERMERCATS", "VALVI", iri_v12$Enseña)
#======================================================================================================
#BON PREU
index_bon_preu_pleis_v12 <- which(pleis_v12$cadena == "BON PREU, S.A.")
function_cadena_pleis_v12(index_bon_preu_pleis_v12)
pleis_v12$ensena <- gsub("BON PREU, S.A.", "BON-PREU", pleis_v12$ensena)
iri_v12$Enseña <- gsub("GRUPO BON PREU", "BON-PREU", iri_v12$Enseña)
#======================================================================================================
#Hiper Manacor
index_hiper_manacor_pleis_v12 <- which(pleis_v12$cadena == "HIPER MANACOR, S.A.")
function_cadena_pleis_v12(index_hiper_manacor_pleis_v12)
pleis_v12$ensena <- gsub("HIPER MANACOR, S.A.", "HIPER-MANACOR", pleis_v12$ensena)
iri_v12$Enseña <- gsub("HIPER MANACOR, S.A.", "HIPER-MANACOR", iri_v12$Enseña)
#======================================================================================================
#Family Cash
index_family_cash <- which(iri_v12$Rótulo == "FAMILY CASH")
function_rotulo(index_family_cash)
iri_v12$Enseña <- gsub("FAMILY CASH", "FAMILY_CASH", iri_v12$Enseña)
pleis_v12$ensena <- gsub("FAMILYCASH", "FAMILY_CASH", pleis_v12$ensena)
#======================================================================================================
#Ignacio de las Cuevas
index_ignacio_cuevas_pleis_v12 <- which(pleis_v12$cadena == "IGNACIO DE LAS CUEVAS, S.")
function_cadena_pleis_v12(index_ignacio_cuevas_pleis_v12)
pleis_v12$ensena <- gsub("IGNACIO DE LAS CUEVAS, S.", "IGNACIO-CUEVAS", pleis_v12$ensena)
iri_v12$Enseña <- gsub("IGNACIO DE LAS CUEVAS, S.A.", "IGNACIO-CUEVAS", iri_v12$Enseña)
#======================================================================================================
#Sangui
index_sangui <- which(iri_v12$Rótulo == "SANGUI")
#======================================================================================================
#Jespac
index_jespac <- which(iri_v12$Rótulo == "JESPAC")
#======================================================================================================
#Aldi
# index_aldi <- which(iri_v12$Rótulo == "ALDI") It is not neccessary because iri_v12 has Enseña=Aldi
#======================================================================================================
#CONDIS
pleis_v12$ensena <- gsub("CONDIS EXPRESS","CONDIS",pleis_v12$ensena)
pleis_v12$ensena <- gsub("DISTOP","CONDIS",pleis_v12$ensena)
#======================================================================================================
#ESTABLECIMIENTOS PLAZA
pleis_v12$ensena <- gsub("SUP. PLAZA", "ESTABLECIMIENTOS PLAZA", pleis_v12$ensena)
#======================================================================================================
#SUPER MOR
index_mor <- which(iri_v12$Rótulo == "SUPERMERCADOS ECO-MOR")
function_rotulo(index_mor)
iri_v12$Enseña <- gsub("SUPERMERCADOS ECO-MOR","SUPER-MOR", iri_v12$Enseña)
#======================================================================================================
#UPPER
index_upper_pleis_v12 <- which(pleis_v12$cadena == "GRUPO UPPER, SOCIEDAD COO" & pleis_v12$ensena == "EUROSPAR")
function_replace_string_ensena(index_upper_pleis_V12, "SPAR") 
pleis_v12$ensena <- gsub("UPPER","SPAR",pleis_v12$ensena)
#======================================================================================================
#HIPER USERA
iri_v12$Enseña <- gsub("HIPER USERA", "HIPER-USERA", iri_v12$Enseña)
pleis_v12$ensena <- gsub("HIPER USERA", "HIPER-USERA", pleis_v12$ensena)
#======================================================================================================




#======================================================================================================
#List of indexes for function_rotulo:
indexes_rotulo_iri_v12 <- list(
  index_franquicia_eroski,
  index_claudio, index_arbol, index_ahorraves, index_salinas, index_pincha_precios,
  index_komo_komo, index_la_compra, index_darvi, index_iberplus, index_la_lonja, #index_maxcoop,
  index_don_market,
  index_cash_lepe, index_gigante, index_novavenda, index_unico, #index_petit_preu #, index_coaliment, index_coaliment_compra_saludable
  index_alsara, index_disara, index_urbasa, index_disbo, index_xaloc, index_cyp_sela, index_la_siesta, index_playa_brava,
  index_calisol, index_proxim, index_suma, index_michelangelo, index_eurocop, index_preti, index_dialprix, index_dicost,
  index_salinas, index_alvimar, index_lupa, index_telco, index_tifer, index_superalca_iri_v12, index_sorli_discau,
  index_vismac, index_dani_iri_v12, index_superservis, index_supersol, #index_udaco#, index_gama
  index_ercoreca, index_netto, index_ugari, index_sangui, index_jespac#, index_aldi
)
lapply(indexes_rotulo_iri_v12,function_rotulo) #It execute function_rotulo() over each element of list "indexes".

#=======================================================================================================



#We are going to remove the words "CENTER" and "SUPERMERCADOS" from the pleis_v12/ensena and iri_v12/Enseña, 
#(this happens at least with Eroski and Carrefour):
iri_v12$Enseña <- gsub("[[:space:]]+[CENTER|SUPERMERCADOS]+$","",iri_v12$Enseña)
#iri_v12$Enseña <- gsub("^[A-Z]+[[:space:]]+DIA","DIA",iri_v12$Enseña) #We remove MAXI from MAXI DIA in iri_v12/Enseña.


#We replace several ensenas to match pleis_v12 with iri_v12, (they are the same concept in both of them).
pleis_v12$ensena <- gsub("SIMPLY MARKET","SABECO",pleis_v12$ensena) #=================================







#==========================================================================Changes in iri_v12's Addresses======================================================================


#We replace the tildes and dieresis in iri_v12/Direccion1:
iri_v12$Direccion1 <- gsub("Á", "A", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("É", "E", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("Í", "I", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("Ó", "O", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("Ú", "U", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("Ü", "U", iri_v12$Direccion1)

#=======================================================================================================================================

iri_v12$Direccion1 <- gsub("^AV+[[:space:]]", "", iri_v12$Direccion1)     
iri_v12$Direccion1 <- gsub("^AV[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AV[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AVD+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AVD[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AVD[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AVDA[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("AVDA[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AVDA,+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AVDA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AVDA", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AVENIDA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AVINGUDA+[[:space:]]", "", iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^ANT+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^ANT[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^ANT[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^ANTIGUA+[[:space:]]", "", iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^ARR+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^ARR[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^ARR[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^ARRAVAL+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^ARRABAL+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RAVAL+[[:space:]]", "", iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^AUT+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AUT[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AUT[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AUTOPISTA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AUTOVIA+[[:space:]]", "", iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^BARRIO+[[:space:]]", "", iri_v12$Direccion1)   
iri_v12$Direccion1 <- gsub("^BARRIAL+[[:space:]]", "", iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^BARRIS+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BARRO+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BAR[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BAR[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BA[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BA[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^B[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^B[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BDA[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BDA[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BD[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BD[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BD+[[:space:]]", "", iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("^BARRIADA+[[:space:]]", "", iri_v12$Direccion1)

mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^BOULEVARD+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BOULEVAR+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^BULEVARD+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BULEVAR+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BOULE+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BOULE[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BOULE[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BO[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BO[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<BO\\>+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BU[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^BU[.]", "", iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^C[.]C[.]+[[:space:]]", "", iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^C[.]C[.]", "", iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^CC[.]+[[:space:]]", "", iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^C[.]C+[[:space:]]", "", iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^CC[.]", "", iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^CC+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CENTRO COMERCIAL+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^C[.] COMERCIAL+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<CENTRO CIAL\\>[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CENTRO C[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CENTRO C[.]", "", iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.



iri_v12$Direccion1 <- gsub("^C[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^C[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^C+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CALLEJON+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CALLE+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^C[/]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^C[/]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CL+[[:space:]]","",iri_v12$Direccion1)  
iri_v12$Direccion1 <- gsub("^CL[.]+[[:space:]]","",iri_v12$Direccion1)  
iri_v12$Direccion1 <- gsub("^CL[.]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^CA[.]+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^CA[.]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^CA+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^COL[.]+[[:space:]]","",iri_v12$Direccion1) 
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]"," ",iri_v12$Direccion1)  
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]"," ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<EL\\>+[[:space:]]"," ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.

iri_v12$Direccion1 <- gsub("^CAM+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CAM[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CAM[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CAMI+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CAMI[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CAMI[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CAMINO+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CNO+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CNO[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CNO[.]", "", iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^CAR+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CAR[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CAR[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CARR+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CARR[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CARR[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CARRER+[[:space:]]", "", iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^CARRETERA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CRTA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CRTA[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CRTA[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CRA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CRA[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CRA[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CRT+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CRT[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CRT[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CR+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CR[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CR[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CTRA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CTRA[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CTRA[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CTA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CTA[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CTA[.]", "", iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^CORREDERA+[[:space:]]", "", iri_v12$Direccion1)
# iri_v12$Direccion1 <- gsub("[^CORREDERA$]", "", iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1) 


iri_v12$Direccion1 <- gsub("^PL+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PL[.]+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PL[.]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PLA+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PLA[.]+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PLA[.]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PLAÇA+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PLZ+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PLZ[.]+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PLZ[.]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PZ+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PZ[.]+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PZ[.]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PZA+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PZA[.]+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PZA[.]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PLAZA+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PRAZA+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PTDA[.]+[[:space:]]","",iri_v12$Direccion1)  
iri_v12$Direccion1 <- gsub("^PTDA[.]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PARTIDA ","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
# iri_v12$Direccion1 <- gsub("^[DE|DEL]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.



iri_v12$Direccion1 <- gsub("^PTO+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PTO[.]+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PTO[.]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PUERTO+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
# iri_v12$Direccion1 <- gsub("^[DE|DEL]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.



iri_v12$Direccion1 <- gsub("^GTA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^GTA[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^GTA[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^GLO+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^GLO[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^GLO[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<GLORIETA\\>", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
# iri_v12$Direccion1 <- gsub("^[DE|DEL]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^GPO+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^GPO[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^GPO[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1)
# iri_v12$Direccion1 <- gsub("^[DE|DEL]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.



iri_v12$Direccion1 <- gsub("^LUG+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^LUG[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^LUG[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^LUGAR+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.

iri_v12$Direccion1 <- gsub("\\<NUEVE\\>", "9",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^PSO+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PSO[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PSO[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PO+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PO[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PO[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PAS+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PAS[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PAS[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PS+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PS[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PS[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PASEO+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.



iri_v12$Direccion1 <- gsub("\\<PARCELA\\>+[[:space:]]", " ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PARCELA\\>", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PORCEL\\>+[[:space:]]", " ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PORCEL\\>", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PARCEL\\>+[[:space:]]", " ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PARCEL\\>", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PARC\\>[.]+[[:space:]]", " ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PARC\\>[.]", " ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PARC\\>", " ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PORC\\>[.]+[[:space:]]", " ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PORC\\>[.]", " ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PORC\\>+[[:space:]]", " ",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^PJE+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PJE[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PJE[.]", "", iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.

iri_v12$Direccion1 <- gsub("^PQE[.]+[[:space:]]+NACIONAL", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PQE+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PQE[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PQE[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PQUE+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PQUE[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PQUE[.]+CIAL", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PQUE[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PQ+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PQ[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PQ[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<PARQUE\\>", "", iri_v12$Direccion1)
mi_gsub(from, to, pleis_v12$direccion)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^POB[.]+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^POB[.]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^POBLA+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^POBLADO+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^P[.]E[.]+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^P[.]E+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^POL+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^POL[.]+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^POL[.]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PLG+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PLG[.]+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PLG[.]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PNO+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PNO[.]+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PNO[.]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PG+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PG[.]+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PG[.]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^P[.]I[.]+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^P[.]I[.]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<POLIGONO INDUSTRIAL\\>","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^POLIGONO+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
# iri_v12$Direccion1 <- gsub("^[DE|DEL]+[[:space:]]", "", iri_v12$Direccion1)  #COMPROBAR ESTOS CORCHETES
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.



iri_v12$Direccion1 <- gsub("^RDA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RDA[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RDA[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^R+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^R[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^R[.]", "", iri_v12$Direccion1)
# iri_v12$Direccion1 <- gsub("^RONDA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<RONDA\\>", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<RESIDENCIAL\\>", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
# iri_v12$Direccion1 <- gsub("^[DE|DEL]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.



iri_v12$Direccion1 <- gsub("^\\<RAMBLA\\>", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<RAMBLE\\>", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RAM+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RAM[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RAM[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
# iri_v12$Direccion1 <- gsub("^[DE|DEL]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^RSD+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RSD[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RSD[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^SENDA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^SDA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^SDA[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^SDA[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^TRV+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^TRV[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^TRV[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^TR+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^TR[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^TR[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^TRAV+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^TRAV[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^TRAV[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^TRVS+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^TRVS[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^TRVS[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^TRAVESIA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^TRAVESERA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^TRAVESSERA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^URB+[[:space:]]", "", iri_v12$Direccion1)  #Mirar si interesa eliminar las urb que aparecen a mitad de dirección.
iri_v12$Direccion1 <- gsub("^URB[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^URB[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^URBANIZACION+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.



iri_v12$Direccion1 <- gsub("^RUA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RUA DA+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RUA DAS+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RUA DO+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RUA DOS+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RUA DE+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^VIA+[[:space:]]", "", iri_v12$Direccion1)  #Mirar si interesa eliminar las urb que aparecen a mitad de dirección.
iri_v12$Direccion1 <- gsub("^VIA[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^VIA[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
iri_v12$Direccion1 <- gsub("^LA+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^LO+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^EL+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^LAS+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^LOS+[[:space:]]","",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("^O+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^OS+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^A+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AS+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^ELS+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^LES+[[:space:]]","",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.

iri_v12$Direccion1 <- gsub("^CAT+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CAT[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CAT[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CATEDRATICO+[[:space:]]", "", iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1) 


iri_v12$Direccion1 <- gsub("^DOCTOR+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^DOUTOR+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^DOC+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^DOC[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^DOC[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^DC+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^DC[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^DC[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^DR+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^DR[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^DR[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^DTOR+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^DTOR[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^DTOR[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.

iri_v12$Direccion1 <- gsub("^DON+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^D[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^D[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^D[*]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^MOSSEN+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.

iri_v12$Direccion1 <- gsub("^OBISPO+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.

iri_v12$Direccion1 <- gsub("^PIN+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PIN[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PIN[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^PINTOR+[[:space:]]", "", iri_v12$Direccion1)
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.



iri_v12$Direccion1 <- gsub("^RAD+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RAD[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RAD[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^RADIOFONISTA", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
mi_gsub(from, to, iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.



#We remove again the Don and its friends. Let see if this is neccessary.
iri_v12$Direccion1 <- gsub("^DON+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^D[.]+[[:space:]]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^D[.]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^D[*]", "", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
iri_v12$Direccion1 <- gsub("^LA+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^LO+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^EL+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^LAS+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^LOS+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^O+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^OS+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^A+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AS+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^ELS+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^LES+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("D[']", "", iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^IND[.]", "", iri_v12$Direccion1)  
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
iri_v12$Direccion1 <- gsub("^PROF[.]", "", iri_v12$Direccion1)  


iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.

#============================================================================================================
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<DELS\\>+[[:space:]]"," DE LOS ",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<DEL\\>+[[:space:]]"," ",iri_v12$Direccion1)   
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<DE\\>+[[:space:]]"," ",iri_v12$Direccion1)  


#We remove content in parenthesis at the end of direccion.
iri_v12$Direccion1 <- gsub("[()]", "", iri_v12$Direccion1)
#============================================================================================================

iri_v12$Direccion1 <- gsub("[[:space:]]+\\<PLANTA\\>+[[:space:]]+[A-Z]+$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+$", "", iri_v12$Direccion1)  #=============== We remove the spaces on the right.

#Numbers to text.
iri_v12$Direccion1 <- gsub("\\<DOCE\\>", "12",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<DIECIOCHO\\>", "18",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<TRECE\\>", "13",iri_v12$Direccion1)


#Numbers
#We are going to remove the number in the address.
iri_v12$Direccion1 <- gsub("[[:space:]]+BAJO+$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+BJ+$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+[A-Z]+$","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+[[:space:]]+[Y]$","",iri_v12$Direccion1) #Numbers with preposition "Y" in the middle.
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\<PTA\\>$","",iri_v12$Direccion1) #Numbers with "pta" in the middle.

#To number with hyphen
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+[0-9]+\\-+[0-9]+$","",iri_v12$Direccion1) #For addresses with three numbers.
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+[0-9]+$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+\\_+[0-9]+$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+\\,+[0-9]+\\,+[0-9]+$","",iri_v12$Direccion1) #Numbers with comma between then.
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+\\,+[0-9]+$","",iri_v12$Direccion1) #Numbers with comma between then.
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+\\,+[[:space:]]+[0-9]+\\,+[[:space:]]+[0-9]+$","",iri_v12$Direccion1) #Numbers with comma and spaces between then.
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+$","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+\\_+$","",iri_v12$Direccion1) 
#There are numbers plus hyphen plus space plus number.
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+[0-9]+$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+$","",iri_v12$Direccion1)

#iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+$", "",iri_v12$Direccion1)



##===========================================================================================================================   
iri_v12$Direccion1 <- gsub("[0-9]+\\_+[0-9]+$","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("BAJO+$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("BJ+$","",iri_v12$Direccion1)
# iri_v12$Direccion1 <- gsub("[0-9]+$","",iri_v12$Direccion1) #PONGO ESTO MÁS ABAJO.
iri_v12$Direccion1 <- gsub("[0-9]+\\-+[A-Z]+$","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("[0-9]+[[:space:]]+\\-+[[:space:]]+[0-9]+[[:space:]]+[A-Z]+$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[0-9]+[[:space:]]+[A-Z]+$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[0-9]+[[:space:]]+[Y]$","",iri_v12$Direccion1) #Numbers with preposition "Y" in the middle.
iri_v12$Direccion1 <- gsub("[0-9]+[[:space:]]+\\<PTA\\>$","",iri_v12$Direccion1) #Numbers with "pta" in the middle.
iri_v12$Direccion1 <- gsub("[0-9]+[A-Z]+\\-+[0-9]+[A-Z]+$","",iri_v12$Direccion1) #add THIS=============================================================%%%%%%%%%%%%%%%%%%%%%%%%%%%
iri_v12$Direccion1 <- gsub("[0-9]+[A-Z]+$","",iri_v12$Direccion1) 

#To number with hyphen
iri_v12$Direccion1 <- gsub("[0-9]+\\-+[0-9]+\\-+[0-9]+$","",iri_v12$Direccion1) #For addresses with three numbers.
iri_v12$Direccion1 <- gsub("[0-9]+\\-+[0-9]+\\-$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[0-9]+\\-+[0-9]+$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[0-9]+\\,+[0-9]+$","",iri_v12$Direccion1) #Numbers with comma between then.
iri_v12$Direccion1 <- gsub("[0-9]+[/]+[0-9]+$","",iri_v12$Direccion1) #Numbers with slash between then.
iri_v12$Direccion1 <- gsub("[0-9]+\\-+$","",iri_v12$Direccion1) 

#There are numbers plus hyphen plus space plus number.
iri_v12$Direccion1 <- gsub("[0-9]+[[:space:]]+\\-+[0-9]+$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[0-9]+[[:space:]]+\\-+$","",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("[0-9]+\\-+[[:space:]]+[A-Z]+\\º$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[0-9]+\\-+[A-Z]+\\º$","",iri_v12$Direccion1) 
##===========================================================================================================================

iri_v12$Direccion1 <- gsub("[0-9]+$","",iri_v12$Direccion1)


#There are numbers plus hyphen plus space plus number.
#To remove comma between numbers.
iri_v12$Direccion1 <- gsub("[0-9]+\\,+$","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub(" S[/]N$","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("S[/]N$","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<SN\\>$","",iri_v12$Direccion1)   

iri_v12$Direccion1 <- gsub(" S[-]N$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("S[-]N$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub(" N[º]$","",iri_v12$Direccion1) 

iri_v12$Direccion1 <- gsub("[[:space:]]+N[.]$","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("N[.][0-9]$","",iri_v12$Direccion1) #We already removed the comma.
iri_v12$Direccion1 <- gsub("[[:space:]]+N[.][0-9]$","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("[0-9]+$","",iri_v12$Direccion1)
#We remove the comma at the end.
iri_v12$Direccion1 <- gsub(",$","",iri_v12$Direccion1)

# iri_v12$Direccion1 <- gsub("[0-9]+\\-+[[:space:]]+[A-Z]+\\º$","",iri_v12$Direccion1) #===============================================================BAJO============================
# iri_v12$Direccion1 <- gsub("[0-9]+\\-+[A-Z]+\\º$","",iri_v12$Direccion1) 

#=========================================================================================================



iri_v12$Direccion1 <- gsub("[[:space:]]+\\<XENDA\\>+$","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<COMPANY\\>","COMPANYS",iri_v12$Direccion1)
# iri_v12$Direccion1 <- gsub("ALEXANDRE ROSELLO","ALEXANDRE ROSSELLO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ROSELLO\\>","ROSSELLO",iri_v12$Direccion1)

#We remove the prepositions "de/del" in iri_v12/Direccion1.
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<DELS\\>+[[:space:]]"," DE LOS ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<DEL\\>+[[:space:]]"," ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<DE\\>+[[:space:]]"," ",iri_v12$Direccion1)  




#==========================================================================================================
iri_v12$Direccion1 <- gsub("\\<ANTON\\>[.]+[[:space:]]", "ANTONIO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ANTON\\>[.]", "ANTONIO ",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("ARQUI[.]+[[:space:]]","ARQUITECTO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ARQUI\\>[.]","ARQUITECTO ",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("AUZOA$","",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("^CTE[.]+[[:space:]]", "COMANDANTE ", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CTE[.]", "COMANDANTE", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^CTE+[[:space:]]", "COMANDANTE ", iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\CONJTO\\>[.]+[[:space:]]", "CONJUNTO ", iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\CONJTO\\>[.]", "CONJUNTO ", iri_v12$Direccion1)

# iri_v12$Direccion1 <- gsub("\\DGUEZ\\>[.]+[[:space:]]", "DOMINGUEZ ", iri_v12$Direccion1)
# iri_v12$Direccion1 <- gsub("\\DGUEZ\\>[.]", "DOMINGUEZ ", iri_v12$Direccion1)
# iri_v12$Direccion1 <- gsub("\\DGUEZ\\>+[[:space:]]", "DOMINGUEZ ", iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<EDIF\\>[.]+[[:space:]]", "EDIFICIO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<EDIF\\>[.]", "EDIFICIO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<EDI\\>[.]+[[:space:]]", "EDIFICIO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<EDI\\>[.]", "EDIFICIO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<EDF\\>[.]+[[:space:]]", "EDIFICIO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<EDF\\>[.]", "EDIFICIO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ED\\>[.]+[[:space:]]", "EDIFICIO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ED\\>[.]", "EDIFICIO ",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<ESCRIT\\>[.]+[[:space:]]", "ESCRITOR ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ESCRIT\\>[.]", "ESCRITOR ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ESCRIT\\>[.]+[[:space:]]", "ESCRITOR ",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("FCO+[[:space:]]","FRANCISCO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("FCO[.]+[[:space:]]","FRANCISCO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("FCO[.]","FRANCISCO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<FDEZ\\>+[[:space:]]", "FERNANDEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<FDEZ\\>[.]+[[:space:]]", "FERNANDEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<FDEZ\\>[.]", "FERNANDEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<FDEZ\\>$", "FERNANDEZ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<FED\\>+[.]", "FEDERICO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("FELIP+[[:space:]]","FELIPE ",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<GCIA\\>+[[:space:]]", "GARCIA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<GCIA\\>[.]+[[:space:]]", "GARCIA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<GCIA\\>[.]", "GARCIA ",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<GLEZ\\>+[[:space:]]", "GONZALEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<GLEZ\\>[.]+[[:space:]]", "GONZALEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<GLEZ\\>[.]", "GONZALEZ ",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("GRAL[.]+[[:space:]]","GENERAL ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("GRAL[.]","GENERAL ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("GRA[.]+[[:space:]]","GENERAL ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("GRA[.]","GENERAL ",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<HDEZ\\>+[[:space:]]", "HERNANDEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<HDEZ\\>[.]+[[:space:]]", "HERNANDEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<HDEZ\\>[.]", "HERNANDEZ ",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("HIST[.]+[[:space:]]","HISTORIADOR ",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("HIST[.]","HISTORIADOR ",iri_v12$Direccion1) 

iri_v12$Direccion1 <- gsub("HNOS[.]+[[:space:]]","HERMANOS ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("HNOS[.]","HERMANOS ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("HNOS+[[:space:]]","HERMANOS ",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("ING[.]", "INGENIERO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("INGEN[.]", "INGENIERO ",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("JOAQ[.]", "JOAQUIN ",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("JURIZMENDI","JUDIZMENDI",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("KALEA$","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("\\<KALE\\>","",iri_v12$Direccion1) 

iri_v12$Direccion1 <- gsub("M[ª]", "MARIA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("M[*]", "MARIA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<M\\>[.]", "MARIA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^M+[[:space:]]", "MARIA ",iri_v12$Direccion1)   
iri_v12$Direccion1 <- gsub("[[:space:]]+M+[[:space:]]", "MARIA ",iri_v12$Direccion1) 

iri_v12$Direccion1 <- gsub("\\<MARAGALL\\>", "MARGALL",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<MTNEZ\\>+[[:space:]]", "MARTINEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<MTNEZ\\>[.]+[[:space:]]", "MARTINEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<MTNEZ\\>[.]", "MARTINEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<MNEZ\\>+[[:space:]]", "MARTINEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<MNEZ\\>[.]+[[:space:]]", "MARTINEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<MNEZ\\>[.]", "MARTINEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<MONTSE\\>", "MONTSERRAT",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<N\\>[.]+[[:space:]]+\\<SRA\\>[.]+[[:space:]]", "NUESTRA SEÑORA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<N\\>[.]+[[:space:]]+\\<SRA\\>[.]", "NUESTRA SEÑORA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<N\\>[.]+\\<SRA\\>[.]+[[:space:]]", "NUESTRA SEÑORA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<N\\>[.]+\\<SRA\\>[.]", "NUESTRA SEÑORA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<NTRA\\>[.]+[[:space:]]", "NUESTRA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<NTRA\\>[.]", "NUESTRA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<NTRA\\>+[[:space:]]", "NUESTRA ",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<NTRO\\>[.]+[[:space:]]", "NUESTRO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<NTRO\\>[.]", "NUESTRO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<NTRO\\>+[[:space:]]", "NUESTRO ",iri_v12$Direccion1)



iri_v12$Direccion1 <- gsub("\\<JOSE ORTEGA Y GASSET\\>", "ORTEGA Y GASSET",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<JOSE ORTEGA GASSET\\>", "ORTEGA Y GASSET",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ORTEGA GASSET\\>", "ORTEGA Y GASSET",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<PERIOD\\>+[[:space:]]", "PERIODISTA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PERIOD\\>[.]", "PERIODISTA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PINTOR JOAQUIN SOROLLA\\>", "SOROLLA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PINTOR SOROLLA\\>", "SOROLLA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<JOAQUIN SOROLLA\\>", "SOROLLA",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<REI JAUME I\\>", "JAIME I",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<REY JAIME I\\>", "JAIME I",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<REY JUAN CARLOS I\\>", "JUAN CARLOS I",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<RDGUEZ\\>+[[:space:]]", "RODRIGUEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<RDGUEZ\\>[.]+[[:space:]]", "RODRIGUEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<RDGUEZ\\>[.]", "RODRIGUEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<RGUEZ\\>[.]+[[:space:]]","RODRIGUEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<RGUEZ\\>[.]","RODRIGUEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<RDEZ\\>+[[:space:]]", "RODRIGUEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<RDEZ\\>[.]+[[:space:]]", "RODRIGUEZ ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<RDEZ\\>[.]", "RODRIGUEZ ",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<SRA\\>[.]+[[:space:]]", "SEÑORA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<SRA\\>[.]", "SEÑORA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<SRA\\>+[[:space:]]", "SEÑORA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("SINDICA[.]+[[:space:]]", "SINDICALISTA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("SINDICA+[[:space:]]", "SINDICALISTA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<SINDICA\\>[.]", "SINDICALISTA ",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("STOS+[[:space:]]", "SANTOS ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("STOS[.]+[[:space:]]", "SANTOS ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("STOS[.]", "SANTOS ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<STOS\\>", "SANTOS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<STO\\>[.]+[[:space:]]", "SANTO ",iri_v12$Direccion1)   
iri_v12$Direccion1 <- gsub("\\<STO\\>[.]", "SANTO ",iri_v12$Direccion1)   
iri_v12$Direccion1 <- gsub("\\<STO\\>+[[:space:]]", "SANTO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<STA\\>[.]+[[:space:]]", "SANTA ",iri_v12$Direccion1)   
iri_v12$Direccion1 <- gsub("\\<STA\\>[.]", "SANTA ",iri_v12$Direccion1)   
iri_v12$Direccion1 <- gsub("\\<STA\\>+[[:space:]]", "SANTA ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ST\\>+[[:space:]]", "SANTO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ST\\>[.]+[[:space:]]", "SANTO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ST\\>[.]", "SANTO ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<S\\>[.]+[[:space:]]", "SAN ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<S\\>[.]", "SAN ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<S\\>+[[:space:]]", "SAN ",iri_v12$Direccion1)



#======================================================================================================
iri_v12$Direccion1 <- gsub("KM+[[:space:]]+[0-9]+$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("KM-[0-9]+$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<KM\\>$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<LOC\\>[.]", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<L\\>[.]", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+\\<LOCAL\\>$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+[[:space:]]+\\<LOCAL\\>$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+[[:space:]]+\\<LOCAL\\>$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+\\<LOCALES\\>$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+[[:space:]]+\\<LOCALES\\>$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+[[:space:]]+\\<LOCALES\\>$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<LOCAL\\>$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<B\\>$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<BLOQUE\\>$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<BLQ\\>$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<BLQ\\>[.]$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<BL\\>[-]$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<P\\>[.]$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<P\\>[-]$", "",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("[[:space:]]+\\<DELS\\>+[[:space:]]"," DE LOS ",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<DEL\\>+[[:space:]]"," ",iri_v12$Direccion1)   
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<DE\\>+[[:space:]]"," ",iri_v12$Direccion1) 



#Translates
iri_v12$Direccion1 <- gsub("\\<ABATXOLO\\>", "ABACHOLO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<AGUSTI\\>", "AGUSTIN",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<AJUNTAMENT\\>","AYUNTAMIENTO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ALACANT\\>", "ALICANTE",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ALBERT\\>", "ALBERTO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ALFONS\\>", "ALFONSO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ALFRED\\>", "ALFREDO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ALINYO\\>", "ALIÑO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ALMOGAVERS\\>", "ALMOGAVERES",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ALPS\\>", "ALPES",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ANNA\\>", "ANA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ANDALUSIA\\>", "ANDALUCIA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ANDREU\\>", "ANDRES",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ANGELS\\>", "ANGELES",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ANSELM\\>", "ANSELMO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ANTIC\\>", "ANTIGUO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ANTIGA\\>", "ANTIGUA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ANTONI\\>", "ANTONIO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ARAGO\\>", "ARAGON",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ARCADI\\>", "ARCADIO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ARQUEBISBE\\>", "ARZOBISPO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ARMADES\\>", "ARMADAS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ARTESANS\\>", "ARTESANOS",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<BAIX\\>", "BAJO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<BASKONIA\\>", "VASCONIA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<BENVINGUTS\\>", "BIENVENIDOS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<BENVINGUT\\>", "BIENVENIDO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<BISBE\\>", "OBISPO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<BISCAIA\\>", "VIZCAYA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<BOTICARI\\>", "BOTICARIO",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<CAMINAS\\>", "CAMINOS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CAMIL\\>", "CAMILO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CAMP\\>", "CAMPO",iri_v12$Direccion1)
iri_v12$Direccion1  <- gsub("\\<CANALETES\\>", "CANALETAS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CAPITA\\>", "CAPITAN",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CAPUTXINS\\>", "CAPUCHINOS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CARME\\>", "CARMEN",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CARLES\\>", "CARLOS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CASTELA\\>", "CASTILLA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CASTELLO\\>", "CASTELLON",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CATALUNYA\\>", "CATALUÑA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CATOLICS\\>", "CATOLICOS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CIMADEVILA\\>", "CIMADEVILLA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CIRCUMVALACIO\\>", "CIRCUNVALACION",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CIUTAT\\>", "CIUDAD",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CIUTADANS\\>", "CIUDADANOS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<COLOM\\>", "COLON",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<COMERÇ\\>", "COMERCIO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<COMTE\\>", "CONDE",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CONCA\\>", "CUENCA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CONCELLO\\>", "CONCEJO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CONCILI\\>", "CONCILIO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CONSTITUCIO\\>", "CONSTITUCION",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<COR\\>", "CORAZON",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CORNELI\\>", "CORNELIO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CREU\\>", "CRUZ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CRIST\\>", "CRISTO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CRISTOFOR\\>", "CRISTOBAL",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<CRISTOVAL\\>", "CRISTOBAL",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<DESEMPARATS\\>", "DESAMPARADOS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<DEU\\>+[[:space:]]", "DIOS ",iri_v12$Direccion1)
# iri_v12$Direccion1 <- gsub("^\\<12\\>", "DOCE",iri_v12$Direccion1)
# iri_v12$Direccion1 <- gsub("^\\<18\\>", "DIECIOCHO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<DOUTOR\\>", "DOCTOR",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<DIPUTACIO\\>", "DIPUTACION",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<D'ELX\\>", "DE ELCHE",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ELX\\>", "ELCHE",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<EDUARD\\>", "EDUARDO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<EGLESIA\\>", "IGLESIA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ENGINYER\\>", "INGENIERO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ENRIC\\>", "ENRIQUE",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<L'ESGLESIA\\>", "LA IGLESIA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ESGLESIA\\>", "IGLESIA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ESPANYA\\>", "ESPAÑA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ESPiri_v12TO\\>", "ESPiri_v12TU",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<L'ESTACIO\\>", "LA ESTACION",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ESTACIO\\>", "ESTACION",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ESTUDIS\\>", "ESTUDIOS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<EXERCITO\\>", "EJERCITO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<EXERCIT\\>", "EJERCITO",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<FLORS\\>", "FLORES",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<FORCES\\>", "FUERZAS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<FRA\\>", "FRAY",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<FREY\\>", "FRAY",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<FRANCESC\\>", "FRANCISCO",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<GAUDENCI\\>", "GAUDENCIO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<GERVASI\\>", "GERVASIO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<GIRONA\\>", "GERONA",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<HOMENS\\>", "HOMBRES",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("[[:space:]]+I+[[:space:]]", " Y ",iri_v12$Direccion1) #conjunción copulativa: Pi y Margall, etc.
iri_v12$Direccion1 <- gsub("\\<IGNASI\\>","IGNACIO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ILLAS\\>","ISLAS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<INDUSTRI\\>","INDUSTRIA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ISIDRE\\>","ISIDRO",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<JACINT\\>","JACINTO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<JAUME\\>","JAIME",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<JOAN\\>", "JUAN",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<JOAQUIM\\>", "JOAQUIN ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<JORDI\\>", "JORGE",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<JULIOL\\>", "JULIO",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<L'ESCORIAL\\>", "EL ESCORIAL",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<LLEIDA\\>", "LERIDA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<LIBERDADE\\>", "LIBERTAD",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<LLIBERTAD\\>", "LIBERTAD",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<LLIBERTAT\\>", "LIBERTAD",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<LLUCIA\\>", "LUCIA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<LLUIS\\>", "LUIS",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<MACIA\\>", "MACIAS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<MAIG\\>", "MAYO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<MAGISTRAT\\>", "MAGISTRADO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<MAIOR\\>", "MAYOR",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<MAJOR\\>", "MAYOR",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<MARE\\>+[[:space:]]", "MADRE ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<MARITIM\\>", "MARITIMO",iri_v12$Direccion1) #We replace exactly "maritim" not words which contain "maritim".
iri_v12$Direccion1 <- gsub("\\<MARIÑA\\>", "MARINA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<MERCAT\\>", "MERCADO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<MESTRE\\>", "MAESTRO",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<NAPOLS\\>", "NAPOLES",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<NATURAIS\\>", "NATURALES",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<NICOLAU\\>", "NICOLAS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<NOVA\\>", "NUEVA",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<ONZE\\>", "ONCE",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ORTIGUEIRA\\>", "ORTIGUERA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<OURENSE\\>", "ORENSE",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<O SABIO\\>", "EL SABIO",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<PALMERES\\>", "PALMERAS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PARAGUAI\\>", "PARAGUAY",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PARALEL\\>", "PARALELO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PARE\\>", "PADRE",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PARLAMENT\\>", "PARLAMENTO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PASQUAL\\>", "PASCUAL",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PASSEIG\\>", "PASEO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PAU\\>", "PAZ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PEDRAIO\\>", "PEDRAYO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PEP\\>", "PEPE",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PERE\\>", "PEDRO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PLATJA\\>", "PLAYA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<POBRESA\\>", "POBREZA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PORT\\>", "PUERTO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PRESIDENT\\>", "PRESIDENTE",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<PRIMER\\>", "PRIMERO",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<REGNE\\>", "REINO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<REIS\\>", "REYES",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<RIU\\>", "RIO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<ROBERT\\>", "ROBERTO",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<SAGRAT\\>", "SAGRADO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<SANT\\>", "SAN",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<SANTISSIMA\\>", "SANTISIMA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<SANTISSIM\\>", "SANTISIMO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<SAVI\\>", "SABIO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<SEBASTIA\\>", "SEBASTIAN",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<SEGON\\>", "SEGUNDO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<SENYORA\\>", "SEÑORA",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<SEPTEMBRE\\>", "SEPTIEMBRE",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<SETEMBRE\\>", "SEPTIEMBRE",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<SETZE\\>", "SIETE",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<TAQUIGRAF\\>", "TAQUIGRAFO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<TARRADELLES\\>", "TARRADELLAS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<TEIXEIRO\\>", "TEIJEIRO",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<TEMPLERS\\>", "TEMPLARIOS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<TORNEIROS\\>", "TORNEROS",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<TRINITAT\\>", "TRINIDAD",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<TRIOMF\\>", "TRIUNFO",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<UNIVERSITAT\\>", "UNIVERSIDAD",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<VERGE\\>", "VIRGEN",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<VICENS\\>", "VICENTE",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<VICENT\\>", "VICENTE",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("\\<XAVIER\\>", "JAVIER",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<XOSE\\>", "JOSE",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<XUNQUEIRA\\>", "JUNQUERA",iri_v12$Direccion1)



iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.


iri_v12$Direccion1 <- gsub("\\-+$", "", iri_v12$Direccion1)  #=============== We remove the hyphens on the right.
iri_v12$Direccion1 <- gsub("[[:space:]]+$", "", iri_v12$Direccion1)  #=============== We remove all the spaces on the right.
iri_v12$Direccion1 <- gsub("\\-+$", "", iri_v12$Direccion1)  #=============== We remove the hyphens on the right.


iri_v12$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri_v12$Direccion1)  
iri_v12$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri_v12$Direccion1) 
iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.
iri_v12$Direccion1 <- gsub("^LA+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^LO+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^EL+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^LAS+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^LOS+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^O+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^OS+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^A+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^AS+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^ELS+[[:space:]]","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("^LES+[[:space:]]","",iri_v12$Direccion1)

iri_v12$Direccion1 <- gsub("^[[:space:]]", "", iri_v12$Direccion1)  #=============== We remove the spaces on the left.



#NATIONAL ROADS
iri_v12$Direccion1 <- gsub("\\<N\\>[.]+[[:space:]]", "NACIONAL ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<N\\>[.]", "NACIONAL ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<N\\>[-]+[[:space:]]", "NACIONAL ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<N\\>[-]", "NACIONAL ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<NACIONAL\\>[-]", "NACIONAL ",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("KM[.]+[[:space:]]+[0-9]+$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("\\<KM\\>[.]", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+$", "", iri_v12$Direccion1)  #=============== We remove the spaces on the right.


iri_v12$Direccion1 <- gsub("\\/+[[:space:]]+\\/","",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+$", "", iri_v12$Direccion1)  #=============== We remove the spaces on the right.

#===============================================================================================PORTAL=======================================================
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<P\\>$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+\\<PORTAL\\>[[:space:]]+[0-9]+$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+$", "", iri_v12$Direccion1)  #=============== We remove the spaces on the right.


iri_v12$Direccion1 <- gsub("[[:space:]]+\\<SOLAR\\>+[[:space:]]+[A-Z|0-9]+$", "",iri_v12$Direccion1)
iri_v12$Direccion1 <- gsub("[[:space:]]+$", "", iri_v12$Direccion1)  #=============== We remove the spaces on the right.

#================================================================Changes in pleis_v12' addresses==================================================================================
#We remove tildes in addresses from pleis_v12.
pleis_v12$direccion <- gsub("Á", "A", pleis_v12$direccion)
pleis_v12$direccion <- gsub("É", "E", pleis_v12$direccion)
pleis_v12$direccion <- gsub("Í", "I", pleis_v12$direccion)
pleis_v12$direccion <- gsub("Ó", "O", pleis_v12$direccion)
pleis_v12$direccion <- gsub("Ú", "U", pleis_v12$direccion)
pleis_v12$direccion <- gsub("Ü", "U", pleis_v12$direccion)

#=====================================================================================================================================

pleis_v12$direccion <- gsub("^AV+[[:space:]]", "", pleis_v12$direccion)     
pleis_v12$direccion <- gsub("^AV[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AV[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AVD+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AVD[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AVD[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AVDA[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("AVDA[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AVDA,+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AVDA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AVDA", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AVENIDA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AVINGUDA+[[:space:]]", "", pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^ANT+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^ANT[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^ANT[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^ANTIGUA+[[:space:]]", "", pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^ARR+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^ARR[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^ARR[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^ARRAVAL+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^ARRABAL+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RAVAL+[[:space:]]", "", pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^AUT+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AUT[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AUT[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AUTOPISTA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AUTOVIA+[[:space:]]", "", pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^BARRIO+[[:space:]]", "", pleis_v12$direccion)   
pleis_v12$direccion <- gsub("^BARRIAL+[[:space:]]", "", pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^BARRIS+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BARRO+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BAR[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BAR[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BA[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BA[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^B[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^B[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BDA[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BDA[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BD[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BD[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BD+[[:space:]]", "", pleis_v12$direccion)

pleis_v12$direccion <- gsub("^BARRIADA+[[:space:]]", "", pleis_v12$direccion)

mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^BOULEVARD+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BOULEVAR+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^BULEVARD+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BULEVAR+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BOULE+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BOULE[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BOULE[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BO[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BO[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<BO\\>+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BU[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^BU[.]", "", pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^C[.]C[.]+[[:space:]]", "", pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^C[.]C[.]", "", pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^CC[.]+[[:space:]]", "", pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^C[.]C+[[:space:]]", "", pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^CC[.]", "", pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^CC+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CENTRO COMERCIAL+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^C[.] COMERCIAL+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<CENTRO CIAL\\>[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CENTRO C[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CENTRO C[.]", "", pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.



pleis_v12$direccion <- gsub("^C[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^C[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^C+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CALLEJON+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CALLE+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^C[/]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^C[/]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CL+[[:space:]]","",pleis_v12$direccion)  
pleis_v12$direccion <- gsub("^CL[.]+[[:space:]]","",pleis_v12$direccion)  
pleis_v12$direccion <- gsub("^CL[.]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^CA[.]+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^CA[.]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^CA+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^COL[.]+[[:space:]]","",pleis_v12$direccion) 
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]"," ",pleis_v12$direccion)  
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]"," ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<EL\\>+[[:space:]]"," ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.

pleis_v12$direccion <- gsub("^CAM+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CAM[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CAM[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CAMI+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CAMI[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CAMI[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CAMINO+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CNO+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CNO[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CNO[.]", "", pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^CAR+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CAR[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CAR[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CARR+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CARR[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CARR[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CARRER+[[:space:]]", "", pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^CARRETERA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CRTA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CRTA[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CRTA[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CRA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CRA[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CRA[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CRT+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CRT[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CRT[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CR+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CR[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CR[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CTRA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CTRA[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CTRA[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CTA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CTA[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CTA[.]", "", pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^CORREDERA+[[:space:]]", "", pleis_v12$direccion)
# pleis_v12$direccion <- gsub("[^CORREDERA$]", "", pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion) 


pleis_v12$direccion <- gsub("^PL+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PL[.]+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PL[.]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PLA+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PLA[.]+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PLA[.]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PLAÇA+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PLZ+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PLZ[.]+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PLZ[.]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PZ+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PZ[.]+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PZ[.]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PZA+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PZA[.]+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PZA[.]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PLAZA+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PRAZA+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PTDA[.]+[[:space:]]","",pleis_v12$direccion)  
pleis_v12$direccion <- gsub("^PTDA[.]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PARTIDA ","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
# pleis_v12$direccion <- gsub("^[DE|DEL]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.



pleis_v12$direccion <- gsub("^PTO+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PTO[.]+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PTO[.]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PUERTO+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
# pleis_v12$direccion <- gsub("^[DE|DEL]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.



pleis_v12$direccion <- gsub("^GTA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^GTA[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^GTA[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^GLO+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^GLO[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^GLO[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<GLORIETA\\>", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
# pleis_v12$direccion <- gsub("^[DE|DEL]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^GPO+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^GPO[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^GPO[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion)
# pleis_v12$direccion <- gsub("^[DE|DEL]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.



pleis_v12$direccion <- gsub("^LUG+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^LUG[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^LUG[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^LUGAR+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.

pleis_v12$direccion <- gsub("\\<NUEVE\\>", "9",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^PSO+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PSO[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PSO[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PO+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PO[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PO[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PAS+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PAS[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PAS[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PS+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PS[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PS[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PASEO+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.



pleis_v12$direccion <- gsub("\\<PARCELA\\>+[[:space:]]", " ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PARCELA\\>", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PORCEL\\>+[[:space:]]", " ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PORCEL\\>", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PARCEL\\>+[[:space:]]", " ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PARCEL\\>", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PARC\\>[.]+[[:space:]]", " ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PARC\\>[.]", " ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PARC\\>", " ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PORC\\>[.]+[[:space:]]", " ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PORC\\>[.]", " ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PORC\\>+[[:space:]]", " ",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^PJE+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PJE[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PJE[.]", "", pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.

pleis_v12$direccion <- gsub("^PQE[.]+[[:space:]]+NACIONAL", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PQE+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PQE[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PQE[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PQUE+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PQUE[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PQUE[.]+CIAL", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PQUE[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PQ+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PQ[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PQ[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<PARQUE\\>", "", pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.

pleis_v12$direccion <- gsub("^POB[.]+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^POB[.]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^POBLA+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^POBLADO+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^P[.]E[.]+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^P[.]E+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^POL+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^POL[.]+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^POL[.]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PLG+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PLG[.]+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PLG[.]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PNO+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PNO[.]+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PNO[.]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PG+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PG[.]+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PG[.]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^P[.]I[.]+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^P[.]I[.]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<POLIGONO INDUSTRIAL\\>","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^POLIGONO+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
# pleis_v12$direccion <- gsub("^[DE|DEL]+[[:space:]]", "", pleis_v12$direccion)  #COMPROBAR ESTOS CORCHETES
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.



pleis_v12$direccion <- gsub("^RDA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RDA[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RDA[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^R+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^R[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^R[.]", "", pleis_v12$direccion)
# pleis_v12$direccion <- gsub("^RONDA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<RONDA\\>", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<RESIDENCIAL\\>", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
# pleis_v12$direccion <- gsub("^[DE|DEL]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.



pleis_v12$direccion <- gsub("^\\<RAMBLA\\>", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<RAMBLE\\>", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RAM+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RAM[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RAM[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
# pleis_v12$direccion <- gsub("^[DE|DEL]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^RSD+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RSD[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RSD[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^SENDA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^SDA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^SDA[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^SDA[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^TRV+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^TRV[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^TRV[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^TR+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^TR[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^TR[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^TRAV+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^TRAV[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^TRAV[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^TRVS+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^TRVS[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^TRVS[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^TRAVESIA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^TRAVESERA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^TRAVESSERA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^URB+[[:space:]]", "", pleis_v12$direccion)  #Mirar si interesa eliminar las urb que aparecen a mitad de dirección.
pleis_v12$direccion <- gsub("^URB[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^URB[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^URBANIZACION+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^VIA+[[:space:]]", "", pleis_v12$direccion)  #Mirar si interesa eliminar las urb que aparecen a mitad de dirección.
pleis_v12$direccion <- gsub("^VIA[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^VIA[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
ipleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^RUA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RUA DA+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RUA DAS+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RUA DO+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RUA DOS+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RUA DE+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
pleis_v12$direccion <- gsub("^LA+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^LO+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^EL+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^LAS+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^LOS+[[:space:]]","",pleis_v12$direccion)

pleis_v12$direccion <- gsub("^O+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^OS+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^A+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AS+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^ELS+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^LES+[[:space:]]","",pleis_v12$direccion)

pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.

pleis_v12$direccion <- gsub("^CAT+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CAT[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CAT[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CATEDRATICO+[[:space:]]", "", pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion) 


pleis_v12$direccion <- gsub("^DOCTOR+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^DOUTOR+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^DOC+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^DOC[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^DOC[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^DC+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^DC[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^DC[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^DR+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^DR[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^DR[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^DTOR+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^DTOR[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^DTOR[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.

pleis_v12$direccion <- gsub("^DON+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^D[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^D[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^D[*]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^MOSSEN+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.

pleis_v12$direccion <- gsub("^OBISPO+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.

pleis_v12$direccion <- gsub("^PIN+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PIN[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PIN[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^PINTOR+[[:space:]]", "", pleis_v12$direccion)
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.



pleis_v12$direccion <- gsub("^RAD+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RAD[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RAD[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^RADIOFONISTA", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
mi_gsub(from, to, pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.



#We remove again the Don and its friends. Let see if this is neccessary.
pleis_v12$direccion <- gsub("^DON+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^D[.]+[[:space:]]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^D[.]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^D[*]", "", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
pleis_v12$direccion <- gsub("^LA+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^LO+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^EL+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^LAS+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^LOS+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^O+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^OS+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^A+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AS+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^ELS+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^LES+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("D[']", "", pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^IND[.]", "", pleis_v12$direccion)  
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
pleis_v12$direccion <- gsub("^PROF[.]", "", pleis_v12$direccion)  


pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.

#============================================================================================================
pleis_v12$direccion <- gsub("[[:space:]]+\\<DELS\\>+[[:space:]]"," DE LOS ",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("[[:space:]]+\\<DEL\\>+[[:space:]]"," ",pleis_v12$direccion)   
pleis_v12$direccion <- gsub("[[:space:]]+\\<DE\\>+[[:space:]]"," ",pleis_v12$direccion)  


#We remove content in parenthesis at the end of direccion.
pleis_v12$direccion <- gsub("[()]", "", pleis_v12$direccion)
#============================================================================================================

pleis_v12$direccion <- gsub("[[:space:]]+\\<PLANTA\\>+[[:space:]]+[A-Z]+$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+$", "", pleis_v12$direccion)  #=============== We remove the spaces on the right.

#Numbers to text.
pleis_v12$direccion <- gsub("\\<DOCE\\>", "12",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<DIECIOCHO\\>", "18",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<TRECE\\>", "13",pleis_v12$direccion)



#Numbers
#We are going to remove the number in the address.
pleis_v12$direccion <- gsub("[[:space:]]+BAJO+$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+BJ+$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+\\-+[A-Z]+$","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+[Y]$","",pleis_v12$direccion) #Numbers with preposition "Y" in the middle.
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\<PTA\\>$","",pleis_v12$direccion) #Numbers with "pta" in the middle.

#To number with hyphen
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+\\-+[0-9]+\\-+[0-9]+$","",pleis_v12$direccion) #For addresses with three numbers.
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+\\-+[0-9]+$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+\\_+[0-9]+$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+\\,+[0-9]+\\,+[0-9]+$","",pleis_v12$direccion) #Numbers with comma between then.
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+\\,+[0-9]+$","",pleis_v12$direccion) #Numbers with comma between then.
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+\\,+[[:space:]]+[0-9]+\\,+[[:space:]]+[0-9]+$","",pleis_v12$direccion) #Numbers with comma and spaces between then.
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+\\-+$","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+\\_+$","",pleis_v12$direccion) 
#There are numbers plus hyphen plus space plus number.
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+[0-9]+$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+$","",pleis_v12$direccion)

#pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+$", "",pleis_v12$direccion)



##===========================================================================================================================   
pleis_v12$direccion <- gsub("[0-9]+\\_+[0-9]+$","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("BAJO+$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("BJ+$","",pleis_v12$direccion)
# pleis_v12$direccion <- gsub("[0-9]+$","",pleis_v12$direccion) #PONGO ESTO MÁS ABAJO.
pleis_v12$direccion <- gsub("[0-9]+\\-+[A-Z]+$","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("[0-9]+[[:space:]]+\\-+[[:space:]]+[0-9]+[[:space:]]+[A-Z]+$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[0-9]+[[:space:]]+[A-Z]+$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[0-9]+[[:space:]]+[Y]$","",pleis_v12$direccion) #Numbers with preposition "Y" in the middle.
pleis_v12$direccion <- gsub("[0-9]+[[:space:]]+\\<PTA\\>$","",pleis_v12$direccion) #Numbers with "pta" in the middle.
pleis_v12$direccion <- gsub("[0-9]+[A-Z]+\\-+[0-9]+[A-Z]+$","",pleis_v12$direccion) #add THIS=============================================================%%%%%%%%%%%%%%%%%%%%%%%%%%%
pleis_v12$direccion <- gsub("[0-9]+[A-Z]+$","",pleis_v12$direccion) 

#To number with hyphen
pleis_v12$direccion <- gsub("[0-9]+\\-+[0-9]+\\-+[0-9]+$","",pleis_v12$direccion) #For addresses with three numbers.
pleis_v12$direccion <- gsub("[0-9]+\\-+[0-9]+\\-$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[0-9]+\\-+[0-9]+$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[0-9]+\\,+[0-9]+$","",pleis_v12$direccion) #Numbers with comma between then.
pleis_v12$direccion <- gsub("[0-9]+[/]+[0-9]+$","",pleis_v12$direccion) #Numbers with slash between then.
pleis_v12$direccion <- gsub("[0-9]+\\-+$","",pleis_v12$direccion) 

#There are numbers plus hyphen plus space plus number.
pleis_v12$direccion <- gsub("[0-9]+[[:space:]]+\\-+[0-9]+$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[0-9]+[[:space:]]+\\-+$","",pleis_v12$direccion)

pleis_v12$direccion <- gsub("[0-9]+\\-+[[:space:]]+[A-Z]+\\º$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[0-9]+\\-+[A-Z]+\\º$","",pleis_v12$direccion) 
##===========================================================================================================================

pleis_v12$direccion <- gsub("[0-9]+$","",pleis_v12$direccion)


#There are numbers plus hyphen plus space plus number.
#To remove comma between numbers.
pleis_v12$direccion <- gsub("[0-9]+\\,+$","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub(" S[/]N$","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("S[/]N$","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("[[:space:]]+\\<SN\\>$","",pleis_v12$direccion)   

pleis_v12$direccion <- gsub(" S[-]N$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("S[-]N$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub(" N[º]$","",pleis_v12$direccion) 

pleis_v12$direccion <- gsub("[[:space:]]+N[.]$","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("N[.][0-9]$","",pleis_v12$direccion) #We already removed the comma.
pleis_v12$direccion <- gsub("[[:space:]]+N[.][0-9]$","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("[0-9]+$","",pleis_v12$direccion) 
#We remove the comma at the end.
pleis_v12$direccion <- gsub(",$","",pleis_v12$direccion)

# pleis_v12$direccion <- gsub("[0-9]+\\-+[[:space:]]+[A-Z]+\\º$","",pleis_v12$direccion) #===============================================================BAJO============================
# pleis_v12$direccion <- gsub("[0-9]+\\-+[A-Z]+\\º$","",pleis_v12$direccion) 

#=========================================================================================================



pleis_v12$direccion <- gsub("[[:space:]]+\\<XENDA\\>+$","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<COMPANY\\>","COMPANYS",pleis_v12$direccion)
# pleis_v12$direccion <- gsub("ALEXANDRE ROSELLO","ALEXANDRE ROSSELLO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ROSELLO\\>","ROSSELLO",pleis_v12$direccion)

#We remove the prepositions "de/del" in iri_v12/Direccion1.
pleis_v12$direccion <- gsub("[[:space:]]+\\<DELS\\>+[[:space:]]"," DE LOS ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+\\<DEL\\>+[[:space:]]"," ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+\\<DE\\>+[[:space:]]"," ",pleis_v12$direccion)  




#==========================================================================================================
pleis_v12$direccion <- gsub("\\<ANTON\\>[.]+[[:space:]]", "ANTONIO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ANTON\\>[.]", "ANTONIO ",pleis_v12$direccion)

pleis_v12$direccion <- gsub("ARQUI[.]+[[:space:]]","ARQUITECTO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ARQUI\\>[.]","ARQUITECTO ",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("AUZOA$","",pleis_v12$direccion)

pleis_v12$direccion <- gsub("^CTE[.]+[[:space:]]", "COMANDANTE ", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CTE[.]", "COMANDANTE", pleis_v12$direccion)
pleis_v12$direccion <- gsub("^CTE+[[:space:]]", "COMANDANTE ", pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\CONJTO\\>[.]+[[:space:]]", "CONJUNTO ", pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\CONJTO\\>[.]", "CONJUNTO ", pleis_v12$direccion)

# pleis_v12$direccion <- gsub("\\DGUEZ\\>[.]+[[:space:]]", "DOMINGUEZ ", pleis_v12$direccion)
# pleis_v12$direccion <- gsub("\\DGUEZ\\>[.]", "DOMINGUEZ ", pleis_v12$direccion)
# pleis_v12$direccion <- gsub("\\DGUEZ\\>+[[:space:]]", "DOMINGUEZ ", pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<EDIF\\>[.]+[[:space:]]", "EDIFICIO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<EDIF\\>[.]", "EDIFICIO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<EDI\\>[.]+[[:space:]]", "EDIFICIO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<EDI\\>[.]", "EDIFICIO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<EDF\\>[.]+[[:space:]]", "EDIFICIO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<EDF\\>[.]", "EDIFICIO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ED\\>[.]+[[:space:]]", "EDIFICIO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ED\\>[.]", "EDIFICIO ",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<ESCRIT\\>[.]+[[:space:]]", "ESCRITOR ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ESCRIT\\>[.]", "ESCRITOR ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ESCRIT\\>[.]+[[:space:]]", "ESCRITOR ",pleis_v12$direccion)

pleis_v12$direccion <- gsub("FCO+[[:space:]]","FRANCISCO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("FCO[.]+[[:space:]]","FRANCISCO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("FCO[.]","FRANCISCO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<FDEZ\\>+[[:space:]]", "FERNANDEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<FDEZ\\>[.]+[[:space:]]", "FERNANDEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<FDEZ\\>[.]", "FERNANDEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<FDEZ\\>$", "FERNANDEZ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<FED\\>+[.]", "FEDERICO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("FELIP+[[:space:]]","FELIPE ",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<GCIA\\>+[[:space:]]", "GARCIA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<GCIA\\>[.]+[[:space:]]", "GARCIA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<GCIA\\>[.]", "GARCIA ",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<GLEZ\\>+[[:space:]]", "GONZALEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<GLEZ\\>[.]+[[:space:]]", "GONZALEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<GLEZ\\>[.]", "GONZALEZ ",pleis_v12$direccion)

pleis_v12$direccion <- gsub("GRAL[.]+[[:space:]]","GENERAL ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("GRAL[.]","GENERAL ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("GRA[.]+[[:space:]]","GENERAL ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("GRA[.]","GENERAL ",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<HDEZ\\>+[[:space:]]", "HERNANDEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<HDEZ\\>[.]+[[:space:]]", "HERNANDEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<HDEZ\\>[.]", "HERNANDEZ ",pleis_v12$direccion)

pleis_v12$direccion <- gsub("HIST[.]+[[:space:]]","HISTORIADOR ",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("HIST[.]","HISTORIADOR ",pleis_v12$direccion) 

pleis_v12$direccion <- gsub("HNOS[.]+[[:space:]]","HERMANOS ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("HNOS[.]","HERMANOS ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("HNOS+[[:space:]]","HERMANOS ",pleis_v12$direccion)

pleis_v12$direccion <- gsub("ING[.]", "INGENIERO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("INGEN[.]", "INGENIERO ",pleis_v12$direccion)

pleis_v12$direccion <- gsub("JOAQ[.]", "JOAQUIN ",pleis_v12$direccion)

pleis_v12$direccion <- gsub("JURIZMENDI","JUDIZMENDI",pleis_v12$direccion)

pleis_v12$direccion <- gsub("KALEA$","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("\\<KALE\\>","",pleis_v12$direccion) 

pleis_v12$direccion <- gsub("M[ª]", "MARIA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("M[*]", "MARIA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<M\\>[.]", "MARIA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^M+[[:space:]]", "MARIA ",pleis_v12$direccion)   
pleis_v12$direccion <- gsub("[[:space:]]+M+[[:space:]]", "MARIA ",pleis_v12$direccion) 

pleis_v12$direccion <- gsub("\\<MARAGALL\\>", "MARGALL",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<MTNEZ\\>+[[:space:]]", "MARTINEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<MTNEZ\\>[.]+[[:space:]]", "MARTINEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<MTNEZ\\>[.]", "MARTINEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<MNEZ\\>+[[:space:]]", "MARTINEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<MNEZ\\>[.]+[[:space:]]", "MARTINEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<MNEZ\\>[.]", "MARTINEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<MONTSE\\>", "MONTSERRAT",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<N\\>[.]+[[:space:]]+\\<SRA\\>[.]+[[:space:]]", "NUESTRA SEÑORA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<N\\>[.]+[[:space:]]+\\<SRA\\>[.]", "NUESTRA SEÑORA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<N\\>[.]+\\<SRA\\>[.]+[[:space:]]", "NUESTRA SEÑORA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<N\\>[.]+\\<SRA\\>[.]", "NUESTRA SEÑORA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<NTRA\\>[.]+[[:space:]]", "NUESTRA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<NTRA\\>[.]", "NUESTRA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<NTRA\\>+[[:space:]]", "NUESTRA ",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<NTRO\\>[.]+[[:space:]]", "NUESTRO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<NTRO\\>[.]", "NUESTRO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<NTRO\\>+[[:space:]]", "NUESTRO ",pleis_v12$direccion)



pleis_v12$direccion <- gsub("\\<JOSE ORTEGA Y GASSET\\>", "ORTEGA Y GASSET",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<JOSE ORTEGA GASSET\\>", "ORTEGA Y GASSET",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ORTEGA GASSET\\>", "ORTEGA Y GASSET",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<PERIOD\\>+[[:space:]]", "PERIODISTA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PERIOD\\>[.]", "PERIODISTA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PINTOR JOAQUIN SOROLLA\\>", "SOROLLA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PINTOR SOROLLA\\>", "SOROLLA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<JOAQUIN SOROLLA\\>", "SOROLLA",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<REI JAUME I\\>", "JAIME I",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<REY JAIME I\\>", "JAIME I",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<REY JUAN CARLOS I\\>", "JUAN CARLOS I",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<RDGUEZ\\>+[[:space:]]", "RODRIGUEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<RDGUEZ\\>[.]+[[:space:]]", "RODRIGUEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<RDGUEZ\\>[.]", "RODRIGUEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<RGUEZ\\>[.]+[[:space:]]","RODRIGUEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<RGUEZ\\>[.]","RODRIGUEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<RDEZ\\>+[[:space:]]", "RODRIGUEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<RDEZ\\>[.]+[[:space:]]", "RODRIGUEZ ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<RDEZ\\>[.]", "RODRIGUEZ ",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<SRA\\>[.]+[[:space:]]", "SEÑORA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<SRA\\>[.]", "SEÑORA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<SRA\\>+[[:space:]]", "SEÑORA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("SINDICA[.]+[[:space:]]", "SINDICALISTA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("SINDICA+[[:space:]]", "SINDICALISTA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<SINDICA\\>[.]", "SINDICALISTA ",pleis_v12$direccion)

pleis_v12$direccion <- gsub("STOS+[[:space:]]", "SANTOS ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("STOS[.]+[[:space:]]", "SANTOS ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("STOS[.]", "SANTOS ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<STOS\\>", "SANTOS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<STO\\>[.]+[[:space:]]", "SANTO ",pleis_v12$direccion)   
pleis_v12$direccion <- gsub("\\<STO\\>[.]", "SANTO ",pleis_v12$direccion)   
pleis_v12$direccion <- gsub("\\<STO\\>+[[:space:]]", "SANTO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<STA\\>[.]+[[:space:]]", "SANTA ",pleis_v12$direccion)   
pleis_v12$direccion <- gsub("\\<STA\\>[.]", "SANTA ",pleis_v12$direccion)   
pleis_v12$direccion <- gsub("\\<STA\\>+[[:space:]]", "SANTA ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ST\\>+[[:space:]]", "SANTO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ST\\>[.]+[[:space:]]", "SANTO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ST\\>[.]", "SANTO ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<S\\>[.]+[[:space:]]", "SAN ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<S\\>[.]", "SAN ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<S\\>+[[:space:]]", "SAN ",pleis_v12$direccion)



#======================================================================================================
pleis_v12$direccion <- gsub("KM+[[:space:]]+[0-9]+$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("KM-[0-9]+$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<KM\\>$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<LOC\\>[.]", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<L\\>[.]", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+\\-+\\<LOCAL\\>$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+\\-+[[:space:]]+\\<LOCAL\\>$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+[[:space:]]+\\<LOCAL\\>$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+\\-+\\<LOCALES\\>$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+\\-+[[:space:]]+\\<LOCALES\\>$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+[[:space:]]+\\<LOCALES\\>$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<LOCAL\\>$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+\\<B\\>$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+\\<BLOQUE\\>$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+\\<BLQ\\>$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+\\<BLQ\\>[.]$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+\\<BL\\>[-]$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+\\<P\\>[.]$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+\\<P\\>[-]$", "",pleis_v12$direccion)

pleis_v12$direccion <- gsub("[[:space:]]+\\<DELS\\>+[[:space:]]"," DE LOS ",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("[[:space:]]+\\<DEL\\>+[[:space:]]"," ",pleis_v12$direccion)   
pleis_v12$direccion <- gsub("[[:space:]]+\\<DE\\>+[[:space:]]"," ",pleis_v12$direccion) 



#Translates
pleis_v12$direccion <- gsub("\\<ABATXOLO\\>", "ABACHOLO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<AGUSTI\\>", "AGUSTIN",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<AJUNTAMENT\\>","AYUNTAMIENTO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ALACANT\\>", "ALICANTE",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ALBERT\\>", "ALBERTO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ALFONS\\>", "ALFONSO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ALFRED\\>", "ALFREDO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ALINYO\\>", "ALIÑO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ALMOGAVERS\\>", "ALMOGAVERES",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ALPS\\>", "ALPES",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ANNA\\>", "ANA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ANDALUSIA\\>", "ANDALUCIA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ANDREU\\>", "ANDRES",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ANGELS\\>", "ANGELES",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ANSELM\\>", "ANSELMO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ANTIC\\>", "ANTIGUO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ANTIGA\\>", "ANTIGUA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ANTONI\\>", "ANTONIO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ARAGO\\>", "ARAGON",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ARCADI\\>", "ARCADIO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ARQUEBISBE\\>", "ARZOBISPO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ARMADES\\>", "ARMADAS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ARTESANS\\>", "ARTESANOS",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<BAIX\\>", "BAJO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<BASKONIA\\>", "VASCONIA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<BENVINGUTS\\>", "BIENVENIDOS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<BENVINGUT\\>", "BIENVENIDO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<BISBE\\>", "OBISPO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<BISCAIA\\>", "VIZCAYA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<BOTICARI\\>", "BOTICARIO",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<CAMINAS\\>", "CAMINOS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CAMIL\\>", "CAMILO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CAMP\\>", "CAMPO",pleis_v12$direccion)
pleis_v12$direccion  <- gsub("\\<CANALETES\\>", "CANALETAS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CAPITA\\>", "CAPITAN",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CAPUTXINS\\>", "CAPUCHINOS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CARME\\>", "CARMEN",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CARLES\\>", "CARLOS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CASTELA\\>", "CASTILLA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CASTELLO\\>", "CASTELLON",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CATALUNYA\\>", "CATALUÑA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CATOLICS\\>", "CATOLICOS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CIMADEVILA\\>", "CIMADEVILLA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CIRCUMVALACIO\\>", "CIRCUNVALACION",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CIUTAT\\>", "CIUDAD",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CIUTADANS\\>", "CIUDADANOS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<COLOM\\>", "COLON",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<COMERÇ\\>", "COMERCIO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<COMTE\\>", "CONDE",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CONCA\\>", "CUENCA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CONCELLO\\>", "CONCEJO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CONCILI\\>", "CONCILIO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CONSTITUCIO\\>", "CONSTITUCION",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<COR\\>", "CORAZON",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CORNELI\\>", "CORNELIO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CREU\\>", "CRUZ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CRIST\\>", "CRISTO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CRISTOFOR\\>", "CRISTOBAL",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<CRISTOVAL\\>", "CRISTOBAL",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<DESEMPARATS\\>", "DESAMPARADOS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<DEU\\>+[[:space:]]", "DIOS ",pleis_v12$direccion)
# pleis_v12$direccion <- gsub("^\\<12\\>", "DOCE",pleis_v12$direccion)
# pleis_v12$direccion <- gsub("^\\<18\\>", "DIECIOCHO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<DOUTOR\\>", "DOCTOR",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<DIPUTACIO\\>", "DIPUTACION",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<D'ELX\\>", "DE ELCHE",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ELX\\>", "ELCHE",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<EDUARD\\>", "EDUARDO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<EGLESIA\\>", "IGLESIA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ENGINYER\\>", "INGENIERO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ENRIC\\>", "ENRIQUE",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<L'ESGLESIA\\>", "LA IGLESIA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ESGLESIA\\>", "IGLESIA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ESPANYA\\>", "ESPAÑA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ESPiri_v12TO\\>", "ESPiri_v12TU",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<L'ESTACIO\\>", "LA ESTACION",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ESTACIO\\>", "ESTACION",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ESTUDIS\\>", "ESTUDIOS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<EXERCITO\\>", "EJERCITO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<EXERCIT\\>", "EJERCITO",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<FLORS\\>", "FLORES",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<FORCES\\>", "FUERZAS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<FRA\\>", "FRAY",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<FREY\\>", "FRAY",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<FRANCESC\\>", "FRANCISCO",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<GAUDENCI\\>", "GAUDENCIO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<GERVASI\\>", "GERVASIO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<GIRONA\\>", "GERONA",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<HOMENS\\>", "HOMBRES",pleis_v12$direccion)

pleis_v12$direccion <- gsub("[[:space:]]+I+[[:space:]]", " Y ",pleis_v12$direccion) #conjunción copulativa: Pi y Margall, etc.
pleis_v12$direccion <- gsub("\\<IGNASI\\>","IGNACIO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ILLAS\\>","ISLAS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<INDUSTRI\\>","INDUSTRIA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ISIDRE\\>","ISIDRO",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<JACINT\\>","JACINTO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<JAUME\\>","JAIME",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<JOAN\\>", "JUAN",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<JOAQUIM\\>", "JOAQUIN ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<JORDI\\>", "JORGE",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<JULIOL\\>", "JULIO",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<L'ESCORIAL\\>", "EL ESCORIAL",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<LLEIDA\\>", "LERIDA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<LIBERDADE\\>", "LIBERTAD",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<LLIBERTAD\\>", "LIBERTAD",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<LLIBERTAT\\>", "LIBERTAD",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<LLUCIA\\>", "LUCIA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<LLUIS\\>", "LUIS",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<MACIA\\>", "MACIAS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<MAIG\\>", "MAYO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<MAGISTRAT\\>", "MAGISTRADO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<MAIOR\\>", "MAYOR",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<MAJOR\\>", "MAYOR",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<MARE\\>+[[:space:]]", "MADRE ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<MARITIM\\>", "MARITIMO",pleis_v12$direccion) #We replace exactly "maritim" not words which contain "maritim".
pleis_v12$direccion <- gsub("\\<MARIÑA\\>", "MARINA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<MERCAT\\>", "MERCADO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<MESTRE\\>", "MAESTRO",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<NAPOLS\\>", "NAPOLES",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<NATURAIS\\>", "NATURALES",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<NICOLAU\\>", "NICOLAS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<NOVA\\>", "NUEVA",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<ONZE\\>", "ONCE",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ORTIGUEIRA\\>", "ORTIGUERA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<OURENSE\\>", "ORENSE",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<O SABIO\\>", "EL SABIO",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<PALMERES\\>", "PALMERAS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PARAGUAI\\>", "PARAGUAY",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PARALEL\\>", "PARALELO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PARE\\>", "PADRE",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PARLAMENT\\>", "PARLAMENTO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PASQUAL\\>", "PASCUAL",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PASSEIG\\>", "PASEO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PAU\\>", "PAZ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PEDRAIO\\>", "PEDRAYO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PEP\\>", "PEPE",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PERE\\>", "PEDRO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PLATJA\\>", "PLAYA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<POBRESA\\>", "POBREZA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PORT\\>", "PUERTO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PRESIDENT\\>", "PRESIDENTE",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<PRIMER\\>", "PRIMERO",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<REGNE\\>", "REINO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<REIS\\>", "REYES",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<RIU\\>", "RIO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<ROBERT\\>", "ROBERTO",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<SAGRAT\\>", "SAGRADO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<SANT\\>", "SAN",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<SANTISSIMA\\>", "SANTISIMA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<SANTISSIM\\>", "SANTISIMO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<SAVI\\>", "SABIO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<SEBASTIA\\>", "SEBASTIAN",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<SEGON\\>", "SEGUNDO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<SENYORA\\>", "SEÑORA",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<SEPTEMBRE\\>", "SEPTIEMBRE",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<SETEMBRE\\>", "SEPTIEMBRE",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<SETZE\\>", "SIETE",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<TAQUIGRAF\\>", "TAQUIGRAFO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<TARRADELLES\\>", "TARRADELLAS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<TEIXEIRO\\>", "TEIJEIRO",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<TEMPLERS\\>", "TEMPLARIOS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<TORNEIROS\\>", "TORNEROS",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<TRINITAT\\>", "TRINIDAD",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<TRIOMF\\>", "TRIUNFO",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<UNIVERSITAT\\>", "UNIVERSIDAD",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<VERGE\\>", "VIRGEN",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<VICENS\\>", "VICENTE",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<VICENT\\>", "VICENTE",pleis_v12$direccion)

pleis_v12$direccion <- gsub("\\<XAVIER\\>", "JAVIER",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<XOSE\\>", "JOSE",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<XUNQUEIRA\\>", "JUNQUERA",pleis_v12$direccion)



pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.


pleis_v12$direccion <- gsub("\\-+$", "", pleis_v12$direccion)  #=============== We remove the hyphens on the right.
pleis_v12$direccion <- gsub("[[:space:]]+$", "", pleis_v12$direccion)  #=============== We remove all the spaces on the right.
pleis_v12$direccion <- gsub("\\-+$", "", pleis_v12$direccion)  #=============== We remove the hyphens on the right.


pleis_v12$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis_v12$direccion)  
pleis_v12$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis_v12$direccion) 
pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.
pleis_v12$direccion <- gsub("^LA+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^LO+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^EL+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^LAS+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^LOS+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^O+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^OS+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^A+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^AS+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^ELS+[[:space:]]","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("^LES+[[:space:]]","",pleis_v12$direccion)

pleis_v12$direccion <- gsub("^[[:space:]]", "", pleis_v12$direccion)  #=============== We remove the spaces on the left.



#NATIONAL ROADS
pleis_v12$direccion <- gsub("\\<N\\>[.]+[[:space:]]", "NACIONAL ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<N\\>[.]", "NACIONAL ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<N\\>[-]+[[:space:]]", "NACIONAL ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<N\\>[-]", "NACIONAL ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<NACIONAL\\>[-]", "NACIONAL ",pleis_v12$direccion)
pleis_v12$direccion <- gsub("KM[.]+[[:space:]]+[0-9]+$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("\\<KM\\>[.]", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+$", "", pleis_v12$direccion)  #=============== We remove the spaces on the right.


pleis_v12$direccion <- gsub("\\/+[[:space:]]+\\/","",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+$", "", pleis_v12$direccion)  #=============== We remove the spaces on the right.

#===============================================================================================PORTAL=======================================================
pleis_v12$direccion <- gsub("[[:space:]]+\\<P\\>$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+\\<PORTAL\\>[[:space:]]+[0-9]+$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+$", "", pleis_v12$direccion)  #=============== We remove the spaces on the right.


pleis_v12$direccion <- gsub("[[:space:]]+\\<SOLAR\\>+[[:space:]]+[A-Z|0-9]+$", "",pleis_v12$direccion)
pleis_v12$direccion <- gsub("[[:space:]]+$", "", pleis_v12$direccion)  #=============== We remove the spaces on the right.






#===============================================================================End of modifications of addresses ===============================================================0











#We create the candidate key: ensena-ciudad-direccion.
pleis_v12$ensena_ciudad_direccion <- paste(pleis_v12$ensena,"-",pleis_v12$poblacion, "-", pleis_v12$direccion)
iri_v12$ensena_ciudad_direccion <- paste(iri_v12$Enseña,"-",iri_v12$Municipio, "-", iri_v12$Direccion1)

# head(pleis)


#Repeated elements in Pleis and iri_v12 with this candidate key.
repeated_pleis_v12 <- which(duplicated(pleis_v12$ensena_ciudad_direccion))
repeated_iri_v12 <- which(duplicated(iri_v12$ensena_ciudad_direccion))

#To test repeated elements.
repeated_pleis_v12_df <- pleis_v12[repeated_pleis_v12,]
repeated_iri_v12_df < iri_v12[repeated_iri_v12,]

#We remove the repeated elements from Pleis and iri_v12.
pleis_v12_not_rep <- pleis_v12[-(repeated_pleis_v12),]
iri_v12_not_rep <- iri_v12[-(repeated_iri_v12),]

# View(head(iri_v12_not_rep)) #traza

#TRAZA:
#iri_v12_not_rep$Direccion1[iri_v12_not_rep$`ID Tienda`==4727]

#We merge the two data frames by the candidate key.
# merge_iri_v12_pleis <- merge(pleis_not_rep,iri_v12_not_rep, by.x = "ensena_cp_direccion", by.y = "ensena_cp_direccion")
merge_iri_v12_pleis_v12 <- merge(pleis_v12_not_rep,iri_v12_not_rep, by.x = "ensena_ciudad_direccion", by.y = "ensena_ciudad_direccion")









write.csv(merge_iri_v12_pleis_v12, file = "merge_iri_v12_pleis_v12_24_09.csv")

# View(merge_iri_v12_pleis_v12)

#TRAZA:
#merge_iri_v12_pleis$Direccion1[merge_iri_v12_pleis$`ID Tienda`==4727]

#View(merge_iri_v12_pleis) #traza

#We obtain the indexed for duplicate values in ensena_cp_direccion with different "ID Tienda". = 0 elements.
# which(duplicated(merge_iri_v12_pleis$`ID Tienda`)) 


#Left join, with dplyr, (with basis package doesn't run because they have different number of rows), to obtain all observations
#of Pleis and only their matches with iri_v12s.
# pleis_not_rep$m_code <- "pleis"
# # iri_v12_not_rep$m_code <- "iri_v12"
# iri_v12_not_rep$m_code <- "iri_v12"
# merge_iri_v12_pleis_pleis <- left_join(pleis_not_rep, iri_v12_not_rep, by = "ensena_cp_direccion")
merge_iri_v12_pleis_v12_pleis_v12 <- left_join(pleis_v12_not_rep, iri_v12_not_rep, by = "ensena_ciudad_direccion")
#View(merge_iri_v12_pleis_pleis) #traza

#TRAZA:
#unique(merge_iri_v12_pleis_pleis$Direccion1[merge_iri_v12_pleis_pleis$`ID Tienda`==4727])

#List of repeated elements at the side of pleis, (0 elements)
# repeated_pleis_merge <- which(duplicated(merge_iri_v12_pleis_pleis$ensena_cp_direccion))


#Right join between Pleis and iri_v12s to obtain all observations of iri_v12s and only their matches with Pleis.
# merge_iri_v12_pleis_iri_v12 <- right_join(pleis_not_rep, iri_v12_not_rep, by = "ensena_cp_direccion")
merge_iri_v12_pleis_v12_iri_v12 <- right_join(pleis_v12_not_rep, iri_v12_not_rep, by = "ensena_ciudad_direccion")
#View(merge_iri_v12_pleis_v12_iri_v12) #traza

#List of repeated elements in iri_v12, (0 elements)
# repeated_iri_v12 <- which(duplicated(merge_iri_v12_pleis_v12_iri_v12$ensena_cp_direccion))

#We need to do a "union", (similar to SQL union) of data frames.
#We add a new field to know whom belong each observation in the union.
pleis_v12_iri_v12_all <- rbind(merge_iri_v12_pleis_v12_pleis_v12, merge_iri_v12_pleis_v12_iri_v12) 

#View(pleis_v12_iri_v12_all)
#TRAZA:
#unique(pleis_v12_iri_v12_all$Direccion1[pleis_v12_iri_v12_all$`ID Tienda`==4727])


repeated_pleis_v12_iri_v12_all <- which(duplicated(pleis_v12_iri_v12_all$ensena_ciudad_direccion)) #It is the number of common observations in pleis_v12 and iri_v12. This number has to be equal
#to the number of observations of "merge_iri_v12_pleis".

#=====================================================================================================
#We are going to prepare data for looking at them in Qlik.
#We are going to extract two files with the elements that are in Pleis and in iri_v12 but without common elements.

#We are going to match by provincia and poblacion; for this we need to adapt the provincia in Pleis.
# merge_iri_v12_pleis_pleis$provincia <- gsub("ALICANTE/ALACANT", "ALICANTE", merge_iri_v12_pleis_pleis$provincia)
# merge_iri_v12_pleis_pleis$provincia <- gsub("BALEARS (ILLES)", "BALEARES", merge_iri_v12_pleis_pleis$provincia)
# merge_iri_v12_pleis_pleis$provincia <- gsub("ARABA/ALAVA", "ALAVA", merge_iri_v12_pleis_pleis$provincia)
# merge_iri_v12_pleis_pleis$provincia <- gsub("BIZKAIA", "VIZCAYA", merge_iri_v12_pleis_pleis$provincia)
# merge_iri_v12_pleis_pleis$provincia <- gsub("CASTELLON/CASTELLO", "CASTELLON", merge_iri_v12_pleis_pleis$provincia)
# merge_iri_v12_pleis_pleis$provincia <- gsub("CORUÑA (A)", "A CORUÑA", merge_iri_v12_pleis_pleis$provincia)
# merge_iri_v12_pleis_pleis$provincia <- gsub("RIOJA (LA)", "LA RIOJA", merge_iri_v12_pleis_pleis$provincia)
# merge_iri_v12_pleis_pleis$provincia <- gsub("VALENCIA/VALÈNCIA", "VALENCIA", merge_iri_v12_pleis_pleis$provincia)
# 
# merge_iri_v12_pleis_iri_v12$provincia <- gsub("CORUÑA (A)", "A CORUÑA", merge_iri_v12_pleis_iri_v12$provincia)
# merge_iri_v12_pleis_iri_v12$Municipio <- gsub("CORUÑA (A)", "A CORUÑA", merge_iri_v12_pleis_iri_v12$Municipio)


# View(merge_iri_v12_pleis_iri_v12[grepl("LEON",merge_iri_v12_pleis_iri_v12$provincia),]) #Test
# View(merge_iri_v12_pleis_iri_v12[grepl("CORUÑA",merge_iri_v12_pleis_iri_v12$provincia),])   
# View(merge_iri_v12_pleis_pleis[grepl("CORUÑA",merge_iri_v12_pleis_pleis$poblacion),])   
# 
# View(merge_iri_v12_pleis_pleis[grepl("5934",merge_iri_v12_pleis_pleis$cod),])   






#Elements in Pleis that not are common with iri_v12.
length(merge_iri_v12_pleis_v12_pleis_v12$`ID Tienda`[is.na(merge_iri_v12_pleis_v12_pleis_v12$`ID Tienda`)])
pleis_v12_not_common_elements <- merge_iri_v12_pleis_v12_pleis_v12[is.na(merge_iri_v12_pleis_v12_pleis_v12$`ID Tienda`),]
#We add the next line to drawing the map in Qlik.
# pleis_v12_not_common_elements$CountryDataSourde <- rep("Spain")
# write.csv(pleis_v12_not_common_elements, file = "pleis_v12_not_common_elements_v12.csv")

#Elements in iri_v12 that not are common with pleis_v12.
length(merge_iri_v12_pleis_v12_iri_v12$cod[is.na(merge_iri_v12_pleis_v12_iri_v12$cod)])
iri_v12_not_common_elements <- merge_iri_v12_pleis_v12_iri_v12[is.na(merge_iri_v12_pleis_v12_iri_v12$cod),]
#We add the next line to drawing the map in Qlik.
# iri_v12_not_common_elements$CountryDataSourde <- rep("Spain")
# write.csv(iri_v12_not_common_elements, file = "iri_v12_not_common_elements_v12.csv")




#We are going to insert in a csv file the Pleis' cod and Id Tienda from iri_v12, to add to SQL Server.
# write.csv(merge_iri_v12_pleis[,c("cod.x", "ID.Tienda.y")], file = "codes_pleis_iri_v12_v12.csv")

# write.csv(repeated_pleis_df[,c("cod")], file = "repeated_elements_pleis_v12.csv")



# View(merge_iri_v12_pleis_v12[grepl("HIPERBER",merge_iri_v12_pleis_v12$ensena_ciudad_direccion),c("cod.x","ID.Tienda.y")])
# 
# write.csv(merge_iri_v12_pleis_v12[grepl("DIA",merge_iri_v12_pleis_v12$ensena.x),c("cod.x","ID.Tienda.y")],file="dia_codes.csv")

# View(merge_iri_v12_pleis_pleis[grepl("5119",merge_iri_v12_pleis_pleis$cod),cod.x])

# repeated_pleis_df$cod



