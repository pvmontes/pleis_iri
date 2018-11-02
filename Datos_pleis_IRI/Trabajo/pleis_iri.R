#This version is a copy of data_analysis_v11.R but in this case we combine some centers from UNIDE.

#install.packages("readxl")
#install.packages("dplyr")

# library(readxl)
# library(dplyr)
# # #
# setwd("C:/Users/maria.purificacion/Documents/Datos_pleis_IRI/Trabajo")
#getwd()

#==================================================================================================


pleis <- read_excel("mis_centros_pleis_Espana_1_a_5_sin_canarias.xlsx")   
iri <- read_excel("mis_directorio IRI p4 18.xlsx")

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
iri$Direccion1 <- toupper(iri$Direccion1)
iri$Enseña <- toupper(iri$Enseña)
iri$Rótulo <- toupper(iri$Rótulo)
iri$Provincia <- toupper(iri$Provincia)
iri$Municipio <- toupper(iri$Municipio)
pleis$direccion <- toupper(pleis$direccion)
pleis$provincia <- toupper(pleis$provincia)
pleis$poblacion <- toupper(pleis$poblacion)


#We remove all the commas from the addresses.
iri$Direccion1 <- gsub(",","",iri$Direccion1)
pleis$direccion <- gsub(",","",pleis$direccion)


#We remove the bars.
iri$Direccion1 <- gsub("[[:space:]]+/+[[:space:]]+/+[[:space:]]+/$","",iri$Direccion1) 
iri$Direccion1 <- gsub("/+[[:space:]]+/+[[:space:]]+/$","",iri$Direccion1) 
pleis$direccion <- gsub("[[:space:]]+/+[[:space:]]+/+[[:space:]]+/$","",pleis$direccion) 
pleis$direccion <- gsub("/+[[:space:]]+/+[[:space:]]+/$","",pleis$direccion) 

#We remove the accents.
iri$Enseña <- gsub("Á", "A", iri$Enseña)
iri$Enseña <- gsub("É", "E", iri$Enseña)
iri$Enseña <- gsub("Í", "I", iri$Enseña)
iri$Enseña <- gsub("Ó", "O", iri$Enseña)
iri$Enseña <- gsub("Ú", "U", iri$Enseña)
iri$Enseña <- gsub("Ü", "U", iri$Enseña)
iri$Rótulo <- gsub("Á", "A", iri$Rótulo)
iri$Rótulo <- gsub("É", "E", iri$Rótulo)
iri$Rótulo <- gsub("Í", "I", iri$Rótulo)
iri$Rótulo <- gsub("Ó", "O", iri$Rótulo)
iri$Rótulo <- gsub("Ú", "U", iri$Rótulo)
iri$Rótulo <- gsub("Ü", "U", iri$Rótulo)
pleis$ensena <- gsub("Á", "A", pleis$ensena)
pleis$ensena <- gsub("É", "E", pleis$ensena)
pleis$ensena <- gsub("Í", "I", pleis$ensena)
pleis$ensena <- gsub("Ó", "O", pleis$ensena)
pleis$ensena <- gsub("Ú", "U", pleis$ensena)
pleis$ensena <- gsub("Ü", "U", pleis$ensena)

#Acento invertido
iri$Enseña <- gsub("À", "A", iri$Enseña)
iri$Enseña <- gsub("È", "E", iri$Enseña)
iri$Enseña <- gsub("Ì", "I", iri$Enseña)
iri$Enseña <- gsub("Ò", "O", iri$Enseña)
iri$Enseña <- gsub("Ù", "U", iri$Enseña)
iri$Rótulo <- gsub("À", "A", iri$Rótulo)
iri$Rótulo <- gsub("È", "E", iri$Rótulo)
iri$Rótulo <- gsub("Ì", "I", iri$Rótulo)
iri$Rótulo <- gsub("Ò", "O", iri$Rótulo)
iri$Rótulo <- gsub("Ù", "U", iri$Rótulo)
pleis$ensena <- gsub("À", "A", pleis$ensena)
pleis$ensena <- gsub("È", "E", pleis$ensena)
pleis$ensena <- gsub("Ì", "I", pleis$ensena)
pleis$ensena <- gsub("Ò", "O", pleis$ensena)
pleis$ensena <- gsub("Ù", "U", pleis$ensena)



#======================================================================================================
#Functions
function_replace_string_ensena <- function(m_vector, string) { 
  pleis[m_vector,"ensena"] <<- c(string) 
}

function_rotulo <- function(m_vector) {
  iri[m_vector,"Enseña"] <<- iri[m_vector,"Rótulo"] 
}

function_cadena_pleis <- function(p_vector) {
  pleis[p_vector,"ensena"] <<- pleis[p_vector,"cadena"]
}

function_unide <- function(m_vector) {
  pleis[m_vector,"ensena"] <<- c("UNIDE, S. COOP.")
}

function_la_despensa <- function(m_vector) {
  pleis[m_vector,"ensena"] <<- c("ECO-MORA")
}

function_leclerc <- function(m_vector){
  pleis[m_vector, "ensena"] <<- c("LECLERC")
}

function_lider_pleis <- function(m_vector){
  pleis[m_vector, "ensena"] <<- c("LIDER-ALIMENT")
}

function_lider_iri <- function(m_vector){
  iri[m_vector, "Enseña"] <<- c("LIDER-ALIMENT")
}


#========================================================================================================
#Eroski
iri$Enseña <- gsub("EROSKI+[[:space:]]+[A-Z]+$","EROSKI", iri$Enseña)
pleis$ensena <- gsub("EROSKI+[[:space:]]+[A-Z]+$","EROSKI", pleis$ensena)
pleis$ensena <- gsub("EROSKI+\\-+[A-Z]+","EROSKI", pleis$ensena)
index_ensena_cadena_eroski <- which(pleis$cadena == "GRUPO EROSKI" & pleis$ensena == "FAMILIA") #To replace the function change_some_fields()

function_replace_string_ensena(index_ensena_cadena_eroski, "EROSKI") 
index_franquicia_eroski <- which(iri$Enseña == "FRANQUICIA EROSKI")
#=======================================================================================================
#El Corte Ingles
pleis$ensena <- gsub("TIENDAS ECI","EL CORTE INGLES", pleis$ensena)
pleis$ensena <- gsub("CONVENIENCE STORE","EL CORTE INGLES", pleis$ensena)
pleis$ensena <- gsub("HIPERCOR","EL CORTE INGLES", pleis$ensena)
pleis$ensena <- gsub("OPENCOR","EL CORTE INGLES", pleis$ensena)
pleis$ensena <- gsub("SUPERCOR EXPRES","EL CORTE INGLES", pleis$ensena)
pleis$ensena <- gsub("SUPERCOR","EL CORTE INGLES", pleis$ensena)
pleis$ensena <- gsub("REPSOL SUPERCOR","EL CORTE INGLES", pleis$ensena)
iri$Enseña <- gsub("HIPERCOR","EL CORTE INGLES", iri$Enseña)
iri$Enseña <- gsub("SUPERCOR","EL CORTE INGLES", iri$Enseña)
#=======================================================================================================
#GADISA
iri$Enseña <- gsub("GADISA","GADIS", iri$Enseña)
# pleis$ensena <- gsub("CLAUDIO","GADIS",pleis$ensena) 
index_claudio <- which(iri$Rótulo == "CLAUDIO")
#=======================================================================================================
#Carrefour
pleis$ensena <- gsub("CARREFOUR+[[:space:]]+[A-Z]+$","CARREFOUR", pleis$ensena)
pleis$ensena <- gsub("SUPECO","CARREFOUR", pleis$ensena)
iri$Enseña <- gsub("CARREFOUR SUPER","CARREFOUR", iri$Enseña)
#=======================================================================================================
#Dia
iri$Enseña <- gsub("^[A-Z]+[[:space:]]+DIA","DIA",iri$Enseña) #We remove MAXI from MAXI DIA in Iri/Enseña.
pleis$ensena <- gsub("DIA+[[:space:]]+[A-Z]+$","DIA", pleis$ensena)
pleis$ensena <- gsub("^[A-Z]+[[:space:]]+DIA","DIA",pleis$ensena) #We remove MAXI from MAXI DIA in Iri/Enseña.
pleis$ensena <- gsub("LA PLAZA DE DIA","DIA", pleis$ensena)
#=======================================================================================================
#El Arbol
index_arbol <- which(iri$Rótulo == "EL ARBOL")
#=======================================================================================================
#Alcampo
pleis$ensena <- gsub("ARO ROJO","SABECO",pleis$ensena)
pleis$ensena <- gsub("HIPER SIMPLY","SABECO",pleis$ensena)
pleis$ensena <- gsub("SIMPLY BASIC","SABECO",pleis$ensena)
pleis$ensena <- gsub("SIMPLY CITY","SABECO",pleis$ensena)
pleis$ensena <- gsub("SIMPLY MARKET","SABECO",pleis$ensena)
pleis$ensena <- gsub("SIMPLY STORE","SABECO",pleis$ensena)
iri$Enseña <- gsub("ALCAMPO CITY", "ALCAMPO", iri$Enseña)
#=======================================================================================================
#7 alimentacion 7, S.A., Supermercados Ribetans
pleis$ensena <- gsub("COALIMENT","COVALCO-COALIMENT",pleis$ensena)
pleis$ensena <- gsub("COALIMENT COMPRA SALUDABLE","COVALCO-COALIMENT",pleis$ensena)
#index_coaliment <- which(iri$Rótulo == "COALIMENT")
#index_coaliment_compra_saludable <- which(iri$Rótulo == "COALIMENT COMPRA SALUDABLE")
#=======================================================================================================
#Ahorraves
index_ahorraves <- which(iri$Rótulo == "AHORRAVES")
#=======================================================================================================
#Salinas
index_salinas <- which(iri$Rótulo == "SALINAS")
#=======================================================================================================
#Pincha precios
index_pincha_precios <- which(iri$Rótulo == "PINCHA PRECIOS")
#=======================================================================================================
#Super Ecozar
index_ecozar <- which(iri$Rótulo == "SUPERMERCADOS ECOZAR")
function_rotulo(index_ecozar)
pleis$ensena <- gsub("SUP.ECOZAR","ECOZAR",pleis$ensena)
#=======================================================================================================
#KOMO-KOMO
index_komo_komo <- which(iri$Rótulo == "KOMO-KOMO")
#=======================================================================================================
#La compra
index_la_compra <- which(iri$Rótulo == "LA COMPRA")
#=======================================================================================================
#Vivo
index_vivo <- which(iri$Rótulo == "VIVO - MARTINEZ")
function_rotulo(index_vivo)
iri$Enseña <- gsub("VIVO - MARTINEZ", "VIVO", iri$Enseña)
#=======================================================================================================
#Darvi, Iberplus
index_darvi <- which(iri$Rótulo == "DARVI")
index_iberplus <- which(iri$Rótulo == "IBERPLUS")
#=======================================================================================================
#Autoservicio El 66
index_el_66 <- which(iri$Rótulo == "AUTOSERVICIO EL 66")
function_rotulo(index_el_66)
pleis$ensena <- gsub("AUTOSERVIC 66","AUTOSERVICIO EL 66",pleis$ensena)
#=======================================================================================================
#Todo Todo
index_todo_todo_romen <- which(iri$Rótulo == "TODO-TODO ROMEN") #| iri$Rótulo == "TODO-TODO OTROS")
function_rotulo(index_todo_todo_romen)
iri$Enseña <- gsub("TODO-TODO ROMEN", "TODO-TODO", iri$Enseña)
index_todo_todo_otros <- which(iri$Rótulo == "TODO-TODO OTROS") #| iri$Rótulo == "TODO-TODO OTROS")
function_rotulo(index_todo_todo_otros)
iri$Enseña <- gsub("TODO-TODO OTROS", "TODO-TODO", iri$Enseña)
pleis$ensena <- gsub("TODO TODO","TODO-TODO",pleis$ensena)
pleis$ensena <- gsub("SPAR TODO-TODO","TODO-TODO",pleis$ensena)
pleis$ensena <- gsub("VIVO-TODOTODO","TODO-TODO",pleis$ensena)   #I don't know if there are match with these centers. I see that not.
#=======================================================================================================
#La lonja
index_la_lonja <- which(iri$Rótulo == "LA LONJA")
#=======================================================================================================
#Cabrero e hijos
index_cabrero_e_hijos <- which(pleis$cadena == "CABRERO E HIJOS, S.A.")
function_cadena_pleis(index_cabrero_e_hijos)
pleis$ensena <- gsub("CABRERO E HIJOS, S.A.","CABRERO E HIJOS",pleis$ensena)
#=======================================================================================================
#Maxcoop
# index_maxcoop <- which(iri$Rótulo == "MAXCOOP")
#=======================================================================================================
#El tostadero
index_el_tostadero <- which(iri$Rótulo == "SUPERMERCADOS EL TOSTADERO")
function_rotulo(index_el_tostadero)
iri$Enseña <- gsub("SUPERMERCADOS EL TOSTADERO", "EL_TOSTADERO", iri$Enseña)
pleis$ensena <- gsub("EL TOSTADERO","EL_TOSTADERO",pleis$ensena)
#=======================================================================================================
#Supersur y Don Market
index_supersur <- which(iri$Rótulo == "SUPER SUR")
function_rotulo(index_supersur)
iri$Enseña <- gsub("SUPER SUR", "SUPERSUR", iri$Enseña)
index_don_market <- which(iri$Rótulo == "DON MARKET")
#=======================================================================================================
#Cash Basauri
index_basauri <- which(iri$Rótulo == "SUPERMERCADO BASAURI")
function_rotulo(index_basauri)
iri$Enseña <- gsub("SUPERMERCADO BASAURI", "BASAURI", iri$Enseña)
#=======================================================================================================
#Cash Lepe
index_cash_lepe <- which(iri$Rótulo == "EL JAMON")
#=======================================================================================================
#Eko ama
index_eko_ama <- which(iri$Rótulo == "EKO AMA")
function_rotulo(index_eko_ama)
iri$Enseña <- gsub("EKO AMA", "EKO-AMA", iri$Enseña)
#=======================================================================================================
#Froiz
#v9
# index_super_froiz <- which(iri$Rótulo == "SUPERMERCADOS FROIZ")
# function_rotulo(index_super_froiz)
# iri$Enseña <- gsub("SUPERMERCADOS FROIZ", "SUPERMERCADOS-FROIZ")
# pleis$ensena <- gsub("SUPER FROIZ","SUPERMERCADOS-FROIZ",pleis$ensena)
# index_tandy <- which(iri$Rótulo == "TANDY-FROIZ")
# function_rotulo(index_tandy)


# pleis$ensena <- gsub("TANDY","TANDY-FROIZ",pleis$ensena)

#v8 , (lo comento el 22/08/2018)
# index_super_froiz <- which(iri$Rótulo == "SUPERMERCADOS FROIZ")
# function_rotulo(index_super_froiz)
# pleis$ensena <- gsub("SUPER FROIZ","SUPERMERCADOS FROIZ",pleis$ensena)
# index_tandy <- which(iri$Rótulo == "TANDY-FROIZ")
# function_rotulo(index_tandy)
# #v9 new (lo comento el 22/08/2018)
# iri$Enseña <- gsub("TANDY-FROIZ", "TANDY", iri$Enseña)

#new version: 22/08/2018
index_froiz_pleis <- which(pleis$cadena == "GRUPO FROIZ")
function_cadena_pleis(index_froiz_pleis)
pleis$ensena <- gsub("GRUPO FROIZ", "FROIZ", pleis$ensena)
iri$Enseña <- gsub("S. FROIZ", "FROIZ", iri$Enseña)


#=======================================================================================================
#Gigante
index_gigante <- which(iri$Rótulo == "GIGANTE")
#=======================================================================================================
#Tradys
index_tradys <- which(iri$Rótulo == "TRADY'S")
function_rotulo(index_tradys)
iri$Enseña <- gsub("TRADY'S", "TRADYS", iri$Enseña)
#=======================================================================================================
#Comerco ---- PRUEBA
#pleis$ensena <- gsub("COMERCO", "COVALCO-COALIMENT", pleis$ensena)  #No modifica nada.
#=======================================================================================================
#Novavenda
index_novavenda <- which(iri$Rótulo == "NOVAVENDA")
#=======================================================================================================
#Petit preu
index_petit_preu <- which(iri$Rótulo == "PETIT PREU")
function_rotulo(index_petit_preu)
iri$Enseña <- gsub("PETIT PREU", "PETIT-PREU", iri$Enseña)
pleis$ensena <- gsub("PETIT PREU", "PETIT-PREU", pleis$ensena)
#=======================================================================================================
#Supermercados Piedra
index_prieda <- which(iri$Rótulo == "SUPERMERCADOS PIEDRA")
function_rotulo(index_prieda)
iri$Enseña <- gsub("SUPERMERCADOS PIEDRA", "SUPER PIEDRA", iri$Enseña)
#=======================================================================================================
#JR Supermercats
index_jr_supermercats <- which(iri$Rótulo == "J.R. SUPERMERCATS")   #J.R. Supermercats
function_rotulo(index_jr_supermercats)
iri$Enseña <- gsub("J.R. SUPERMERCATS", "JR-SUPERMERCAT", iri$Enseña)
pleis$ensena <- gsub("JR SUPERMERCAT", "JR-SUPERMERCAT", pleis$ensena)
#======================================================================================================
#Super Alba
index_super_alba <- which(iri$Rótulo == "SUPER ALBA")
function_rotulo(index_super_alba)
iri$Enseña <- gsub("SUPER ALBA", "SUPER-ALBA", iri$Enseña)
pleis$ensena <- gsub("SUPER ALBA", "SUPER-ALBA", pleis$ensena)
#======================================================================================================
#Unico
index_unico <- which(iri$Rótulo == "UNICO")
#======================================================================================================
#CONSUM
index_consum <- which(pleis$cadena == "CONSUM, SDAD.COOP.")
function_replace_string_ensena(index_consum, "CONSUM-SC")
iri$Enseña <- gsub("CONSUM SC", "CONSUM-SC", iri$Enseña)
#======================================================================================================
#Alsara
index_alsara <- which(iri$Rótulo == "ALSARA")
#======================================================================================================
#Coop-Consumo
index_coop_consumo <- which(iri$Rótulo == "COOP. CONSUMO")
function_rotulo(index_coop_consumo)
iri$Enseña <- gsub("COOP. CONSUMO", "COOP-CONSUMO", iri$Enseña)
pleis$ensena <- gsub("COOP.CONSUMO", "COOP-CONSUMO", pleis$ensena)
#======================================================================================================
#Disara
index_disara <- which(iri$Rótulo == "DISARA")
#======================================================================================================
#Super siete villas
index_siete_villas <- which(iri$Rótulo == "SUPER SIETE VILLAS")
function_rotulo(index_siete_villas)
iri$Enseña <- gsub("SUPER SIETE VILLAS", "SUPER-SIETE-VILLAS", iri$Enseña)
pleis$ensena <- gsub("SUPER 7 VILLAS", "SUPER-SIETE-VILLAS", pleis$ensena)
#======================================================================================================
#Super Alcoop
index_alcoop <- which(iri$Rótulo == "SUPER ALCOOP")
function_rotulo(index_alcoop)
iri$Enseña <- gsub("SUPER ALCOOP", "SUPER-ALCOOP", iri$Enseña)
pleis$ensena <- gsub("SUPER ALCOOP", "SUPER-ALCOOP", pleis$ensena)
#======================================================================================================
#Urbasa
index_urbasa <- which(iri$Rótulo == "URBASA")
#======================================================================================================
#Santa Mª Magdalena
index_m_magdalena <- which(iri$Rótulo == "SANTA MARIA MAGDALENA")
function_rotulo(index_m_magdalena)
iri$Enseña <- gsub("SANTA MARIA MAGDALENA", "SANTA-MARIA-MAGDALENA", iri$Enseña)
pleis$ensena <- gsub("STA[.]M[*] MAGDALE", "SANTA-MARIA-MAGDALENA", pleis$ensena)   
#======================================================================================================
#Super Cordoba
index_super_cordoba <- which(iri$Rótulo == "SUPERSERVICIO CORDOBA")
function_rotulo(index_super_cordoba)
iri$Enseña <- gsub("SUPERSERVICIO CORDOBA", "SUPERSERVICIO-CORDOBA", iri$Enseña)
pleis$ensena <- gsub("SUPER[.] CORDOBA", "SUPERSERVICIO-CORDOBA", pleis$ensena)   
#======================================================================================================
#Super Navarrete
index_super_navarrete <- which(iri$Rótulo == "SUPERMERCADO NAVARRETE")
function_rotulo(index_super_navarrete)
iri$Enseña <- gsub("SUPERMERCADO NAVARRETE", "SUPERMERCADO-NAVARRETE", iri$Enseña)
pleis$ensena <- gsub("SUP[.]NAVARRETE", "SUPERMERCADO-NAVARRETE", pleis$ensena)   
#======================================================================================================
#Super Bonarea
index_super_bonarea <- which(iri$Rótulo == "SUPER BONAREA")
function_rotulo(index_super_bonarea)
iri$Enseña <- gsub("SUPER BONAREA", "SUPER-BONAREA", iri$Enseña)
pleis$ensena <- gsub("SUPER BONAREA", "SUPER-BONAREA", pleis$ensena) 
#======================================================================================================
#Coviran
index_coviran <- which(pleis$cadena == "COVIRAN, S.C.A.")   
function_cadena_pleis(index_coviran)
pleis$ensena <- gsub("COVIRAN, S.C.A.", "COVIRAN", pleis$ensena)   
#======================================================================================================
#Del Rio
index_del_rio <- which(iri$Rótulo == "SUPERMERCADOS DEL RIO")
function_rotulo(index_del_rio)
iri$Enseña <- gsub("SUPERMERCADOS DEL RIO", "SUPERMERCADOS-DEL-RIO", iri$Enseña)
pleis$ensena <- gsub("DEL RIO", "SUPERMERCADOS-DEL-RIO", pleis$ensena)
#======================================================================================================
#Super Huracan
# index_huracan <- which(iri$Rótulo == "SUPER HURACAN")
# function_rotulo(index_huracan)
# iri$Enseña <- gsub("SUPER HURACAN", "SUPER-HURACAN", iri$Enseña)
# pleis

#======================================================================================================
#SPAR
index_spar <- which(iri$Rótulo == "ANTIGÜA SPAR - POMARES" | iri$Rótulo == "SPAR - DOMINGO" | iri$Rótulo == "SPAR - EXCLUIB" |
                      iri$Rótulo == "SPAR - FRAGADIS" | iri$Rótulo == "SPAR - INSULAR" | 
                      iri$Rótulo == "SPAR - LIDER AL" |
                      iri$Rótulo == "SPAR - MIQUEL" | iri$Rótulo == "SPAR - MOLDES" | iri$Rótulo == "SPAR - OTROS" |
                      iri$Rótulo == "SPAR - POMARES" | iri$Rótulo == "SPAR - ROMEN" | iri$Rótulo == "SPAR - UPPER" |
                      iri$Rótulo == "SPAR - VALVI" | iri$Rótulo == "SPAR EXPRESS - LIDER AL" | iri$Rótulo == "SPAR EXPRESS - UPPER" |
                      iri$Rótulo == "SPAR OTROS" | iri$Rótulo == "SUPER SPAR - OTROS")

function_rotulo(index_spar)
iri$Enseña <- gsub("ANTIGÜA SPAR - POMARES" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SPAR - DOMINGO" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SPAR - EXCLUIB" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SPAR - FRAGADIS" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SPAR - INSULAR" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SPAR - LIDER AL" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SPAR - MIQUEL" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SPAR - MOLDES" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SPAR - OTROS" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SPAR - POMARES" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SPAR - ROMEN" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SPAR - UPPER" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SPAR - VALVI" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SPAR EXPRESS - LIDER AL" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SPAR EXPRESS - UPPER" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SPAR OTROS" , "SPAR", iri$Enseña)
iri$Enseña <- gsub("SUPER SPAR - OTROS" , "SPAR", iri$Enseña)

pleis$ensena <- gsub("SPAR EXPRESS" , "SPAR", pleis$ensena)
pleis$ensena <- gsub("SPAR TODO-TODO" , "SPAR", pleis$ensena)
pleis$ensena <- gsub("SUPER SPAR" , "SPAR", pleis$ensena)
pleis$ensena <- gsub("SUPERSPAR" , "SPAR", pleis$ensena)
pleis$ensena <- gsub("VALVI-EUROSPAR" , "SPAR", pleis$ensena)

#====================================================================================================== 
#Deza
iri$Enseña <- gsub("DEZA ALIMENTACION", "DEZA", iri$Enseña)
#======================================================================================================  
#Binipreu
index_binipreu <- which(iri$Rótulo == "BINIPREU MERCAT" | iri$Rótulo == "BINIPREU")
function_rotulo(index_binipreu)   
iri$Enseña <- gsub("BINIPREU MERCAT" , "BINIPREU", iri$Enseña)
#======================================================================================================
#Disbo
index_disbo <- which(iri$Rótulo == "DISBO")
#====================================================================================================== 
#Eco mora
index_ecomora <- which(pleis$cadena == "ECO MORA, S.A.")
function_cadena_pleis(index_ecomora)
pleis$ensena <- gsub("ECO MORA, S.A.", "ECO-MORA", pleis$ensena)

iri$Enseña <- gsub("ECO MORA, S.A. \\(HIPER MANACOR\\)", "ECO-MORA", iri$Enseña)  #Eco Mora, S.A. (Hiper Manacor)   #ECO MORA, S.A. (HIPER MANACOR)




#Alternativa para La Despensa, (obtenemos mejores resultados con el código de arriba):
# index_la_despensa <- which(iri$Rótulo == "LA DESPENSA" | iri$Rótulo == "LA DESPENSA EXPRESS")
# function_rotulo(index_la_despensa)
# iri$Enseña <- gsub("LA DESPENSA", "LA-DESPENSA", iri$Enseña)
# iri$Enseña <- gsub("LA DESPENSA EXPRESS", "LA-DESPENSA", iri$Enseña)
# pleis$ensena <- gsub("LA DESPENSA", "LA-DESPENSA", pleis$ensena)
# pleis$ensena <- gsub("LA DESPENSA EX", "LA-DESPENSA", pleis$ensena)

#Para los centros cuya enseña es La Despensa pero la cadena no es Eco Mora.
# index_san_rafael <- which(pleis$cadena == "COOPERATIVA SAN RAFAEL S." & pleis$ensena == "LA DESPENSA")
# index_despensa_castilla_mancha <- which(pleis$cadena == "LA DESPENSA DE CASTILLA LA MANCHA, S.L." & pleis$ensena == "LA DESPENSA")
# index_independiente <- which(pleis$cadena == "INDEPENDIENTE" & pleis$ensena == "LA DESPENSA")
#Lo mismo pero en una línea.
index_no_eco_mora <- which(pleis$cadena != "ECO MORA, S.A." & pleis$ensena == "LA DESPENSA")
function_la_despensa(index_no_eco_mora)
#======================================================================================================
#HUNOSA
index_hunosa <- which(iri$Rótulo == "ECONOMATOS DE HUNOSA")
function_rotulo(index_hunosa)
pleis$ensena <- gsub("ECO. HUNOSA", "HUNOSA", pleis$ensena)
iri$Enseña <- gsub("ECONOMATOS DE HUNOSA", "HUNOSA", iri$Enseña)
#======================================================================================================
#Bip bip
pleis$ensena <- gsub("BIP BIP", "BIP-BIP", pleis$ensena)
index_bip_bip_ecos <- which(iri$Rótulo == "BIP-BIP - ECOS")
function_rotulo(index_bip_bip_ecos)
iri$Enseña <- gsub("BIP-BIP - ECOS", "BIP-BIP", iri$Enseña)

index_bip_bip_garcia_romo <- which(iri$Rótulo == "BIP-BIP - GARCIA ROMO")
function_rotulo(index_bip_bip_garcia_romo)
iri$Enseña <- gsub("BIP-BIP - GARCIA ROMO", "BIP-BIP", iri$Enseña)

index_bip_bip_moya <- which(iri$Rótulo == "BIP-BIP MOYA")
function_rotulo(index_bip_bip_moya)
iri$Enseña <- gsub("BIP-BIP MOYA", "BIP-BIP", iri$Enseña)
#======================================================================================================
#Insular General Alimentaria -> INCLUIDO EN EL GRUPO DE SPAR
# index_iga_pleis <- which(pleis$cadena == "INSULAR GENERAL ALIMENTAR")
# function_cadena_pleis(index_iga_pleis)
# pleis$ensena <- gsub("INSULAR GENERAL ALIMENTAR", "IGA", pleis$ensena)
# iri$Enseña <- gsub("INSULAR GENERAL ALIMENTARIA, S.A.", "IGA", iri$Enseña)
# # index_iga_iri <- which(iri$Rótulo == "SPAR - INSULAR")
# # function_rotulo(index_iga_iri)
# # iri$Enseña <- 
# 
# 
# 
# 
#  d <- gsub("INSULAR GENERAL ALIMENTARIA, S.A.", "IGA", iri$Enseña)
#  which(d == "IGA")
#  which(iri$Enseña == "INSULAR GENERAL ALIMENTARIA, S.A.")   toupper("Insular General Alimentaria, S.A.")
# # 
# # grep("INSULAR GENERAL", iri)
# # 
# # which(iri$Rótulo == "SPAR - INSULAR")

#======================================================================================================
#J. Marquez
index_j_marquez <- which(iri$Rótulo == "J. MARQUEZ")
function_rotulo(index_j_marquez)
iri$Enseña <- gsub("J. MARQUEZ", "J-MARQUEZ", iri$Enseña)
pleis$ensena <- gsub("J.MARQUEZ", "J-MARQUEZ", pleis$ensena)
#======================================================================================================
#Xaloc
index_xaloc <- which(iri$Rótulo == "XALOC")
pleis$ensena <- gsub("SUPER XALOC", "XALOC", pleis$ensena)
#======================================================================================================
#Cyp sela
index_cyp_sela <- which(iri$Rótulo == "CYP SELA")
#======================================================================================================
#La siesta
index_la_siesta <- which(iri$Rótulo == "LA SIESTA")
#======================================================================================================
#Els Masos
index_els_masos <- which(iri$Rótulo == "ELS MASOS")
function_rotulo(index_els_masos)
iri$Enseña <- gsub("ELS MASOS", "MASOS", iri$Enseña)
pleis$ensena <- gsub("ELS MASOS", "MASOS", pleis$ensena)
#======================================================================================================
#Playa Brava
index_playa_brava <- which(iri$Rótulo == "PLAYA BRAVA")
#======================================================================================================
#El Golfet
index_el_golfet <- which(iri$Rótulo == "EL GOLFET")
function_rotulo(index_el_golfet)
iri$Enseña <- gsub("EL GOLFET", "GOLFET", iri$Enseña)
pleis$ensena <- gsub("EL GOLFET", "GOLFET", pleis$ensena)
#======================================================================================================
#Calisol
index_calisol <- which(iri$Rótulo == "CALISOL")
#======================================================================================================
#MAS Y MAS
index_mas_y_mas <- which(iri$Rótulo == "MAS Y MAS - SORIANO" | iri$Rótulo == "MAS Y MAS - LUIS RODRIGUEZ" | 
                           iri$Rótulo == "MAS Y MAS - FORNES" | iri$Rótulo == "MAS Y MAS - PIÑA")
function_rotulo(index_mas_y_mas)
iri$Enseña <- gsub("MAS Y MAS - SORIANO", "MAS-Y-MAS", iri$Enseña)
iri$Enseña <- gsub("MAS Y MAS - LUIS RODRIGUEZ", "MAS-Y-MAS", iri$Enseña)
iri$Enseña <- gsub("MAS Y MAS - FORNES", "MAS-Y-MAS", iri$Enseña)
iri$Enseña <- gsub("MAS Y MAS - PIÑA", "MAS-Y-MAS", iri$Enseña)
pleis$ensena <- gsub("MAS Y MAS", "MAS-Y-MAS", pleis$ensena)
#======================================================================================================
#D y D
index_d_y_d <- which(iri$Rótulo == "D Y D")
function_rotulo(index_d_y_d)
iri$Enseña <- gsub("D Y D", "D-Y-D", iri$Enseña)
pleis$ensena <- gsub("D Y D", "D-Y-D", pleis$ensena)
#======================================================================================================
#Juma
index_juma <- which(iri$Rótulo == "SUPERMERCADO JUMA")
function_rotulo(index_juma)
iri$Enseña <- gsub("SUPERMERCADO JUMA", "JUMA", iri$Enseña)
#======================================================================================================
#Leclerc
index_leclerc <- which(pleis$ensena == "LECLERC EXPRESS")
function_leclerc(index_leclerc)
#======================================================================================================
#Lider Aliment
index_lider <- which(pleis$cadena == "LIDER ALIMENT, S.A.")
function_cadena_pleis(index_lider)
pleis$ensena <- gsub("LIDER ALIMENT, S.A.", "LIDER-ALIMENT", pleis$ensena)
iri$Enseña <- gsub("LIDER ALIMENT", "LIDER-ALIMENT", iri$Enseña)

#nv of 07_09_18
# index_lider_iri <- which(iri$Enseña == "LIDER ALIMENT" & (iri$Rótulo == 'AL LADO' | iri$Rótulo == 'MIKRO' | iri$Rótulo == 'SPAR-LIDER AL' | 
#                                                             iri$Rótulo == 'SPAR EXPRESS-LIDER AL' | iri$Rótulo == 'TANDY-LIDER AL' ))
# function_lider_iri(index_lider_iri)
# index_lider_pleis <- which(pleis$cadena == "LIDER ALIMENT, S.A." & (pleis$ensena == "AL LADO" | pleis$ensena == "EUROCASH" | pleis$ensena == "EUROSPAR" |
#                                                                       pleis$ensena == "MIKRO" | pleis$ensena == "SPAR" | pleis$ensena == "SPAR EXPRESS" |
#                                                                       pleis$ensena == "TANDY"))
# function_lider_pleis(index_lider_pleis)


#======================================================================================================
#Mikro
#En Iri, el rótulo Mikro solo pertenece a la cadena Lider Aliment, S.A.; en Pleis la enseña Mikro pertenece
#a las cadenas Caro Ruiz, S.A., Independiente, Lider o Luis Piña.
#Para hacer la comparación con Iri, considero que todos los Mikro de Pleis son de Lider.
#I comment the next lines on 07_09 and replace with the new system of lines 584.
index_mikro <- which(pleis$ensena == "MIKRO" | pleis$ensena == "MIKROSUR")
function_lider_pleis(index_mikro)

#Otra posibilidad: considerar los supermercados no desde la enseña de Iri, sino desde el rótulo de Iri y sus coincidencias
#con la enseña de Pleis. La posibilidad anterior ofrece más coincidencias.
# pleis$ensena <- gsub("MIKROSUR", "MIKRO", pleis$ensena)
# index_mikro <- which(iri$Rótulo == "MIKRO")
#======================================================================================================
#======================================================================================================
#Minymas
index_minymas <- which(iri$Rótulo == "MINYMAS - PIÑA" | iri$Rótulo == "MINYMAS - LUIS RODRIGUEZ")
function_rotulo(index_minymas)
iri$Enseña <- gsub("MINYMAS - PIÑA", "MINYMAS", iri$Enseña)
iri$Enseña <- gsub("MINYMAS - LUIS RODRIGUEZ", "MINYMAS", iri$Enseña)
#======================================================================================================
#Tandy que no pertenecen a Lider Aliment ni a Froiz
index_tandy_otros <- which(iri$Rótulo == "TANDY - BALDEVA" | iri$Rótulo == "TANDY OTROS") # | iri$Rótulo == "TANDY-FROIZ")
function_rotulo(index_tandy_otros)
iri$Enseña <- gsub("TANDY - BALDEVA", "TANDY", iri$Enseña)
iri$Enseña <- gsub("TANDY OTROS", "TANDY", iri$Enseña)
# iri$Enseña <- gsub("TANDY-FROIZ", "TANDY", iri$Enseña)
pleis$cadena <- gsub("TANDI", "TANDY", pleis$cadena)
#======================================================================================================
#RELINQUE
pleis$ensena <- gsub("AUTOS RELINQUE", "RELINQUE", pleis$ensena)
index_relinque <- which(iri$Rótulo == "AUTOSERVICIO RELINQUE")
function_rotulo(index_relinque)
iri$Enseña <- gsub("AUTOSERVICIO RELINQUE", "RELINQUE", iri$Enseña)
#======================================================================================================
#MASKOMO
pleis$ensena <- gsub("MASCERKA", "MASKOM", pleis$ensena)
pleis$ensena <- gsub("MASKOMPRA", "MASKOM", pleis$ensena)
index_maskomo <- which(iri$Rótulo == "MASKOM" | iri$Rótulo == "SUPERMERCADO CAYETANO")
function_rotulo(index_maskomo)
iri$Enseña <- gsub("SUPERMERCADO CAYETANO", "MASKOM", iri$Enseña)
#======================================================================================================
#Albeyco
pleis$ensena <- gsub("PROMO-CASH", "ALBEYCO", pleis$ensena)
index_albeyco <- which(iri$Rótulo == "MI SUPER ALBEYCO")
function_rotulo(index_albeyco)
iri$Enseña <- gsub("MI SUPER ALBEYCO", "ALBEYCO", iri$Enseña)
#======================================================================================================
#Proxim y Suma, (de Miquel)
index_proxim <- which(iri$Rótulo == "PROXIM")
index_suma <- which(iri$Rótulo == "SUMA")
index_suma_express <- which(iri$Rótulo == "SUMA EXPRESS")
function_rotulo(index_suma_express)
iri$Enseña <- gsub("SUMA EXPRESS", "SUMA", iri$Enseña)
#======================================================================================================
#Michelangelo
index_michelangelo <- which(iri$Rótulo == "MICHELANGELO")
#======================================================================================================
#Eurocop
index_eurocop <- which(iri$Rótulo == "EUROCOP")
#======================================================================================================
#Preti
index_preti <- which(iri$Rótulo == "PRETI")
#======================================================================================================
#Musgrave
##Dialprix
index_dialprix <- which(iri$Rótulo == "DIALPRIX")
#Dicost
index_dicost <- which(iri$Rótulo == "DICOST")
#Super Valu
index_super_valu <- which(iri$Rótulo == "SUPER VALU")
function_rotulo(index_super_valu)
iri$Enseña <- gsub("SUPER VALU", "SUPER-VALU", iri$Enseña)
pleis$ensena <- gsub("SUPER VALU", "SUPER-VALU", pleis$ensena)
#======================================================================================================
#Super Dumbo
index_super_dumbo <- which(iri$Rótulo == "SUPER DUMBO")
function_rotulo(index_super_dumbo)
iri$Enseña <- gsub("SUPER DUMBO", "SUPER-DUMBO", iri$Enseña)
pleis$ensena <- gsub("SUPER DUMBO", "SUPER-DUMBO", pleis$ensena)
#======================================================================================================
#La Kompra
index_la_kompra <- which(iri$Rótulo == "LA KOMPRA")
function_rotulo(index_la_kompra)
iri$Enseña <- gsub("LA KOMPRA", "LA-KOMPRA", iri$Enseña)
pleis$ensena <- gsub("LA KOMPRA", "LA-KOMPRA", pleis$ensena)
#======================================================================================================
#Salinas
index_salinas <- which(iri$Rótulo == "SALINAS")
#======================================================================================================
#Pepe La sal
index_pepe_la_sal <- which(iri$Rótulo == "PEPE LA SAL" | iri$Rótulo == "PEPE LA SAL (DISCOUNT)")  
function_rotulo(index_pepe_la_sal)
iri$Enseña <- gsub("PEPE LA SAL [(]DISCOUNT[)]", "PEPE-LA-SAL", iri$Enseña)
iri$Enseña <- gsub("PEPE LA SAL", "PEPE-LA-SAL", iri$Enseña)
pleis$ensena <- gsub("PEPE LA SAL", "PEPE-LA-SAL", pleis$ensena)
#======================================================================================================
#Super Patri
index_super_patri <- which(iri$Rótulo == "SUPER PATRI")
function_rotulo(index_super_patri)
iri$Enseña <- gsub("SUPER PATRI", "SUPER-PATRI", iri$Enseña)
pleis$ensena <- gsub("SUPER PATRI", "SUPER-PATRI", pleis$ensena)
pleis$ensena <- gsub("CASH PATRI", "SUPER-PATRI", pleis$ensena)
#======================================================================================================
#Super Montgo
index_super_montgo <- which(iri$Rótulo == "SUPER MONTGO")
function_rotulo(index_super_montgo)
iri$Enseña <- gsub("SUPER MONTGO", "SUPER-MONTGO", iri$Enseña)
pleis$ensena <- gsub("SUPER MONTGO", "SUPER-MONTGO", pleis$ensena)
#======================================================================================================
#Super El Pinell
index_el_pinell <- which(iri$Rótulo == "SUPER EL PINELL")
function_rotulo(index_el_pinell)
iri$Enseña <- gsub("SUPER EL PINELL", "SUPER-EL-PINELL", iri$Enseña)
pleis$ensena <- gsub("SUPERMERCAT EL PINELL", "SUPER-EL-PINELL", pleis$ensena)
#======================================================================================================
#Ruiz Galan
index_ruiz_galan <- which(iri$Rótulo == "SUPERMERCADOS RUIZ GALAN")
function_rotulo(index_ruiz_galan)
iri$Enseña <- gsub("SUPERMERCADOS RUIZ GALAN", "SUPER-RUIZ-GALAN", iri$Enseña)
pleis$ensena <- gsub("S.RUIZ GALAN", "SUPER-RUIZ-GALAN", pleis$ensena)
#======================================================================================================
#Saavedra hermanos
index_saavedra_hermanos <- which(iri$Rótulo == "SAAVEDRA HERMANOS")
function_rotulo(index_saavedra_hermanos)

iri$Enseña <- gsub("SAAVEDRA HERMANOS", "SAAVEDRA-HERMANOS", iri$Enseña)
pleis$ensena <- gsub("SAAVEDRA HNOS.", "SAAVEDRA-HERMANOS", pleis$ensena)
pleis$ensena <- gsub("SUP. SAAVEDRA", "SAAVEDRA-HERMANOS", pleis$ensena)
#======================================================================================================
#Alvimar
index_alvimar <- which(iri$Rótulo == "ALVIMAR")
#======================================================================================================
#Lupa
index_lupa <- which(iri$Rótulo == "LUPA")
#======================================================================================================
#Telco
index_telco <- which(iri$Rótulo == "TELCO")
#======================================================================================================
#Tifer
index_tifer <- which(iri$Rótulo == "TIFER")
#======================================================================================================
#Hermanos Martin
# iri$Enseña <- gsub("GRUPO HERMANOS MARTIN", "HERMANOS-MARTIN", iri$Enseña)
# pleis$ensena <- gsub("HNOS[.] MARTIN", "HERMANOS-MARTIN", pleis$ensena)

#Grupo Hermanos Martin
index_hermanos_martin_pleis <- which(pleis$cadena == "GRUPO HERMANOS MARTIN, S.")
function_cadena_pleis(index_hermanos_martin_pleis)
pleis$ensena <- gsub("GRUPO HERMANOS MARTIN, S.", "HERMANOS-MARTIN", pleis$ensena)
iri$Enseña <- gsub("GRUPO HERMANOS MARTIN", "HERMANOS-MARTIN", iri$Enseña)
#======================================================================================================
#Los Duendes
# index_los_duendes <- which(iri$Rótulo == "LOS DUENDES")
# function_rotulo(index_los_duendes)
# iri$Enseña <- gsub("LOS DUENDES", "LOS-DUENDES", iri$Enseña)
# pleis$ensena <- gsub("LOS DUENDES", "LOS-DUENDES", pleis$ensena)
# #======================================================================================================
# #Super Carmela
# index_super_carmela <- which(iri$Rótulo == "SUPER CARMELA")
# function_rotulo(index_super_carmela)
# iri$Enseña <- gsub("SUPER CARMELA", "SUPER-CARMELA", iri$Enseña)
# pleis$ensena <- gsub("SUPER CARMELA", "SUPER-CARMELA", pleis$ensena)
# #======================================================================================================
# #El Trebol
# index_el_trebol <- which(iri$Rótulo == "SUPER TREBOL")
# function_rotulo(index_el_trebol)
# iri$Enseña <- gsub("SUPER TREBOL", "SUPER-TREBOL", iri$Enseña)
# pleis$ensena <- gsub("SUPER TREBOL", "SUPER-TREBOL", pleis$ensena)
#======================================================================================================
#Super Carmela, (we use the generic name instead the small names because there are centers which own to the enseña but don't match between Iri and Pleis)
iri$Enseña <- gsub("SUPER CARMELA, S.L.", "SUPER-CARMELA", iri$Enseña)  #Super Carmela, S.L.
index_super_carmela_pleis <- which(pleis$cadena == "SUPER CARMELA, S.L.")
function_cadena_pleis(index_super_carmela_pleis)
pleis$ensena <- gsub("SUPER CARMELA, S.L.", "SUPER-CARMELA", pleis$ensena)
#======================================================================================================
#Super G
pleis$ensena <- gsub("SUPER G", "SUPER-G", pleis$ensena)
index_super_g <- which(iri$Rótulo == "SUPER \"G\"")
function_rotulo(index_super_g)
iri$Enseña <- gsub("SUPER \"G\"", "SUPER-G", iri$Enseña)
#======================================================================================================
#Casa Evaristo
pleis$ensena <- gsub("CASA EVARISTO", "CASA-EVARISTO", pleis$ensena)
index_casa_evaristo <- which(iri$Rótulo == "CASA EVARISTO")
function_rotulo(index_casa_evaristo)
iri$Enseña <- gsub("CASA EVARISTO", "CASA-EVARISTO", iri$Enseña)
#======================================================================================================
#Super Tallo
pleis$ensena <- gsub("SUPER TALLO", "SUPER-TALLO", pleis$ensena)
index_super_tallo <- which(iri$Rótulo == "SUPER TALLO")
function_rotulo(index_super_tallo)
iri$Enseña <- gsub("SUPER TALLO", "SUPER-TALLO", iri$Enseña)
#======================================================================================================
#Super Arcos
pleis$ensena <- gsub("SUPER ARCOS", "SUPER-ARCOS", pleis$ensena)
index_super_arcos <- which(iri$Rótulo == "SUPER ARCOS")
function_rotulo(index_super_arcos)
iri$Enseña <- gsub("SUPER ARCOS", "SUPER-ARCOS", iri$Enseña)
#======================================================================================================
#Super Enrique
pleis$ensena <- gsub("SAN ENRIQUE", "SAN-ENRIQUE", pleis$ensena)
index_super_enrique <- which(iri$Rótulo == "SAN ENRIQUE")
function_rotulo(index_super_enrique)
iri$Enseña <- gsub("SAN ENRIQUE", "SAN-ENRIQUE", iri$Enseña)
#======================================================================================================
#Super Montserrat
index_super_montserrat_pleis <- which(pleis$cadena == "SUPER MONTSERRAT, S.L.")
function_cadena_pleis(index_super_montserrat_pleis)
pleis$ensena <- gsub("SUPER MONTSERRAT, S.L.", "SUPER-MONTSERRAT", pleis$ensena)
index_super_montserrat_iri <- which(iri$Rótulo == "HIPER MONTSERRAT" | iri$Rótulo == "SUPER MONTSERR" | iri$Rótulo == "SUPER MONTSERRAT")
function_rotulo(index_super_montserrat_iri)
iri$Enseña <- gsub("HIPER MONTSERRAT", "SUPER-MONTSERRAT", iri$Enseña)
iri$Enseña <- gsub("SUPER MONTSERR", "SUPER-MONTSERRAT", iri$Enseña)
iri$Enseña <- gsub("SUPER MONTSERRAT", "SUPER-MONTSERRAT", iri$Enseña)
#======================================================================================================
#Superalca
index_superalca_iri <- which(iri$Rótulo == "SUPERALCA")
index_superalca_pleis <- which(pleis$cadena == "SUPERALCA, S.L.")
function_cadena_pleis(index_superalca_pleis)
pleis$ensena <- gsub("SUPERALCA, S.L.", "SUPERALCA", pleis$ensena)
#======================================================================================================
#Sorli Discau
pleis$ensena <- gsub("SORLI DISCAU", "SORLI-DISCAU", pleis$ensena)
index_sorli_discau <- which(iri$Rótulo == "SORLI-DISCAU")
#======================================================================================================
#Baly
index_baly <- which(iri$Rótulo == "SUPERMERCADO BALY")
function_rotulo(index_baly)
iri$Enseña <- gsub("SUPERMERCADO BALY", "BALY", iri$Enseña)
#======================================================================================================
#Costa Blanca
index_costa_blanca <- which(iri$Rótulo == "SUPERMERCADOS COSTA BLANCA")
function_rotulo(index_costa_blanca)
iri$Enseña <- gsub("SUPERMERCADOS COSTA BLANCA", "COSTA BLANCA", iri$Enseña)
#======================================================================================================
#Egea
pleis$ensena <- gsub("SUPER VIA-EGEA", "EGEA", pleis$ensena)
pleis$ensena <- gsub("SUPER EGEA", "EGEA", pleis$ensena)
index_egea <- which(iri$Rótulo == "ALIMENTACION EGEA")
function_rotulo(index_egea)
iri$Enseña <- gsub("ALIMENTACION EGEA", "EGEA", iri$Enseña)
#======================================================================================================
#Supermercados Cadiz
index_super_cadiz_pleis <- which(pleis$cadena == "SUPERMERCADOS CADIZ, S.L.")
function_cadena_pleis(index_super_cadiz_pleis)
pleis$ensena <- gsub("SUPERMERCADOS CADIZ, S.L.", "SUPERMERCADOS-CADIZ", pleis$ensena)
iri$Enseña <- gsub("SUPERMERCADOS CADIZ, S.L.", "SUPERMERCADOS-CADIZ", iri$Enseña)   
#======================================================================================================
#Vismac
index_vismac <- which(iri$Rótulo == "VISMAC")
#======================================================================================================
#Codi
index_codi <- which(iri$Rótulo == "SUPERMERCADOS CODI" | iri$Rótulo == "CODI SUPERMERCADOS")
function_rotulo(index_codi)
iri$Enseña <- gsub("SUPERMERCADOS CODI", "CODI", iri$Enseña)
iri$Enseña <- gsub("CODI SUPERMERCADOS", "CODI", iri$Enseña)
#======================================================================================================
#Dani
index_dani_pleis <- which(pleis$cadena == "SUPERMERCADOS DANI, S.A.")
function_cadena_pleis(index_dani_pleis)
pleis$ensena <- gsub("SUPERMERCADOS DANI, S.A.", "DANI", pleis$ensena)
index_dani_iri <- which(iri$Rótulo == "DANI")
#======================================================================================================
#Super Hiber
pleis$ensena <- gsub("SUPER HIBER", "SUPER-HIBER", pleis$ensena)
iri$Enseña <- gsub("SUPERMERCADOS HIBER", "SUPER-HIBER", iri$Enseña)
#======================================================================================================
#La Salve
index_la_salve_pleis <- which(pleis$cadena == "SUPERMERCADOS LA SALVE")
function_cadena_pleis(index_la_salve_pleis)
pleis$ensena <- gsub("SUPERMERCADOS LA SALVE", "SUPERMERCADOS-LA-SALVE", pleis$ensena)
index_la_salve_iri <- which(iri$Rótulo == "SUPERMERCADOS LA SALVE")
function_rotulo(index_la_salve_iri)
iri$Enseña <- gsub("SUPERMERCADOS LA SALVE", "SUPERMERCADOS-LA-SALVE", iri$Enseña)
#======================================================================================================
#Los Alpes
index_los_alpes <- which(iri$Rótulo == "SUPERMERCADOS LOS ALPES")
function_rotulo(index_los_alpes)
iri$Enseña <- gsub("SUPERMERCADOS LOS ALPES", "LOS ALPES", iri$Enseña)
#======================================================================================================
#Ros
pleis$ensena <- gsub("SUPER ROS", "SUPER-ROS", pleis$ensena)
# index_prueba <- which(iri$Rótulo == "SUPERMERCADO ROS (ANGELINO ROS) (A)")
index_ros <- which(iri$Rótulo == "SUPERMERCADOS ROS, S.L." | iri$Rótulo == "SUPERMERCADO ROS (ANGELINO ROS) (A)")  #Supermercado Ros (Angelino Ros) (A)
function_rotulo(index_ros)
iri$Enseña <- gsub("SUPERMERCADOS ROS, S.L.", "SUPER-ROS", iri$Enseña)
iri$Enseña <- gsub("SUPERMERCADO ROS [(]ANGELINO ROS[)] [(]A[)]", "SUPER-ROS", iri$Enseña)
#======================================================================================================
#Sanchez Romero
index_sanchez_romero <- which(iri$Rótulo == "SANCHEZ ROMERO")
function_rotulo(index_sanchez_romero)
iri$Enseña <- gsub("SANCHEZ ROMERO", "SANCHEZ-ROMERO", iri$Enseña)
pleis$ensena <- gsub("SANCHEZ ROMERO", "SANCHEZ-ROMERO", pleis$ensena)
#======================================================================================================
#Super Tantiña
pleis$ensena <- gsub("SUPER TANTIÑA", "SUPER-TANTIÑA", pleis$ensena)
index_super_tantina <- which(iri$Rótulo == "SUPERMERCADOS TANTIÑA")
function_rotulo(index_super_tantina)
iri$Enseña <- gsub("SUPERMERCADOS TANTIÑA", "SUPER-TANTIÑA", iri$Enseña)
#======================================================================================================
#Super Touriño
pleis$ensena <- gsub("SUPER TOURIÑO", "SUPER-TOURIÑO", pleis$ensena)
index_super_tourino <- which(iri$Rótulo == "SUPERMERCADO TOURIÑO")
function_rotulo(index_super_tourino)
iri$Enseña <- gsub("SUPERMERCADO TOURIÑO", "SUPER-TOURIÑO", iri$Enseña)
#======================================================================================================
#Supermertcats Llobet
index_llobet_pleis <- which(pleis$cadena == "SUPERMERCATS LLOBET, S.A.")
function_cadena_pleis(index_llobet_pleis)
pleis$ensena <- gsub("SUPERMERCATS LLOBET, S.A.", "SUPER-LLOBET", pleis$ensena)
iri$Enseña <- gsub("SUPERMERCATS LLOBET, S.A.", "SUPER-LLOBET", iri$Enseña)
#======================================================================================================
#Supermas
index_supermas <- which(iri$Rótulo == "SUPERMERCADOS SUPERMAS")
function_rotulo(index_supermas)
iri$Enseña <- gsub("SUPERMERCADOS SUPERMAS", "SUPERMAS", iri$Enseña)
#======================================================================================================
#Superservis
index_superservis <- which(iri$Rótulo == "SUPERSERVIS")
#======================================================================================================
#Supersol
index_supersol <- which(iri$Rótulo == "SUPERSOL")
#======================================================================================================
#Supertauro
pleis$ensena <- gsub("SUPER TAURO", "SUPER-TAURO", pleis$ensena)
index_super_tauro <- which(iri$Rótulo == "SUPER TAURO")
function_rotulo(index_super_tauro)
iri$Enseña <- gsub("SUPER TAURO", "SUPER-TAURO", iri$Enseña)
#======================================================================================================
#Supermercats Pujol
index_pujol_pleis <- which(pleis$cadena == "SUPSA SUPERMERCATS PUJOL")
function_cadena_pleis(index_pujol_pleis)
pleis$ensena <- gsub("SUPSA SUPERMERCATS PUJOL", "PUJOL", pleis$ensena)
iri$Enseña <- gsub("SUPERMERCADOS PUJOL", "PUJOL", iri$Enseña)
#======================================================================================================
#Sutega
index_sutega <- which(pleis$cadena == "SUTEGA, S.L.")
function_cadena_pleis(index_sutega)
#======================================================================================================
#Alvica
pleis$ensena <- gsub("SUPER ALVICA", "ALVICA", pleis$ensena)
index_alvica <- which(iri$Rótulo == "SUPERMERCADOS ALVICA")
function_rotulo(index_alvica)
iri$Enseña <- gsub("SUPERMERCADOS ALVICA", "ALVICA", iri$Enseña)
#======================================================================================================
#Udaco
# index_udaco <- which(iri$Rótulo == "UDACO")
#======================================================================================================


#Functions to replace the ensena names that aren't in Iri.
#Alternative function to increase the performance.
# index_gama_unide <- which(pleis$ensena == "GAMA" & pleis$cadena == "UNIDE, S.COOP.") #To replace the function change_some_fields()   
index_gama_unide <- which(pleis$ensena == "GAMA") 
index_giro_unide <- which(pleis$ensena == "AUTOS.GIRO")
index_cash_unide <- which(pleis$ensena == "CASH UNIDE")
index_maxcoop_unide <- which(pleis$ensena == "MAXCOOP")
index_ana_unide <- which(pleis$ensena == "SUPER ANA")
index_udaco <- which(pleis$ensena == "UDACO")

indexes_unide <- list(index_gama_unide, index_giro_unide, index_cash_unide, index_maxcoop_unide, 
                      index_ana_unide, index_udaco
)
lapply(indexes_unide, function_unide)

# function_unide(index_gama_unide)
#=====================================================================================================
#Super BM
pleis$ensena <- gsub("SUPER BM-GELSA","UVESCO",pleis$ensena)
pleis$ensena <- gsub("SUPER BM","UVESCO",pleis$ensena) #Lo que estaba


#Con esto obtenemos peores resultados.
# pleis$ensena <- gsub("SUPER BM", "SUPER-BM", pleis$ensena)
# index_super_bm <- which(iri$Rótulo == "SUPER BM" )
# function_rotulo(index_super_bm)
# iri$Enseña <- gsub("SUPER BM", "SUPER-BM", iri$Enseña)

#Hacer como en UNIDE.
#=====================================================================================================
#Ercoreca
index_ercoreca <- which(iri$Rótulo == "ERCORECA")
#=====================================================================================================
#Netto
pleis$ensena <- gsub("NETTO S-20", "NETTO", pleis$ensena)
index_netto <- which(iri$Rótulo == "NETTO")
#=====================================================================================================
#Super Amara
pleis$ensena <- gsub("SUPER AMARA", "SUPER-AMARA", pleis$ensena)
index_super_amara <- which(iri$Rótulo == "SUPER AMARA")
function_rotulo(index_super_amara)
iri$Enseña <- gsub("SUPER AMARA", "SUPER-AMARA", iri$Enseña)
#======================================================================================================
#Ugari
index_ugari <- which(iri$Rótulo == "UGARI")
#======================================================================================================
#Berriak
index_berriak <- which(iri$Rótulo == "BERRIAK SUPERMERKATUA")
function_rotulo(index_berriak)
iri$Enseña <- gsub("BERRIAK SUPERMERKATUA", "BERRIAK", iri$Enseña)
#======================================================================================================
#Valvi
index_valvi <- which(iri$Rótulo == "VALVI SUPERMERCATS")
function_rotulo(index_valvi)
iri$Enseña <- gsub("VALVI SUPERMERCATS", "VALVI", iri$Enseña)
#======================================================================================================
#BON PREU
index_bon_preu_pleis <- which(pleis$cadena == "BON PREU, S.A.")
function_cadena_pleis(index_bon_preu_pleis)
pleis$ensena <- gsub("BON PREU, S.A.", "BON-PREU", pleis$ensena)
iri$Enseña <- gsub("GRUPO BON PREU", "BON-PREU", iri$Enseña)
#======================================================================================================
#Hiper Manacor
index_hiper_manacor_pleis <- which(pleis$cadena == "HIPER MANACOR, S.A.")
function_cadena_pleis(index_hiper_manacor_pleis)
pleis$ensena <- gsub("HIPER MANACOR, S.A.", "HIPER-MANACOR", pleis$ensena)
iri$Enseña <- gsub("HIPER MANACOR, S.A.", "HIPER-MANACOR", iri$Enseña)
#======================================================================================================
#Family Cash
index_family_cash <- which(iri$Rótulo == "FAMILY CASH")
function_rotulo(index_family_cash)
iri$Enseña <- gsub("FAMILY CASH", "FAMILY_CASH", iri$Enseña)
pleis$ensena <- gsub("FAMILYCASH", "FAMILY_CASH", pleis$ensena)
#======================================================================================================
#Ignacio de las Cuevas
index_ignacio_cuevas_pleis <- which(pleis$cadena == "IGNACIO DE LAS CUEVAS, S.")
function_cadena_pleis(index_ignacio_cuevas_pleis)
pleis$ensena <- gsub("IGNACIO DE LAS CUEVAS, S.", "IGANCIO-CUEVAS", pleis$ensena)
iri$Enseña <- gsub("IGNACIO DE LAS CUEVAS, S.A.", "IGANCIO-CUEVAS", iri$Enseña)
#======================================================================================================
#Sangui
index_sangui <- which(iri$Rótulo == "SANGUI")
#======================================================================================================
#Jespac
index_jespac <- which(iri$Rótulo == "JESPAC")
#======================================================================================================
#Aldi
# index_aldi <- which(iri$Rótulo == "ALDI") It is not neccessary because Iri has Enseña=Aldi
#======================================================================================================
#CONDIS
pleis$ensena <- gsub("CONDIS EXPRESS","CONDIS",pleis$ensena)
pleis$ensena <- gsub("DISTOP","CONDIS",pleis$ensena)
#======================================================================================================
#ESTABLECIMIENTOS PLAZA
pleis$ensena <- gsub("SUP. PLAZA", "ESTABLECIMIENTOS PLAZA", pleis$ensena)
#======================================================================================================
#SUPER MOR
index_mor <- which(iri$Rótulo == "SUPERMERCADOS ECO-MOR")
function_rotulo(index_mor)
iri$Enseña <- gsub("SUPERMERCADOS ECO-MOR","SUPER-MOR", iri$Enseña)
#======================================================================================================




#======================================================================================================
#List of indexes for function_rotulo:
indexes_rotulo_iri <- list(index_franquicia_eroski, index_claudio, index_arbol, index_ahorraves, index_salinas, index_pincha_precios,
                           index_komo_komo, index_la_compra, index_darvi, index_iberplus, index_la_lonja, #index_maxcoop,
                           index_don_market,
                           index_cash_lepe, index_gigante, index_novavenda, index_unico, #index_petit_preu #, index_coaliment, index_coaliment_compra_saludable
                           index_alsara, index_disara, index_urbasa, index_disbo, index_xaloc, index_cyp_sela, index_la_siesta, index_playa_brava,
                           index_calisol, index_proxim, index_suma, index_michelangelo, index_eurocop, index_preti, index_dialprix, index_dicost,
                           index_salinas, index_alvimar, index_lupa, index_telco, index_tifer, index_superalca_iri, index_sorli_discau,
                           index_vismac, index_dani_iri, index_superservis, index_supersol, #index_udaco#, index_gama
                           index_ercoreca, index_netto, index_ugari, index_sangui, index_jespac#, index_aldi
)
lapply(indexes_rotulo_iri,function_rotulo) #It execute function_rotulo() over each element of list "indexes".

#=======================================================================================================



#We are going to remove the words "CENTER" and "SUPERMERCADOS" from the Pleis/ensena and Iri/Enseña, 
#(this happens at least with Eroski and Carrefour):
iri$Enseña <- gsub("[[:space:]]+[CENTER|SUPERMERCADOS]+$","",iri$Enseña)
#iri$Enseña <- gsub("^[A-Z]+[[:space:]]+DIA","DIA",iri$Enseña) #We remove MAXI from MAXI DIA in Iri/Enseña.


#We replace several ensenas to match Pleis with Iri, (they are the same concept in both of them).
pleis$ensena <- gsub("SIMPLY MARKET","SABECO",pleis$ensena) #=================================







#==========================================================================Changes in Iri's Addresses======================================================================


#We replace the tildes and dieresis in Iri/Direccion1:
iri$Direccion1 <- gsub("Á", "A", iri$Direccion1)
iri$Direccion1 <- gsub("É", "E", iri$Direccion1)
iri$Direccion1 <- gsub("Í", "I", iri$Direccion1)
iri$Direccion1 <- gsub("Ó", "O", iri$Direccion1)
iri$Direccion1 <- gsub("Ú", "U", iri$Direccion1)
iri$Direccion1 <- gsub("Ü", "U", iri$Direccion1)

#=======================================================================================================================================

# iri$Direccion1 <- gsub("^AV+[[:space:]]", "", iri$Direccion1)     
# iri$Direccion1 <- gsub("^AV[.]+[[:space:]]", "", iri$Direccion1)
# iri$Direccion1 <- gsub("^AV[.]", "", iri$Direccion1)
# iri$Direccion1 <- gsub("^\\<AVD\\>+[[:space:]]", "", iri$Direccion1)
# iri$Direccion1 <- gsub("^\\<AVD\\>[.]+[[:space:]]", "", iri$Direccion1)
# iri$Direccion1 <- gsub("^\\<AVD\\>[.]", "", iri$Direccion1)
# iri$Direccion1 <- gsub("^\\<AVDA\\>[.]+[[:space:]]", "", iri$Direccion1)
# iri$Direccion1 <- gsub("^\\<AVDA\\>[.]", "", iri$Direccion1)
# iri$Direccion1 <- gsub("^\\<AVDA\\>,+[[:space:]]", "", iri$Direccion1)
# iri$Direccion1 <- gsub("^\\<AVDA\\>+[[:space:]]", "", iri$Direccion1)
# iri$Direccion1 <- gsub("^\\<AVDA\\>", "", iri$Direccion1)
# iri$Direccion1 <- gsub("^\\<AVENIDA\\>+[[:space:]]", "", iri$Direccion1)
# iri$Direccion1 <- gsub("^\\<AVINGUDA\\>+[[:space:]]", "", iri$Direccion1)

iri$Direccion1 <- gsub("^AV+[[:space:]]", "", iri$Direccion1)     
iri$Direccion1 <- gsub("^AV[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^AV[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^AVD+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^AVD[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^AVD[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^AVDA[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("AVDA[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^AVDA,+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^AVDA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^AVDA", "", iri$Direccion1)
iri$Direccion1 <- gsub("^AVENIDA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^AVINGUDA+[[:space:]]", "", iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^ANT+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^ANT[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^ANT[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^ANTIGUA+[[:space:]]", "", iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^ARR+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^ARR[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^ARR[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^ARRAVAL+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^ARRABAL+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RAVAL+[[:space:]]", "", iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^AUT+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^AUT[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^AUT[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^AUTOPISTA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^AUTOVIA+[[:space:]]", "", iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^BARRIO+[[:space:]]", "", iri$Direccion1)   
iri$Direccion1 <- gsub("^BARRIAL+[[:space:]]", "", iri$Direccion1) 
iri$Direccion1 <- gsub("^BARRIS+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BARRO+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BAR[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BAR[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BA[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BA[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^B[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^B[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BDA[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BDA[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BD[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BD[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BD+[[:space:]]", "", iri$Direccion1)

iri$Direccion1 <- gsub("^BARRIADA+[[:space:]]", "", iri$Direccion1)

mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^BOULEVARD+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BOULEVAR+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^BULEVARD+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BULEVAR+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BOULE+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BOULE[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BOULE[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BO[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BO[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("\\<BO\\>+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BU[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^BU[.]", "", iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^C[.]C[.]+[[:space:]]", "", iri$Direccion1) 
iri$Direccion1 <- gsub("^C[.]C[.]", "", iri$Direccion1) 
iri$Direccion1 <- gsub("^CC[.]+[[:space:]]", "", iri$Direccion1) 
iri$Direccion1 <- gsub("^C[.]C+[[:space:]]", "", iri$Direccion1) 
iri$Direccion1 <- gsub("^CC[.]", "", iri$Direccion1) 
iri$Direccion1 <- gsub("^CC+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CENTRO COMERCIAL+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^C[.] COMERCIAL+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^\\<CENTRO CIAL\\>[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CENTRO C[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CENTRO C[.]", "", iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.



iri$Direccion1 <- gsub("^C[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^C[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^C+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CALLEJON+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CALLE+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^C[/]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^C[/]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CL+[[:space:]]","",iri$Direccion1)  
iri$Direccion1 <- gsub("^CL[.]+[[:space:]]","",iri$Direccion1)  
iri$Direccion1 <- gsub("^CL[.]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^CA[.]+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^CA[.]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^CA+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^COL[.]+[[:space:]]","",iri$Direccion1) 
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]"," ",iri$Direccion1)  
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]"," ",iri$Direccion1)
iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<EL\\>+[[:space:]]"," ",iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.

iri$Direccion1 <- gsub("^CAM+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CAM[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CAM[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CAMI+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CAMI[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CAMI[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CAMINO+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CNO+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CNO[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CNO[.]", "", iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^CAR+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CAR[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CAR[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CARR+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CARR[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CARR[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CARRER+[[:space:]]", "", iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^CARRETERA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CRTA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CRTA[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CRTA[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CRA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CRA[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CRA[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CRT+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CRT[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CRT[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CR+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CR[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CR[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CTRA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CTRA[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CTRA[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CTA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CTA[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CTA[.]", "", iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^CORREDERA+[[:space:]]", "", iri$Direccion1)
# iri$Direccion1 <- gsub("[^CORREDERA$]", "", iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1) 


iri$Direccion1 <- gsub("^PL+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PL[.]+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PL[.]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PLA+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PLA[.]+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PLA[.]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PLAÇA+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PLZ+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PLZ[.]+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PLZ[.]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PZ+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PZ[.]+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PZ[.]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PZA+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PZA[.]+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PZA[.]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PLAZA+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PRAZA+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PTDA[.]+[[:space:]]","",iri$Direccion1)  
iri$Direccion1 <- gsub("^PTDA[.]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PARTIDA ","",iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
# iri$Direccion1 <- gsub("^[DE|DEL]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.



iri$Direccion1 <- gsub("^PTO+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PTO[.]+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PTO[.]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PUERTO+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
# iri$Direccion1 <- gsub("^[DE|DEL]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.



iri$Direccion1 <- gsub("^GTA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^GTA[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^GTA[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^GLO+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^GLO[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^GLO[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^\\<GLORIETA\\>", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
# iri$Direccion1 <- gsub("^[DE|DEL]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^GPO+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^GPO[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^GPO[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1)
# iri$Direccion1 <- gsub("^[DE|DEL]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.



iri$Direccion1 <- gsub("^LUG+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^LUG[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^LUG[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^LUGAR+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.

iri$Direccion1 <- gsub("\\<NUEVE\\>", "9",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^PSO+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PSO[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PSO[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PO+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PO[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PO[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PAS+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PAS[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PAS[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PS+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PS[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PS[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PASEO+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.



iri$Direccion1 <- gsub("\\<PARCELA\\>+[[:space:]]", " ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PARCELA\\>", "",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PORCEL\\>+[[:space:]]", " ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PORCEL\\>", "",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PARCEL\\>+[[:space:]]", " ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PARCEL\\>", "",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PARC\\>[.]+[[:space:]]", " ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PARC\\>[.]", " ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PARC\\>", " ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PORC\\>[.]+[[:space:]]", " ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PORC\\>[.]", " ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PORC\\>+[[:space:]]", " ",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^PJE+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PJE[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PJE[.]", "", iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.

iri$Direccion1 <- gsub("^PQE[.]+[[:space:]]+NACIONAL", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PQE+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PQE[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PQE[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PQUE+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PQUE[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PQUE[.]+CIAL", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PQUE[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^\\<PARQUE\\>", "", iri$Direccion1)
mi_gsub(from, to, pleis$direccion)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.



iri$Direccion1 <- gsub("^POBLADO+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^P.E.+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^P.E+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^POL+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^POL[.]+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^POL[.]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PLG+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PLG[.]+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PLG[.]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PNO+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PNO[.]+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PNO[.]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PG+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PG[.]+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^PG[.]","",iri$Direccion1)
iri$Direccion1 <- gsub("^P[.]I[.]+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^P[.]I[.]","",iri$Direccion1)
iri$Direccion1 <- gsub("^\\<POLIGONO INDUSTRIAL\\>","",iri$Direccion1)
iri$Direccion1 <- gsub("^POLIGONO+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1) 
# iri$Direccion1 <- gsub("^[DE|DEL]+[[:space:]]", "", iri$Direccion1)  #COMPROBAR ESTOS CORCHETES
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.



iri$Direccion1 <- gsub("^RDA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RDA[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RDA[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^R+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^R[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^R[.]", "", iri$Direccion1)
# iri$Direccion1 <- gsub("^RONDA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^\\<RONDA\\>", "", iri$Direccion1)
iri$Direccion1 <- gsub("^\\<RESIDENCIAL\\>", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1) 
# iri$Direccion1 <- gsub("^[DE|DEL]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.




iri$Direccion1 <- gsub("^\\<RAMBLE\\>", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RAM+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RAM[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RAM[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1) 
# iri$Direccion1 <- gsub("^[DE|DEL]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^RSD+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RSD[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RSD[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^SENDA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^SDA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^SDA[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^SDA[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^TRV+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^TRV[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^TRV[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^TR+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^TR[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^TR[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^TRAV+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^TRAV[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^TRAV[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^TRVS+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^TRVS[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^TRVS[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^TRAVESIA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^TRAVESERA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^TRAVESSERA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^URB+[[:space:]]", "", iri$Direccion1)  #Mirar si interesa eliminar las urb que aparecen a mitad de dirección.
iri$Direccion1 <- gsub("^URB[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^URB[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^URBANIZACION+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.



iri$Direccion1 <- gsub("^RUA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RUA DA+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RUA DAS+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RUA DO+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RUA DOS+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RUA DE+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
iri$Direccion1 <- gsub("^LA+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^LO+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^EL+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^LAS+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^LOS+[[:space:]]","",iri$Direccion1)

iri$Direccion1 <- gsub("^O+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^OS+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^A+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^AS+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^ELS+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^LES+[[:space:]]","",iri$Direccion1)

iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.

iri$Direccion1 <- gsub("^CAT+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CAT[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CAT[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^CATEDRATICO+[[:space:]]", "", iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1) 


iri$Direccion1 <- gsub("^DOCTOR+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^DOUTOR+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^DOC+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^DOC[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^DOC[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^DC+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^DC[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^DC[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^DR+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^DR[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^DR[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^DTOR+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^DTOR[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^DTOR[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.

iri$Direccion1 <- gsub("^DON+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^D[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^D[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^D[*]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^MOSSEN+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.

iri$Direccion1 <- gsub("^OBISPO+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.

iri$Direccion1 <- gsub("^PIN+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PIN[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PIN[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^PINTOR+[[:space:]]", "", iri$Direccion1)
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.



iri$Direccion1 <- gsub("^RAD+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RAD[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RAD[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^RADIOFONISTA", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
mi_gsub(from, to, iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.



#We remove again the Don and its friends. Let see if this is neccessary.
iri$Direccion1 <- gsub("^DON+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^D[.]+[[:space:]]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^D[.]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^D[*]", "", iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
iri$Direccion1 <- gsub("^LA+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^LO+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^EL+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^LAS+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^LOS+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^O+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^OS+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^A+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^AS+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^ELS+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^LES+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("D[']", "", iri$Direccion1) 
iri$Direccion1 <- gsub("^IND[.]", "", iri$Direccion1)  
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
iri$Direccion1 <- gsub("^PROF[.]", "", iri$Direccion1)  


iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.

#============================================================================================================
iri$Direccion1 <- gsub("[[:space:]]+\\<DELS\\>+[[:space:]]"," DE LOS ",iri$Direccion1) 
iri$Direccion1 <- gsub("[[:space:]]+\\<DEL\\>+[[:space:]]"," ",iri$Direccion1)   
iri$Direccion1 <- gsub("[[:space:]]+\\<DE\\>+[[:space:]]"," ",iri$Direccion1)  


#We remove content in parenthesis at the end of direccion.
iri$Direccion1 <- gsub("[()]", "", iri$Direccion1)
#============================================================================================================

iri$Direccion1 <- gsub("[[:space:]]+\\<PLANTA\\>+[[:space:]]+[A-Z]+$","",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+$", "", iri$Direccion1)  #=============== We remove the spaces on the right.


#Numbers
#We are going to remove the number in the address.
iri$Direccion1 <- gsub("[[:space:]]+BAJO+$","",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+$","",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+[A-Z]+$","",iri$Direccion1) 
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+[[:space:]]+[Y]$","",iri$Direccion1) #Numbers with preposition "Y" in the middle.
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\<PTA\\>$","",iri$Direccion1) #Numbers with "pta" in the middle.

#To number with hyphen
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+[0-9]+\\-+[0-9]+$","",iri$Direccion1) #For addresses with three numbers.
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+[0-9]+$","",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+\\_+[0-9]+$","",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+\\,+[0-9]+\\,+[0-9]+$","",iri$Direccion1) #Numbers with comma between then.
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+\\,+[0-9]+$","",iri$Direccion1) #Numbers with comma between then.
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+\\,+[[:space:]]+[0-9]+\\,+[[:space:]]+[0-9]+$","",iri$Direccion1) #Numbers with comma and spaces between then.
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+$","",iri$Direccion1) 
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+\\_+$","",iri$Direccion1) 
#There are numbers plus hyphen plus space plus number.
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+[0-9]+$","",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+$","",iri$Direccion1)

#iri$Direccion1 <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+$", "",iri$Direccion1)



##===========================================================================================================================   
iri$Direccion1 <- gsub("[0-9]+\\_+[0-9]+$","",iri$Direccion1) 
iri$Direccion1 <- gsub("BAJO+$","",iri$Direccion1)
# iri$Direccion1 <- gsub("[0-9]+$","",iri$Direccion1) #PONGO ESTO MÁS ABAJO.
iri$Direccion1 <- gsub("[0-9]+\\-+[A-Z]+$","",iri$Direccion1) 
iri$Direccion1 <- gsub("[0-9]+[[:space:]]+\\-+[[:space:]]+[0-9]+[[:space:]]+[A-Z]+$","",iri$Direccion1)
iri$Direccion1 <- gsub("[0-9]+[[:space:]]+[A-Z]+$","",iri$Direccion1)
iri$Direccion1 <- gsub("[0-9]+[[:space:]]+[Y]$","",iri$Direccion1) #Numbers with preposition "Y" in the middle.
iri$Direccion1 <- gsub("[0-9]+[[:space:]]+\\<PTA\\>$","",iri$Direccion1) #Numbers with "pta" in the middle.
iri$Direccion1 <- gsub("[0-9]+[A-Z]+\\-+[0-9]+[A-Z]+$","",iri$Direccion1) #add THIS=============================================================%%%%%%%%%%%%%%%%%%%%%%%%%%%
iri$Direccion1 <- gsub("[0-9]+[A-Z]+$","",iri$Direccion1) 

#To number with hyphen
iri$Direccion1 <- gsub("[0-9]+\\-+[0-9]+\\-+[0-9]+$","",iri$Direccion1) #For addresses with three numbers.
iri$Direccion1 <- gsub("[0-9]+\\-+[0-9]+\\-$","",iri$Direccion1)
iri$Direccion1 <- gsub("[0-9]+\\-+[0-9]+$","",iri$Direccion1)
iri$Direccion1 <- gsub("[0-9]+\\,+[0-9]+$","",iri$Direccion1) #Numbers with comma between then.
iri$Direccion1 <- gsub("[0-9]+[/]+[0-9]+$","",iri$Direccion1) #Numbers with slash between then.
iri$Direccion1 <- gsub("[0-9]+\\-+$","",iri$Direccion1) 

#There are numbers plus hyphen plus space plus number.
iri$Direccion1 <- gsub("[0-9]+[[:space:]]+\\-+[0-9]+$","",iri$Direccion1)
iri$Direccion1 <- gsub("[0-9]+[[:space:]]+\\-+$","",iri$Direccion1)

iri$Direccion1 <- gsub("[0-9]+\\-+[[:space:]]+[A-Z]+\\º$","",iri$Direccion1)
iri$Direccion1 <- gsub("[0-9]+\\-+[A-Z]+\\º$","",iri$Direccion1) 
##===========================================================================================================================

iri$Direccion1 <- gsub("[0-9]+$","",iri$Direccion1)


#There are numbers plus hyphen plus space plus number.
#To remove comma between numbers.
iri$Direccion1 <- gsub("[0-9]+\\,+$","",iri$Direccion1) 
iri$Direccion1 <- gsub(" S[/]N$","",iri$Direccion1) 
iri$Direccion1 <- gsub("S[/]N$","",iri$Direccion1) 
iri$Direccion1 <- gsub("[[:space:]]+\\<SN\\>$","",iri$Direccion1)   

iri$Direccion1 <- gsub(" S[-]N$","",iri$Direccion1)
iri$Direccion1 <- gsub("S[-]N$","",iri$Direccion1)
iri$Direccion1 <- gsub(" N[º]$","",iri$Direccion1) 

iri$Direccion1 <- gsub("[[:space:]]+N[.]$","",iri$Direccion1) 
iri$Direccion1 <- gsub("N[.][0-9]$","",iri$Direccion1) #We already removed the comma.
iri$Direccion1 <- gsub("[[:space:]]+N[.][0-9]$","",iri$Direccion1) 
#We remove the comma at the end.
iri$Direccion1 <- gsub(",$","",iri$Direccion1)

# iri$Direccion1 <- gsub("[0-9]+\\-+[[:space:]]+[A-Z]+\\º$","",iri$Direccion1) #===============================================================BAJO============================
# iri$Direccion1 <- gsub("[0-9]+\\-+[A-Z]+\\º$","",iri$Direccion1) 

#=========================================================================================================



iri$Direccion1 <- gsub("[[:space:]]+\\<XENDA\\>+$","",iri$Direccion1)
iri$Direccion1 <- gsub("\\<COMPANY\\>","COMPANYS",iri$Direccion1)
# iri$Direccion1 <- gsub("ALEXANDRE ROSELLO","ALEXANDRE ROSSELLO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ROSELLO\\>","ROSSELLO",iri$Direccion1)

#We remove the prepositions "de/del" in Iri/Direccion1.
iri$Direccion1 <- gsub("[[:space:]]+\\<DELS\\>+[[:space:]]"," DE LOS ",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+\\<DEL\\>+[[:space:]]"," ",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+\\<DE\\>+[[:space:]]"," ",iri$Direccion1)  




#==========================================================================================================
iri$Direccion1 <- gsub("\\<ANTON\\>[.]+[[:space:]]", "ANTONIO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ANTON\\>[.]", "ANTONIO ",iri$Direccion1)

iri$Direccion1 <- gsub("ARQUI[.]+[[:space:]]","ARQUITECTO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ARQUI\\>[.]","ARQUITECTO ",iri$Direccion1) 
iri$Direccion1 <- gsub("AUZOA$","",iri$Direccion1)

iri$Direccion1 <- gsub("^CTE[.]+[[:space:]]", "COMANDANTE ", iri$Direccion1)
iri$Direccion1 <- gsub("^CTE[.]", "COMANDANTE", iri$Direccion1)
iri$Direccion1 <- gsub("^CTE+[[:space:]]", "COMANDANTE ", iri$Direccion1)

iri$Direccion1 <- gsub("\\CONJTO\\>[.]+[[:space:]]", "CONJUNTO ", iri$Direccion1)
iri$Direccion1 <- gsub("\\CONJTO\\>[.]", "CONJUNTO ", iri$Direccion1)

iri$Direccion1 <- gsub("\\<EDIF\\>[.]+[[:space:]]", "EDIFICIO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<EDIF\\>[.]", "EDIFICIO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<EDI\\>[.]+[[:space:]]", "EDIFICIO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<EDI\\>[.]", "EDIFICIO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<EDF\\>[.]+[[:space:]]", "EDIFICIO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<EDF\\>[.]", "EDIFICIO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ED\\>[.]+[[:space:]]", "EDIFICIO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ED\\>[.]", "EDIFICIO ",iri$Direccion1)

iri$Direccion1 <- gsub("\\<ESCRIT\\>[.]+[[:space:]]", "ESCRITOR ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ESCRIT\\>[.]", "ESCRITOR ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ESCRIT\\>[.]+[[:space:]]", "ESCRITOR ",iri$Direccion1)

iri$Direccion1 <- gsub("FCO+[[:space:]]","FRANCISCO ",iri$Direccion1)
iri$Direccion1 <- gsub("FCO[.]+[[:space:]]","FRANCISCO ",iri$Direccion1)
iri$Direccion1 <- gsub("FCO[.]","FRANCISCO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<FDEZ\\>+[[:space:]]", "FERNANDEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<FDEZ\\>[.]+[[:space:]]", "FERNANDEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<FDEZ\\>[.]", "FERNANDEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<FDEZ\\>$", "FERNANDEZ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<FED\\>+[.]", "FEDERICO ",iri$Direccion1)
iri$Direccion1 <- gsub("FELIP+[[:space:]]","FELIPE ",iri$Direccion1)

iri$Direccion1 <- gsub("\\<GCIA\\>+[[:space:]]", "GARCIA ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<GCIA\\>[.]+[[:space:]]", "GARCIA ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<GCIA\\>[.]", "GARCIA ",iri$Direccion1)

iri$Direccion1 <- gsub("\\<GLEZ\\>+[[:space:]]", "GONZALEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<GLEZ\\>[.]+[[:space:]]", "GONZALEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<GLEZ\\>[.]", "GONZALEZ ",iri$Direccion1)

iri$Direccion1 <- gsub("GRAL[.]+[[:space:]]","GENERAL ",iri$Direccion1)
iri$Direccion1 <- gsub("GRAL[.]","GENERAL ",iri$Direccion1)
iri$Direccion1 <- gsub("GRA[.]+[[:space:]]","GENERAL ",iri$Direccion1)
iri$Direccion1 <- gsub("GRA[.]","GENERAL ",iri$Direccion1)

iri$Direccion1 <- gsub("\\<HDEZ\\>+[[:space:]]", "HERNANDEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<HDEZ\\>[.]+[[:space:]]", "HERNANDEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<HDEZ\\>[.]", "HERNANDEZ ",iri$Direccion1)

iri$Direccion1 <- gsub("HIST[.]+[[:space:]]","HISTORIADOR ",iri$Direccion1) 
iri$Direccion1 <- gsub("HIST[.]","HISTORIADOR ",iri$Direccion1) 

iri$Direccion1 <- gsub("HNOS[.]+[[:space:]]","HERMANOS ",iri$Direccion1)
iri$Direccion1 <- gsub("HNOS[.]","HERMANOS ",iri$Direccion1)
iri$Direccion1 <- gsub("HNOS+[[:space:]]","HERMANOS ",iri$Direccion1)

iri$Direccion1 <- gsub("ING[.]", "INGENIERO ",iri$Direccion1)
iri$Direccion1 <- gsub("INGEN[.]", "INGENIERO ",iri$Direccion1)

iri$Direccion1 <- gsub("JOAQ[.]", "JOAQUIN ",iri$Direccion1)

iri$Direccion1 <- gsub("JURIZMENDI","JUDIZMENDI",iri$Direccion1)

iri$Direccion1 <- gsub("KALEA$","",iri$Direccion1) 
iri$Direccion1 <- gsub("\\<KALE\\>","",iri$Direccion1) 

iri$Direccion1 <- gsub("M[ª]", "MARIA",iri$Direccion1)
iri$Direccion1 <- gsub("M[*]", "MARIA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<M\\>[.]", "MARIA ",iri$Direccion1)
iri$Direccion1 <- gsub("^M+[[:space:]]", "MARIA ",iri$Direccion1)   
iri$Direccion1 <- gsub("[[:space:]]+M+[[:space:]]", "MARIA ",iri$Direccion1) 

iri$Direccion1 <- gsub("\\<MTNEZ\\>+[[:space:]]", "MARTINEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<MTNEZ\\>[.]+[[:space:]]", "MARTINEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<MTNEZ\\>[.]", "MARTINEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<MNEZ\\>+[[:space:]]", "MARTINEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<MNEZ\\>[.]+[[:space:]]", "MARTINEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<MNEZ\\>[.]", "MARTINEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<MONTSE\\>", "MONTSERRAT",iri$Direccion1)

iri$Direccion1 <- gsub("\\<N\\>[.]+[[:space:]]+\\<SRA\\>[.]+[[:space:]]", "NUESTRA SEÑORA ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<N\\>[.]+[[:space:]]+\\<SRA\\>[.]", "NUESTRA SEÑORA ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<N\\>[.]+\\<SRA\\>[.]+[[:space:]]", "NUESTRA SEÑORA ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<N\\>[.]+\\<SRA\\>[.]", "NUESTRA SEÑORA ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<NTRA\\>[.]+[[:space:]]", "NUESTRA ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<NTRA\\>[.]", "NUESTRA ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<NTRA\\>+[[:space:]]", "NUESTRA ",iri$Direccion1)

iri$Direccion1 <- gsub("\\<NTRO\\>[.]+[[:space:]]", "NUESTRO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<NTRO\\>[.]", "NUESTRO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<NTRO\\>+[[:space:]]", "NUESTRO ",iri$Direccion1)



iri$Direccion1 <- gsub("\\<JOSE ORTEGA Y GASSET\\>", "ORTEGA Y GASSET",iri$Direccion1)
iri$Direccion1 <- gsub("\\<JOSE ORTEGA GASSET\\>", "ORTEGA Y GASSET",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ORTEGA GASSET\\>", "ORTEGA Y GASSET",iri$Direccion1)

iri$Direccion1 <- gsub("\\<PERIOD\\>+[[:space:]]", "PERIODISTA ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PERIOD\\>[.]", "PERIODISTA ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PINTOR JOAQUIN SOROLLA\\>", "SOROLLA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PINTOR SOROLLA\\>", "SOROLLA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<JOAQUIN SOROLLA\\>", "SOROLLA",iri$Direccion1)

iri$Direccion1 <- gsub("\\<REY JUAN CARLOS I\\>", "JUAN CARLOS I",iri$Direccion1)
iri$Direccion1 <- gsub("\\<RDGUEZ\\>+[[:space:]]", "RODRIGUEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<RDGUEZ\\>[.]+[[:space:]]", "RODRIGUEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<RDGUEZ\\>[.]", "RODRIGUEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<RGUEZ\\>[.]+[[:space:]]","RODRIGUEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<RGUEZ\\>[.]","RODRIGUEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<RDEZ\\>+[[:space:]]", "RODRIGUEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<RDEZ\\>[.]+[[:space:]]", "RODRIGUEZ ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<RDEZ\\>[.]", "RODRIGUEZ ",iri$Direccion1)

iri$Direccion1 <- gsub("\\<SRA\\>[.]+[[:space:]]", "SEÑORA ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<SRA\\>[.]", "SEÑORA ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<SRA\\>+[[:space:]]", "SEÑORA ",iri$Direccion1)
iri$Direccion1 <- gsub("SINDICA[.]+[[:space:]]", "SINDICALISTA ",iri$Direccion1)
iri$Direccion1 <- gsub("SINDICA+[[:space:]]", "SINDICALISTA ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<SINDICA\\>[.]", "SINDICALISTA ",iri$Direccion1)

iri$Direccion1 <- gsub("STOS+[[:space:]]", "SANTOS ",iri$Direccion1)
iri$Direccion1 <- gsub("STOS[.]+[[:space:]]", "SANTOS ",iri$Direccion1)
iri$Direccion1 <- gsub("STOS[.]", "SANTOS ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<STOS\\>", "SANTOS",iri$Direccion1)
iri$Direccion1 <- gsub("\\<STO\\>[.]+[[:space:]]", "SANTO ",iri$Direccion1)   
iri$Direccion1 <- gsub("\\<STO\\>[.]", "SANTO ",iri$Direccion1)   
iri$Direccion1 <- gsub("\\<STO\\>+[[:space:]]", "SANTO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<STA\\>[.]+[[:space:]]", "SANTA ",iri$Direccion1)   
iri$Direccion1 <- gsub("\\<STA\\>[.]", "SANTA ",iri$Direccion1)   
iri$Direccion1 <- gsub("\\<STA\\>+[[:space:]]", "SANTA ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ST\\>+[[:space:]]", "SANTO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ST\\>[.]+[[:space:]]", "SANTO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ST\\>[.]", "SANTO ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<S\\>[.]+[[:space:]]", "SAN ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<S\\>[.]", "SAN ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<S\\>+[[:space:]]", "SAN ",iri$Direccion1)



#======================================================================================================
iri$Direccion1 <- gsub("KM+[[:space:]]+[0-9]+$", "",iri$Direccion1)
iri$Direccion1 <- gsub("KM-[0-9]+$", "",iri$Direccion1)
iri$Direccion1 <- gsub("\\<KM\\>$", "",iri$Direccion1)
iri$Direccion1 <- gsub("\\<LOC\\>[.]", "",iri$Direccion1)
iri$Direccion1 <- gsub("\\<L\\>[.]", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+\\<LOCAL\\>$", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+[[:space:]]+\\<LOCAL\\>$", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+[[:space:]]+\\<LOCAL\\>$", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+\\<LOCALES\\>$", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+\\-+[[:space:]]+\\<LOCALES\\>$", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+[[:space:]]+\\<LOCALES\\>$", "",iri$Direccion1)
iri$Direccion1 <- gsub("\\<LOCAL\\>$", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+\\<B\\>$", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+\\<BLOQUE\\>$", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+\\<BLQ\\>$", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+\\<BLQ\\>[.]$", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+\\<P\\>[.]$", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+\\<P\\>[-]$", "",iri$Direccion1)

iri$Direccion1 <- gsub("[[:space:]]+\\<DELS\\>+[[:space:]]"," DE LOS ",iri$Direccion1) 
iri$Direccion1 <- gsub("[[:space:]]+\\<DEL\\>+[[:space:]]"," ",iri$Direccion1)   
iri$Direccion1 <- gsub("[[:space:]]+\\<DE\\>+[[:space:]]"," ",iri$Direccion1) 



#Translates
iri$Direccion1 <- gsub("\\<AGUSTI\\>", "AGUSTIN",iri$Direccion1)
iri$Direccion1 <- gsub("\\<AJUNTAMENT\\>","AYUNTAMIENTO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ALACANT\\>", "ALICANTE",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ALBERT\\>", "ALBERTO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ALFONS\\>", "ALFONSO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ALFRED\\>", "ALFREDO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ALINYO\\>", "ALIÑO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ALMOGAVERS\\>", "ALMOGAVERES",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ALPS\\>", "ALPES",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ANNA\\>", "ANA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ANDALUSIA\\>", "ANDALUCIA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ANGELS\\>", "ANGELES",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ANSELM\\>", "ANSELMO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ANTIC\\>", "ANTIGUO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ANTONI\\>", "ANTONIO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ARCADI\\>", "ARCADIO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ARQUEBISBE\\>", "ARZOBISPO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ARMADES\\>", "ARMADAS",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ARTESANS\\>", "ARTESANOS",iri$Direccion1)

iri$Direccion1 <- gsub("\\<BAIX\\>", "BAJO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<BISBE\\>", "OBISPO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<BISCAIA\\>", "VIZCAYA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<BOTICARI\\>", "BOTICARIO",iri$Direccion1)

iri$Direccion1 <- gsub("\\<CAMINAS\\>", "CAMINOS",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CAMIL\\>", "CAMILO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CAMP\\>", "CAMPO",iri$Direccion1)
iri$Direccion1  <- gsub("\\<CANALETES\\>", "CANALETAS",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CAPITA\\>", "CAPITAN",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CAPUTXINS\\>", "CAPUCHINOS",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CARME\\>", "CARMEN",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CARLES\\>", "CARLOS",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CASTELLO\\>", "CASTELLON",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CATALUNYA\\>", "CATALUÑA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CATOLICS\\>", "CATOLICOS",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CIMADEVILA\\>", "CIMADEVILLA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CIRCUMVALACIO\\>", "CIRCUNVALACION",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CIUTAT\\>", "CIUDAD",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CIUTADANS\\>", "CIUDADANOS",iri$Direccion1)
iri$Direccion1 <- gsub("\\<COLOM\\>", "COLON",iri$Direccion1)
iri$Direccion1 <- gsub("\\<COMERÇ\\>", "COMERCIO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<COMTE\\>", "CONDE",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CONCA\\>", "CUENCA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CONCELLO\\>", "CONCEJO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CONCILI\\>", "CONCILIO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CONSTITUCIO\\>", "CONSTITUCION",iri$Direccion1)
iri$Direccion1 <- gsub("\\<COR\\>", "CORAZON",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CORNELI\\>", "CORNELIO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CREU\\>", "CRUZ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CRIST\\>", "CRISTO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CRISTOFOR\\>", "CRISTOBAL",iri$Direccion1)
iri$Direccion1 <- gsub("\\<CRISTOVAL\\>", "CRISTOBAL",iri$Direccion1)

iri$Direccion1 <- gsub("\\<DESEMPARATS\\>", "DESAMPARADOS",iri$Direccion1)
iri$Direccion1 <- gsub("\\<DEU\\>+[[:space:]]", "DIOS ",iri$Direccion1)
iri$Direccion1 <- gsub("^\\<12\\>", "DOCE",iri$Direccion1)
iri$Direccion1 <- gsub("\\<DOUTOR\\>", "DOCTOR",iri$Direccion1)
iri$Direccion1 <- gsub("\\<DIPUTACIO\\>", "DIPUTACION",iri$Direccion1)
iri$Direccion1 <- gsub("\\<D'ELX\\>", "DE ELCHE",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ELX\\>", "ELCHE",iri$Direccion1)

iri$Direccion1 <- gsub("\\<EDUARD\\>", "EDUARDO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<EGLESIA\\>", "IGLESIA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ENGINYER\\>", "INGENIERO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ENRIC\\>", "ENRIQUE",iri$Direccion1)
iri$Direccion1 <- gsub("\\<L'ESGLESIA\\>", "LA IGLESIA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ESGLESIA\\>", "IGLESIA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ESPANYA\\>", "ESPAÑA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<L'ESTACIO\\>", "LA ESTACION",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ESTACIO\\>", "ESTACION",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ESTUDIS\\>", "ESTUDIOS",iri$Direccion1)
iri$Direccion1 <- gsub("\\<EXERCITO\\>", "EJERCITO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<EXERCIT\\>", "EJERCITO",iri$Direccion1)

iri$Direccion1 <- gsub("\\<FLORS\\>", "FLORES",iri$Direccion1)
iri$Direccion1 <- gsub("\\<FORCES\\>", "FUERZAS",iri$Direccion1)
iri$Direccion1 <- gsub("\\<FRA\\>", "FRAY",iri$Direccion1)
iri$Direccion1 <- gsub("\\<FRANCESC\\>", "FRANCISCO",iri$Direccion1)

iri$Direccion1 <- gsub("\\<GAUDENCI\\>", "GAUDENCIO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<GERVASI\\>", "GERVASIO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<GIRONA\\>", "GERONA",iri$Direccion1)

iri$Direccion1 <- gsub("\\<HOMENS\\>", "HOMBRES",iri$Direccion1)

iri$Direccion1 <- gsub("[[:space:]]+I+[[:space:]]", " Y ",iri$Direccion1) #conjunción copulativa: Pi y Margall, etc.
iri$Direccion1 <- gsub("\\<IGNASI\\>","IGNACIO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ILLAS\\>","ISLAS",iri$Direccion1)
iri$Direccion1 <- gsub("\\<INDUSTRI\\>","INDUSTRIA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ISIDRE\\>","ISIDRO",iri$Direccion1)

iri$Direccion1 <- gsub("\\<JACINT\\>","JACINTO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<JAUME\\>","JAIME",iri$Direccion1)
iri$Direccion1 <- gsub("\\<JOAN\\>", "JUAN",iri$Direccion1)
iri$Direccion1 <- gsub("\\<JOAQUIM\\>", "JOAQUIN ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<JORDI\\>", "JORGE",iri$Direccion1)
iri$Direccion1 <- gsub("\\<JULIOL\\>", "JULIO",iri$Direccion1)

iri$Direccion1 <- gsub("\\<L'ESCORIAL\\>", "EL ESCORIAL",iri$Direccion1)

iri$Direccion1 <- gsub("\\<LLEIDA\\>", "LERIDA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<LIBERDADE\\>", "LIBERTAD",iri$Direccion1)
iri$Direccion1 <- gsub("\\<LLIBERTAD\\>", "LIBERTAD",iri$Direccion1)
iri$Direccion1 <- gsub("\\<LLIBERTAT\\>", "LIBERTAD",iri$Direccion1)
iri$Direccion1 <- gsub("\\<LLUCIA\\>", "LUCIA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<LLUIS\\>", "LUIS",iri$Direccion1)

iri$Direccion1 <- gsub("\\<MACIA\\>", "MACIAS",iri$Direccion1)
iri$Direccion1 <- gsub("\\<MAIG\\>", "MAYO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<MAGISTRAT\\>", "MAGISTRADO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<MAJOR\\>", "MAYOR",iri$Direccion1)
iri$Direccion1 <- gsub("\\<MARE\\>+[[:space:]]", "MADRE ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<MARITIM\\>", "MARITIMO",iri$Direccion1) #We replace exactly "maritim" not words which contain "maritim".
iri$Direccion1 <- gsub("\\<MARIÑA\\>", "MARINA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<MERCAT\\>", "MERCADO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<MESTRE\\>", "MAESTRO",iri$Direccion1)

iri$Direccion1 <- gsub("\\<NAPOLS\\>", "NAPOLES",iri$Direccion1)
iri$Direccion1 <- gsub("\\<NATURAIS\\>", "NATURALES",iri$Direccion1)
iri$Direccion1 <- gsub("\\<NOVA\\>", "NUEVA",iri$Direccion1)

iri$Direccion1 <- gsub("\\<ONZE\\>", "ONCE",iri$Direccion1)
iri$Direccion1 <- gsub("\\<OURENSE\\>", "ORENSE",iri$Direccion1)
iri$Direccion1 <- gsub("\\<O SABIO\\>", "EL SABIO",iri$Direccion1)

iri$Direccion1 <- gsub("\\<PALMERES\\>", "PALMERAS",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PARAGUAI\\>", "PARAGUAY",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PARALEL\\>", "PARALELO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PARE\\>", "PADRE",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PARLAMENT\\>", "PARLAMENTO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PASQUAL\\>", "PASCUAL",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PASSEIG\\>", "PASEO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PAU\\>", "PAZ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PEP\\>", "PEPE",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PERE\\>", "PEDRO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PLATJA\\>", "PLAYA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<POBRESA\\>", "POBREZA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PORT\\>", "PUERTO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PRESIDENT\\>", "PRESIDENTE",iri$Direccion1)
iri$Direccion1 <- gsub("\\<PRIMER\\>", "PRIMERO",iri$Direccion1)

iri$Direccion1 <- gsub("\\<REGNE\\>", "REINO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<REIS\\>", "REYES",iri$Direccion1)
iri$Direccion1 <- gsub("\\<RIU\\>", "RIO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<ROBERT\\>", "ROBERTO",iri$Direccion1)

iri$Direccion1 <- gsub("\\<SAGRAT\\>", "SAGRADO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<SANT\\>", "SAN",iri$Direccion1)
iri$Direccion1 <- gsub("\\<SANTISSIMA\\>", "SANTISIMA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<SANTISSIM\\>", "SANTISIMO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<SAVI\\>", "SABIO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<SEBASTIA\\>", "SEBASTIAN",iri$Direccion1)
iri$Direccion1 <- gsub("\\<SEGON\\>", "SEGUNDO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<SENYORA\\>", "SEÑORA",iri$Direccion1)
iri$Direccion1 <- gsub("\\<SETEMBRE\\>", "SEPTIEMBRE",iri$Direccion1)
iri$Direccion1 <- gsub("\\<SETZE\\>", "SIETE",iri$Direccion1)

iri$Direccion1 <- gsub("\\<TAQUIGRAF\\>", "TAQUIGRAFO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<TEIXEIRO\\>", "TEIJEIRO",iri$Direccion1)
iri$Direccion1 <- gsub("\\<TEMPLERS\\>", "TEMPLARIOS",iri$Direccion1)
iri$Direccion1 <- gsub("^\\<13\\>", "TRECE",iri$Direccion1)
iri$Direccion1 <- gsub("\\<TRINITAT\\>", "TRINIDAD",iri$Direccion1)
iri$Direccion1 <- gsub("\\<TRIOMF\\>", "TRIUNFO",iri$Direccion1)

iri$Direccion1 <- gsub("\\<UNIVERSITAT\\>", "UNIVERSIDAD",iri$Direccion1)

iri$Direccion1 <- gsub("\\<VERGE\\>", "VIRGEN",iri$Direccion1)
iri$Direccion1 <- gsub("\\<VICENS\\>", "VICENTE",iri$Direccion1)
iri$Direccion1 <- gsub("\\<VICENT\\>", "VICENTE",iri$Direccion1)

iri$Direccion1 <- gsub("\\<XAVIER\\>", "JAVIER",iri$Direccion1)
iri$Direccion1 <- gsub("\\<XOSE\\>", "JOSE",iri$Direccion1)
iri$Direccion1 <- gsub("\\<XUNQUEIRA\\>", "JUNQUERA",iri$Direccion1)



iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.


iri$Direccion1 <- gsub("\\-+$", "", iri$Direccion1)  #=============== We remove the hyphens on the right.
iri$Direccion1 <- gsub("[[:space:]]+$", "", iri$Direccion1)  #=============== We remove all the spaces on the right.
iri$Direccion1 <- gsub("\\-+$", "", iri$Direccion1)  #=============== We remove the hyphens on the right.


iri$Direccion1 <- gsub("^\\<DELS\\>+[[:space:]]","",iri$Direccion1)  
iri$Direccion1 <- gsub("^\\<DEL\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DE\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DOS\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DO\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^\\<DAS\\>+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^\\<DA\\>+[[:space:]]","",iri$Direccion1) 
iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.
iri$Direccion1 <- gsub("^LA+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^LO+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^EL+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^LAS+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^LOS+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^O+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^OS+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^A+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^AS+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^ELS+[[:space:]]","",iri$Direccion1)
iri$Direccion1 <- gsub("^LES+[[:space:]]","",iri$Direccion1)

iri$Direccion1 <- gsub("^[[:space:]]", "", iri$Direccion1)  #=============== We remove the spaces on the left.



#NATIONAL ROADS
iri$Direccion1 <- gsub("\\<N\\>[.]+[[:space:]]", "NACIONAL ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<N\\>[.]", "NACIONAL ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<N\\>[-]+[[:space:]]", "NACIONAL ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<N\\>[-]", "NACIONAL ",iri$Direccion1)
iri$Direccion1 <- gsub("\\<NACIONAL\\>[-]", "NACIONAL ",iri$Direccion1)
iri$Direccion1 <- gsub("KM[.]+[[:space:]]+[0-9]+$", "",iri$Direccion1)
iri$Direccion1 <- gsub("\\<KM\\>[.]", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+$", "", iri$Direccion1)  #=============== We remove the spaces on the right.


iri$Direccion1 <- gsub("\\/+[[:space:]]+\\/","",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+$", "", iri$Direccion1)  #=============== We remove the spaces on the right.

#===============================================================================================PORTAL=======================================================
iri$Direccion1 <- gsub("[[:space:]]+\\<P\\>$", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+\\<PORTAL\\>[[:space:]]+[0-9]+$", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+$", "", iri$Direccion1)  #=============== We remove the spaces on the right.


iri$Direccion1 <- gsub("[[:space:]]+\\<SOLAR\\>+[[:space:]]+[A-Z|0-9]+$", "",iri$Direccion1)
iri$Direccion1 <- gsub("[[:space:]]+$", "", iri$Direccion1)  #=============== We remove the spaces on the right.

#================================================================Changes in Pleis' addresses==================================================================================
#We remove tildes in addresses from Pleis.
pleis$direccion <- gsub("Á", "A", pleis$direccion)
pleis$direccion <- gsub("É", "E", pleis$direccion)
pleis$direccion <- gsub("Í", "I", pleis$direccion)
pleis$direccion <- gsub("Ó", "O", pleis$direccion)
pleis$direccion <- gsub("Ú", "U", pleis$direccion)
pleis$direccion <- gsub("Ü", "U", pleis$direccion)

#=====================================================================================================================================

# pleis$direccion <- gsub("^\\<AV\\>+[[:space:]]", "", pleis$direccion)    
# pleis$direccion <- gsub("^\\AV\\>[.]+[[:space:]]", "", pleis$direccion)
# pleis$direccion <- gsub("^\\<AV\\>[.]", "", pleis$direccion)
# pleis$direccion <- gsub("^\\<AVD\\>+[[:space:]]", "", pleis$direccion)
# pleis$direccion <- gsub("^\\<AVD\\>[.]+[[:space:]]", "", pleis$direccion)
# pleis$direccion <- gsub("^\\<AVD\\>[.]", "", pleis$direccion)
# pleis$direccion <- gsub("^\\<AVDA\\>[.]+[[:space:]]", "", pleis$direccion)
# pleis$direccion <- gsub("^\\<AVDA\\>[.]", "", pleis$direccion)
# pleis$direccion <- gsub("^\\<AVDA\\>,+[[:space:]]", "", pleis$direccion)
# pleis$direccion <- gsub("^\\<AVDA\\>+[[:space:]]", "", pleis$direccion)
# pleis$direccion <- gsub("^\\<AVDA>\\", "", pleis$direccion)
# pleis$direccion <- gsub("^\\<AVENIDA>\\+[[:space:]]", "", pleis$direccion)
# pleis$direccion <- gsub("^\\<AVINGUDA\\>+[[:space:]]", "", pleis$direccion)

pleis$direccion <- gsub("^AV+[[:space:]]", "", pleis$direccion)    
pleis$direccion <- gsub("^AV[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^AV[.]", "", pleis$direccion)
pleis$direccion <- gsub("^AVD+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^AVD[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^AVD[.]", "", pleis$direccion)
pleis$direccion <- gsub("^AVDA[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^AVDA[.]", "", pleis$direccion)
pleis$direccion <- gsub("^AVDA,+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^AVDA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^AVDA", "", pleis$direccion)
pleis$direccion <- gsub("^AVENIDA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^AVINGUDA+[[:space:]]", "", pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.



pleis$direccion <- gsub("^ANT+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^ANT[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^ANT[.]", "", pleis$direccion)
pleis$direccion <- gsub("^ANTIGUA+[[:space:]]", "", pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^ARR+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^ARR[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^ARR[.]", "", pleis$direccion)
pleis$direccion <- gsub("^ARRAVAL+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^ARRABAL+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^RAVAL+[[:space:]]", "", pleis$direccion)
mi_gsub(from, to, iri$Direccion1)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^AUT+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^AUT[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^AUT[.]", "", pleis$direccion)
pleis$direccion <- gsub("^AUTOPISTA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^AUTOVIA+[[:space:]]", "", pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.



pleis$direccion <- gsub("^BARRIO+[[:space:]]", "", pleis$direccion)   
pleis$direccion <- gsub("^BARRIAL+[[:space:]]", "", pleis$direccion) 
pleis$direccion <- gsub("^BARRIS+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^BARRO+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^BAR[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^BAR[.]", "", pleis$direccion)
pleis$direccion <- gsub("^BA[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^BA[.]", "", pleis$direccion)
pleis$direccion <- gsub("^B[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^B[.]", "", pleis$direccion)
pleis$direccion <- gsub("^BDA[.]", "", pleis$direccion)
pleis$direccion <- gsub("^BDA[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^BD[.]", "", pleis$direccion)
pleis$direccion <- gsub("^BD[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^BD+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^BARRIADA+[[:space:]]", "", pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^BOULEVARD+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^BOULEVAR+[[:space:]]","",pleis$direccion)  
pleis$direccion <- gsub("^BULEVARD+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^BULEVAR+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^BOULE+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^BOULE[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^BOULE[.]", "", pleis$direccion)
pleis$direccion <- gsub("^BO[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^BO[.]", "", pleis$direccion)
pleis$direccion <- gsub("\\<BO\\>+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^BU[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^BU[.]", "", pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^C[.]C[.]+[[:space:]]", "", pleis$direccion) 
pleis$direccion <- gsub("^C[.]C[.]", "", pleis$direccion) 
pleis$direccion <- gsub("^CC[.]+[[:space:]]", "", pleis$direccion) 
pleis$direccion <- gsub("^C[.]C+[[:space:]]", "", pleis$direccion) 
pleis$direccion <- gsub("^CC[.]", "", pleis$direccion) 
pleis$direccion <- gsub("^CC+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CENTRO COMERCIAL+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^C[.] COMERCIAL+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^\\<CENTRO CIAL\\>[.]", "", pleis$direccion)
pleis$direccion <- gsub("^CENTRO C[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CENTRO C[.]", "", pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^C[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^C[.]", "", pleis$direccion)
pleis$direccion <- gsub("^C+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CALLEJON+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CALLE+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^C[/]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^C[/]", "", pleis$direccion)
pleis$direccion <- gsub("^CL+[[:space:]]","",pleis$direccion)  
pleis$direccion <- gsub("^CL[.]+[[:space:]]","",pleis$direccion)  
pleis$direccion <- gsub("^CL[.]","",pleis$direccion)  
pleis$direccion <- gsub("^CA[.]+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^CA[.]","",pleis$direccion) 
pleis$direccion <- gsub("^CA+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^COL[.]+[[:space:]]","",pleis$direccion) 
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]"," ",pleis$direccion)
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]"," ",pleis$direccion)  
pleis$direccion <- gsub("^\\<EL\\>+[[:space:]]"," ",pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.

pleis$direccion <- gsub("^CAM+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CAM[.]", "", pleis$direccion)
pleis$direccion <- gsub("^CAM[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CAMI+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CAMI[.]", "", pleis$direccion)
pleis$direccion <- gsub("^CAMI[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CAMINO+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CNO+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CNO[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CNO[.]", "", pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^CAR+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CAR[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CAR[.]", "", pleis$direccion)
pleis$direccion <- gsub("^CARR+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CARR[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CARR[.]", "", pleis$direccion)
pleis$direccion <- gsub("^CARRER+[[:space:]]", "", pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^CARRETERA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CRTA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CRTA[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CRTA[.]", "", pleis$direccion)
pleis$direccion <- gsub("^CRA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CRA[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CRA[.]", "", pleis$direccion)
pleis$direccion <- gsub("^CRT+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CRT[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CRT[.]", "", pleis$direccion)
pleis$direccion <- gsub("^CR+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CR[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CR[.]", "", pleis$direccion)
pleis$direccion <- gsub("^CTRA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CTRA[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CTRA[.]", "", pleis$direccion)
pleis$direccion <- gsub("^CTA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CTA[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CTA[.]", "", pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.





pleis$direccion <- gsub("^CORREDERA+[[:space:]]", "", pleis$direccion)
# pleis$direccion <- gsub("[^CORREDERA$]", "", pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion) 


pleis$direccion <- gsub("^PL+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PL[.]+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PL[.]","",pleis$direccion)
pleis$direccion <- gsub("^PLA+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PLA[.]+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PLA[.]","",pleis$direccion)
pleis$direccion <- gsub("^PLAÇA+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PLZ+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PLZ[.]+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PLZ[.]","",pleis$direccion)
pleis$direccion <- gsub("^PZ+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PZ[.]+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PZ[.]","",pleis$direccion)
pleis$direccion <- gsub("^PZA+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PZA[.]+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PZA[.]","",pleis$direccion)
pleis$direccion <- gsub("^PLAZA+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PRAZA+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PTDA[.]+[[:space:]]","",pleis$direccion)  
pleis$direccion <- gsub("^PTDA[.]","",pleis$direccion)
pleis$direccion <- gsub("^PARTIDA ","",pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^PTO+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PTO[.]+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PTO[.]","",pleis$direccion)
pleis$direccion <- gsub("^PUERTO+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.





pleis$direccion <- gsub("^GTA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^GTA[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^GTA[.]", "", pleis$direccion)
pleis$direccion <- gsub("^GLO+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^GLO[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^GLO[.]", "", pleis$direccion)
pleis$direccion <- gsub("^\\<GLORIETA\\>", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^GPO+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^GPO[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^GPO[.]", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^LUG+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^LUG[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^LUG[.]", "", pleis$direccion)
pleis$direccion <- gsub("^LUGAR+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("\\<NUEVE\\>", "9",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^PSO+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PSO[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PO[.]", "", pleis$direccion)
pleis$direccion <- gsub("^PO+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PO[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PSO[.]", "", pleis$direccion)
pleis$direccion <- gsub("^PAS+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PAS[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PAS[.]", "", pleis$direccion)
pleis$direccion <- gsub("^PS+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PS[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PS[.]", "", pleis$direccion)
pleis$direccion <- gsub("^PASEO+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.



pleis$direccion <- gsub("\\<PARCELA\\>+[[:space:]]", " ",pleis$direccion)
pleis$direccion <- gsub("\\<PARCELA\\>", "",pleis$direccion)
pleis$direccion <- gsub("\\<PORCEL\\>+[[:space:]]", " ",pleis$direccion)
pleis$direccion <- gsub("\\<PORCEL\\>", "",pleis$direccion)
pleis$direccion <- gsub("\\<PARCEL\\>+[[:space:]]", " ",pleis$direccion)
pleis$direccion <- gsub("\\<PARCEL\\>", "",pleis$direccion)
pleis$direccion <- gsub("\\<PARC\\>[.]+[[:space:]]", " ",pleis$direccion)
pleis$direccion <- gsub("\\<PARC\\>[.]", " ",pleis$direccion)
pleis$direccion <- gsub("\\<PARC\\>", " ",pleis$direccion)
pleis$direccion <- gsub("\\<PORC\\>[.]+[[:space:]]", " ",pleis$direccion)
pleis$direccion <- gsub("\\<PORC\\>[.]", " ",pleis$direccion)
pleis$direccion <- gsub("\\<PORC\\>+[[:space:]]", " ",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^PJE+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PJE[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PJE[.]", "", pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.

pleis$direccion <- gsub("^PQE[.]+[[:space:]]+NACIONAL", "", pleis$direccion)
pleis$direccion <- gsub("^PQE+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PQE[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PQE[.]", "", pleis$direccion)
pleis$direccion <- gsub("^PQUE+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PQUE[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PQUE[.]+CIAL", "", pleis$direccion)
pleis$direccion <- gsub("^PQUE[.]", "", pleis$direccion)
pleis$direccion <- gsub("^\\<PARQUE\\>", "", pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^POBLADO+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^P.E.+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^P.E+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^POL+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^POL[.]+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^POL[.]","",pleis$direccion)
pleis$direccion <- gsub("^PLG+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PLG[.]+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PLG[.]","",pleis$direccion)
pleis$direccion <- gsub("^PNO+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PNO[.]+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PNO[.]","",pleis$direccion)
pleis$direccion <- gsub("^PG+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PG[.]+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^PG[.]","",pleis$direccion)
pleis$direccion <- gsub("^P[.]I[.]+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^P[.]I[.]","",pleis$direccion)
pleis$direccion <- gsub("^\\<POLIGONO INDUSTRIAL\\>","",pleis$direccion)
pleis$direccion <- gsub("^POLIGONO+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.





pleis$direccion <- gsub("^RDA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^RDA[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^RDA[.]", "", pleis$direccion)
pleis$direccion <- gsub("^R+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^R[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^R[.]", "", pleis$direccion)
pleis$direccion <- gsub("^\\<RONDA\\>", "", pleis$direccion)
pleis$direccion <- gsub("^\\<RESIDENCIAL\\>", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^\\<RAMBLE\\>", "", pleis$direccion)
pleis$direccion <- gsub("^RAM+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^RAM[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^RAM[.]", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.



pleis$direccion <- gsub("^RSD+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^RSD[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^RSD[.]", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^SENDA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^SDA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^SDA[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^SDA[.]", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^TRV+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^TRV[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^TRV[.]", "", pleis$direccion)
pleis$direccion <- gsub("^TR+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^TR[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^TR[.]", "", pleis$direccion)
pleis$direccion <- gsub("^TRAV+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^TRAV[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^TRAV[.]", "", pleis$direccion)
pleis$direccion <- gsub("^TRVS+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^TRVS[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^TRVS[.]", "", pleis$direccion)
pleis$direccion <- gsub("^TRAVESIA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^TRAVESERA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^TRAVESSERA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^URB+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^URB[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^URB[.]", "", pleis$direccion)
pleis$direccion <- gsub("^URBANIZACION+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.



pleis$direccion <- gsub("^RUA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^RUA DA+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^RUA DAS+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^RUA DO+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^RUA DOS+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^RUA DE+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.





# pleis$direccion <- gsub("^[DE|DEL]+[[:space:]]","",pleis$direccion)  #COMPROBAR ESTOS CORCHETES
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion) 
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.
pleis$direccion <- gsub("^LA+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^LO+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^EL+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^LAS+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^LOS+[[:space:]]","",pleis$direccion)

pleis$direccion <- gsub("^O+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^OS+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^A+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^AS+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^ELS+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^LES+[[:space:]]","",pleis$direccion)

pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.



pleis$direccion <- gsub("^CAT+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CAT[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^CAT[.]", "", pleis$direccion)
pleis$direccion <- gsub("^CATEDRATICO+[[:space:]]", "", pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion) 


pleis$direccion <- gsub("^DOCTOR+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^DOUTOR+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^DOC+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^DOC[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^DOC[.]", "", pleis$direccion)
pleis$direccion <- gsub("^DC+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^DC[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^DC[.]", "", pleis$direccion)
pleis$direccion <- gsub("^DR+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^DR[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^DR[.]", "", pleis$direccion)
pleis$direccion <- gsub("^DTOR+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^DTOR[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^DTOR[.]", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.



pleis$direccion <- gsub("^DON+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^D[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^D[.]", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^MOSSEN+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.

pleis$direccion <- gsub("^OBISPO+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.

pleis$direccion <- gsub("^PIN+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PIN[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^PIN[.]", "", pleis$direccion)
pleis$direccion <- gsub("^PINTOR+[[:space:]]", "", pleis$direccion)
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^RAD+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^RAD[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^RAD[.]", "", pleis$direccion)
pleis$direccion <- gsub("^RADIOFONISTA", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.
mi_gsub(from, to, pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("^DON+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^D[.]+[[:space:]]", "", pleis$direccion)
pleis$direccion <- gsub("^D[.]", "", pleis$direccion)
pleis$direccion <- gsub("^D[*]", "", pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.



# pleis$direccion <- gsub("^[DE|DEL]+[[:space:]]","",pleis$direccion)  #COMPROBAR ESTOS CORCHETES
pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.
pleis$direccion <- gsub("^LA+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^LO+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^EL+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^LAS+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^LOS+[[:space:]]","",pleis$direccion)

pleis$direccion <- gsub("^O+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^OS+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^A+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^AS+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^ELS+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^LES+[[:space:]]","",pleis$direccion)

pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.

#==================================================================================================================

# pleis$direccion <- gsub("^D[']", "", pleis$direccion)
pleis$direccion <- gsub("D[']", "", pleis$direccion)

pleis$direccion <- gsub("^IND[.]", "", pleis$direccion) 



pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.

pleis$direccion <- gsub("^PROF[.]", "", pleis$direccion)


pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.


pleis$direccion <- gsub("[[:space:]]+\\<DELS\\>+[[:space:]]"," DE LOS ",pleis$direccion) 
pleis$direccion <- gsub("[[:space:]]+\\<DEL\\>+[[:space:]]"," ",pleis$direccion) 
pleis$direccion <- gsub("[[:space:]]+\\<DE\\>+[[:space:]]"," ",pleis$direccion)  


#We remove content in parenthesis at the end of direccion.
pleis$direccion <- gsub("[()]", "", pleis$direccion)

pleis$direccion <- gsub("[[:space:]]+\\<PLANTA\\>+[[:space:]]+[A-Z]+$","",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+$", "", pleis$direccion)  #=============== We remove the spaces on the right.

#We remove the number.
pleis$direccion <- gsub("[[:space:]]+BAJO+$","",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+[0-9]+$","",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+[0-9]+\\-+[A-Z]+$","",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+[Y]$","",pleis$direccion) #Numbers with preposition "Y" in the middle.
pleis$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\<PTA\\>$","",pleis$direccion) #Numbers with "pta" in the middle.


pleis$direccion <- gsub("[[:space:]]+[0-9]+\\-+[0-9]+\\-+[0-9]+$","",pleis$direccion) #For addresses with three numbers.
pleis$direccion <- gsub("[[:space:]]+[0-9]+\\-+[0-9]+$","",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+[0-9]+\\_+[0-9]+$","",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+[0-9]+\\,+[0-9]+$","",pleis$direccion) #Numbers with comma between then.
pleis$direccion <- gsub("[[:space:]]+[0-9]+\\-+$","",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+[0-9]+\\_+$","",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+[0-9]+$","",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+$","",pleis$direccion)


pleis$direccion <- gsub("[0-9]+\\_+[0-9]+$","",pleis$direccion)
pleis$direccion <- gsub("BAJO+$","",pleis$direccion)
# pleis$direccion <- gsub("[0-9]+$","",pleis$direccion)   #PONGO ESTO MÁS ABAJO
pleis$direccion <- gsub("[0-9]+\\-+[A-Z]+$","",pleis$direccion)
pleis$direccion <- gsub("[0-9]+[[:space:]]+\\-+[[:space:]]+[0-9]+[[:space:]]+[A-Z]+$","",pleis$direccion)
pleis$direccion <- gsub("[0-9]+[[:space:]]+[A-Z]+$","",pleis$direccion)
pleis$direccion <- gsub("[0-9]+[[:space:]]+[Y]$","",pleis$direccion) #Numbers with preposition "Y" in the middle.
pleis$direccion <- gsub("[0-9]+[[:space:]]+\\<PTA\\>$","",pleis$direccion) #Numbers with "pta" in the middle.
pleis$direccion <- gsub("[0-9]+[A-Z]+\\-+[0-9]+[A-Z]+$","",pleis$direccion) #add THIS=============================================================%%%%%%%%%%%%%%%%%%%%%%%%%%%
pleis$direccion <- gsub("[0-9]+[A-Z]+$","",pleis$direccion)

#To number with hyphen
pleis$direccion <- gsub("[0-9]+\\-+[0-9]+\\-+[0-9]+$","",pleis$direccion) #For addresses with three numbers.
pleis$direccion <- gsub("[0-9]+\\-+[0-9]+\\-$","",pleis$direccion)
pleis$direccion <- gsub("[0-9]+\\-+[0-9]+$","",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+[0-9]+\\,+[0-9]+\\,+[0-9]+$","",pleis$direccion) #Numbers with comma between then.
pleis$direccion <- gsub("[0-9]+\\,+[0-9]+$","",pleis$direccion) #Numbers with comma between then.
pleis$direccion <- gsub("[[:space:]]+[0-9]+\\,+[[:space:]]+[0-9]+\\,+[[:space:]]+[0-9]+$","",pleis$direccion) #Numbers with comma and spaces between then.
pleis$direccion <- gsub("[0-9]+[/]+[0-9]+$","",pleis$direccion) #Numbers with slash between then.
pleis$direccion <- gsub("[0-9]+\\-+$","",pleis$direccion)
#There are numbers plus hyphen plus space plus number.
pleis$direccion <- gsub("[0-9]+[[:space:]]+\\-+[0-9]+$","",pleis$direccion)
pleis$direccion <- gsub("[0-9]+[[:space:]]+\\-+$","",pleis$direccion)

pleis$direccion <- gsub("[0-9]+\\-+[[:space:]]+[A-Z]+\\º$","",pleis$direccion)
pleis$direccion <- gsub("[0-9]+\\-+[A-Z]+\\º$","",pleis$direccion)
##===========================================================================================================================

pleis$direccion <- gsub("[0-9]+$","",pleis$direccion)
#And the S/N.
pleis$direccion <- gsub("[0-9]+\\,+$","",pleis$direccion)
pleis$direccion <- gsub(" S[/]N$","",pleis$direccion)
pleis$direccion <- gsub("S[/]N$","",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+\\<SN\\>$","",pleis$direccion)
pleis$direccion <- gsub(" S[-]N$","",pleis$direccion)
pleis$direccion <- gsub("S[-]N$","",pleis$direccion)
pleis$direccion <- gsub(" N[º]$","",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+N[.]$","",pleis$direccion)
pleis$direccion <- gsub("N[.][0-9]$","",pleis$direccion) #We already removed the comma.
pleis$direccion <- gsub("[[:space:]]+N[.][0-9]$","",pleis$direccion)
# #We remove the comma at the end.
pleis$direccion <- gsub(",$","",pleis$direccion)

pleis$direccion <- gsub("[0-9]+\\-+[[:space:]]+[A-Z]+\\º$","",pleis$direccion) #===============================================================BAJO============================
pleis$direccion <- gsub("[0-9]+\\-+[A-Z]+\\º$","",pleis$direccion)



#====================================================================================================================================

pleis$direccion <- gsub("[[:space:]]+\\<XENDA\\>+$","",pleis$direccion)
pleis$direccion <- gsub("\\<COMPANY\\>","COMPANYS",pleis$direccion)
# pleis$direccion <- gsub("ALEXANDRE ROSELLO","ALEXANDRE ROSSELLO",pleis$direccion)
pleis$direccion <- gsub("\\<ROSELLO\\>","ROSSELLO",pleis$direccion)

pleis$direccion <- gsub("\\<ANTON\\>[.]+[[:space:]]", "ANTONIO ",pleis$direccion)
pleis$direccion <- gsub("\\<ANTON\\>[.]", "ANTONIO ",pleis$direccion)

pleis$direccion <- gsub("ARQUI[.]+[[:space:]]","ARQUITECTO ",pleis$direccion)
pleis$direccion <- gsub("\\<ARQUI\\>[.]","ARQUITECTO ",pleis$direccion)   
pleis$direccion <- gsub("ARQUIT[.]+[[:space:]]","ARQUITECTO ",pleis$direccion)
pleis$direccion <- gsub("ARQUIT[.]","ARQUITECTO ",pleis$direccion)

pleis$direccion <- gsub("AUZOA$","",pleis$direccion)

pleis$direccion <- gsub("^CTE[.]+[[:space:]]", "COMANDANTE ", pleis$direccion)
pleis$direccion <- gsub("^CTE[.]", "COMANDANTE ", pleis$direccion)
pleis$direccion <- gsub("^CTE+[[:space:]]", "COMANDANTE ", pleis$direccion)

pleis$direccion <- gsub("\\CONJTO\\>[.]+[[:space:]]", "CONJUNTO ", pleis$direccion)
pleis$direccion <- gsub("\\CONJTO\\>[.]", "CONJUNTO ", pleis$direccion)

pleis$direccion <- gsub("\\<EDIF\\>[.]+[[:space:]]", "EDIFICIO ",pleis$direccion)
pleis$direccion <- gsub("\\<EDIF\\>[.]", "EDIFICIO ",pleis$direccion)
pleis$direccion <- gsub("\\<EDI\\>[.]+[[:space:]]", "EDIFICIO ",pleis$direccion)
pleis$direccion <- gsub("\\<EDI\\>[.]", "EDIFICIO ",pleis$direccion)
pleis$direccion <- gsub("\\<EDF\\>[.]+[[:space:]]", "EDIFICIO ",pleis$direccion)
pleis$direccion <- gsub("\\<EDF\\>[.]", "EDIFICIO ",pleis$direccion)
pleis$direccion <- gsub("\\<ED\\>[.]+[[:space:]]", "EDIFICIO ",pleis$direccion)
pleis$direccion <- gsub("\\<ED\\>[.]", "EDIFICIO ",pleis$direccion)

pleis$direccion  <- gsub("\\<ESCRIT\\>[.]+[[:space:]]", "ESCRITOR ",pleis$direccion )
pleis$direccion  <- gsub("\\<ESCRIT\\>[.]", "ESCRITOR ",pleis$direccion )
pleis$direccion  <- gsub("\\<ESCRIT\\>[.]+[[:space:]]", "ESCRITOR ",pleis$direccion )

pleis$direccion <- gsub("FCO+[[:space:]]","FRANCISCO ",pleis$direccion)
pleis$direccion <- gsub("FCO[.]+[[:space:]]","FRANCISCO ",pleis$direccion)
pleis$direccion <- gsub("FCO[.]","FRANCISCO ",pleis$direccion)
pleis$direccion <- gsub("\\<FDEZ\\>+[[:space:]]", "FERNANDEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<FDEZ\\>[.]+[[:space:]]", "FERNANDEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<FDEZ\\>[.]", "FERNANDEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<FDEZ\\>$", "FERNANDEZ",pleis$direccion)
pleis$direccion <- gsub("\\<FED\\>+[.]", "FEDERICO ",pleis$direccion)
pleis$direccion <- gsub("FELIP+[[:space:]]","FELIPE ",pleis$direccion)
pleis$direccion <- gsub("FERNANDO CATOLICO+[[:space:]]","FERNANDO EL CATOLICO ",pleis$direccion)
pleis$direccion <- gsub("FRA+[[:space:]]","FRAY ",pleis$direccion)

pleis$direccion <- gsub("\\<GCIA\\>+[[:space:]]", "GARCIA ",pleis$direccion)
pleis$direccion <- gsub("\\<GCIA\\>[.]+[[:space:]]", "GARCIA ",pleis$direccion)
pleis$direccion <- gsub("\\<GCIA\\>[.]", "GARCIA ",pleis$direccion)

pleis$direccion <- gsub("\\<GLEZ\\>+[[:space:]]", "GONZALEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<GLEZ\\>[.]+[[:space:]]", "GONZALEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<GLEZ\\>[.]", "GONZALEZ ",pleis$direccion)

pleis$direccion <- gsub("GRAL[.]","GENERAL ",pleis$direccion)
pleis$direccion <- gsub("GRAL+[[:space:]]","GENERAL ",pleis$direccion)
pleis$direccion <- gsub("GRA[.]","GENERAL ",pleis$direccion)
# pleis$direccion <- gsub("GRA+[[:space:]]","GENERAL ",pleis$direccion)   #==================================================================================

pleis$direccion <- gsub("\\<HDEZ\\>+[[:space:]]", "HERNANDEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<HDEZ\\>[.]+[[:space:]]", "HERNANDEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<HDEZ\\>[.]", "HERNANDEZ ",pleis$direccion)

pleis$direccion <- gsub("HIST[.]+[[:space:]]","HISTORIADOR ",pleis$direccion) 
pleis$direccion <- gsub("HIST[.]","HISTORIADOR ",pleis$direccion) 

pleis$direccion <- gsub("HNOS+[[:space:]]","HERMANOS ",pleis$direccion)
pleis$direccion <- gsub("HNOS[.]+[[:space:]]","HERMANOS ",pleis$direccion)
pleis$direccion <- gsub("HNOS[.]","HERMANOS ",pleis$direccion)

pleis$direccion <- gsub("ING[.]", "INGENIERO ",pleis$direccion)
pleis$direccion <- gsub("INGEN[.]", "INGENIERO ",pleis$direccion)

pleis$direccion <- gsub("JOAQ[.]", "JOAQUIN ",pleis$direccion)

pleis$direccion <- gsub("JURIZMENDI","JUDIZMENDI",pleis$direccion)

pleis$direccion <- gsub("KALEA$","",pleis$direccion) 
pleis$direccion <- gsub("\\<KALE\\>","",pleis$direccion) 

pleis$direccion <- gsub("M[ª]", "MARIA",pleis$direccion)
pleis$direccion <- gsub("M[*]", "MARIA",pleis$direccion)  #I doubt about this, because Manuel, Mario, etc.
pleis$direccion <- gsub("\\<M\\>[.]", "MARIA ",pleis$direccion)
pleis$direccion <- gsub("^M+[[:space:]]", "MARIA ",pleis$direccion)   
pleis$direccion <- gsub("[[:space:]]+M+[[:space:]]", "MARIA ",pleis$direccion)


pleis$direccion <- gsub("\\<MTNEZ\\>+[[:space:]]", "MARTINEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<MTNEZ\\>[.]+[[:space:]]", "MARTINEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<MTNEZ\\>[.]", "MARTINEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<MNEZ\\>+[[:space:]]", "MARTINEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<MNEZ\\>[.]+[[:space:]]", "MARTINEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<MNEZ\\>[.]", "MARTINEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<MONTSE\\>", "MONTSERRAT",pleis$direccion)

pleis$direccion <- gsub("\\<N\\>[.]+[[:space:]]+\\<SRA\\>[.]+[[:space:]]", "NUESTRA SEÑORA ",pleis$direccion)
pleis$direccion <- gsub("\\<N\\>[.]+[[:space:]]+\\<SRA\\>[.]", "NUESTRA SEÑORA ",pleis$direccion)
pleis$direccion <- gsub("\\<N\\>[.]+\\<SRA\\>[.]+[[:space:]]", "NUESTRA SEÑORA ",pleis$direccion)
pleis$direccion <- gsub("\\<N\\>[.]+\\<SRA\\>[.]", "NUESTRA SEÑORA ",pleis$direccion)
pleis$direccion <- gsub("\\<NTRA\\>[.]+[[:space:]]", "NUESTRA ",pleis$direccion)
pleis$direccion <- gsub("\\<NTRA\\>[.]", "NUESTRA ",pleis$direccion)
pleis$direccion <- gsub("\\<NTRA\\>+[[:space:]]", "NUESTRA ",pleis$direccion)

pleis$direccion <- gsub("\\<NTRO\\>[.]+[[:space:]]", "NUESTRO ",pleis$direccion)
pleis$direccion <- gsub("\\<NTRO\\>[.]", "NUESTRO ",pleis$direccion)
pleis$direccion <- gsub("\\<NTRO\\>+[[:space:]]", "NUESTRO ",pleis$direccion)

pleis$direccion <- gsub("\\<JOSE ORTEGA Y GASSET\\>", "ORTEGA Y GASSET", pleis$direccion)
pleis$direccion <- gsub("\\<JOSE ORTEGA GASSET\\>", "ORTEGA Y GASSET",pleis$direccion)
pleis$direccion <- gsub("\\<ORTEGA GASSET\\>", "ORTEGA Y GASSET",pleis$direccion)

pleis$direccion <- gsub("\\<PERIOD\\>+[[:space:]]", "PERIODISTA ",pleis$direccion)
pleis$direccion <- gsub("\\<PERIOD\\>[.]", "PERIODISTA ",pleis$direccion)
pleis$direccion <- gsub("\\<PINTOR JOAQUIN SOROLLA\\>", "SOROLLA",pleis$direccion)
pleis$direccion <- gsub("\\<PINTOR SOROLLA\\>", "SOROLLA",pleis$direccion)
pleis$direccion <- gsub("\\<JOAQUIN SOROLLA\\>", "SOROLLA",pleis$direccion)

pleis$direccion <- gsub("\\<REY JUAN CARLOS I\\>", "JUAN CARLOS I", pleis$direccion)
pleis$direccion <- gsub("\\<RDGUEZ\\>+[[:space:]]", "RODRIGUEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<RDGUEZ\\>[.]+[[:space:]]", "RODRIGUEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<RDGUEZ\\>[.]", "RODRIGUEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<RGUEZ\\>[.]+[[:space:]]","RODRIGUEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<RGUEZ\\>[.]","RODRIGUEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<RDEZ\\>+[[:space:]]", "RODRIGUEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<RDEZ\\>[.]+[[:space:]]", "RODRIGUEZ ",pleis$direccion)
pleis$direccion <- gsub("\\<RDEZ\\>[.]", "RODRIGUEZ ",pleis$direccion)

pleis$direccion <- gsub("\\<SRA\\>[.]+[[:space:]]", "SEÑORA ",pleis$direccion)
pleis$direccion <- gsub("\\<SRA\\>[.]", "SEÑORA ",pleis$direccion)
pleis$direccion <- gsub("\\<SRA\\>+[[:space:]]", "SEÑORA ",pleis$direccion)
pleis$direccion <- gsub("SINDICA[.]+[[:space:]]", "SINDICALISTA ",pleis$direccion)
pleis$direccion <- gsub("SINDICA+[[:space:]]", "SINDICALISTA ",pleis$direccion)
pleis$direccion <- gsub("\\<SINDICA\\>[.]", "SINDICALISTA ",pleis$direccion)

pleis$direccion <- gsub("STOS+[[:space:]]", "SANTOS ",pleis$direccion)
pleis$direccion <- gsub("STOS[.]+[[:space:]]", "SANTOS ",pleis$direccion)
pleis$direccion <- gsub("STOS[.]", "SANTOS ",pleis$direccion)
pleis$direccion <- gsub("\\<STOS\\>", "SANTOS",pleis$direccion)
pleis$direccion <- gsub("\\<STO\\>[.]+[[:space:]]", "SANTO ",pleis$direccion) 
pleis$direccion <- gsub("\\<STO\\>[.]", "SANTO ",pleis$direccion) 
pleis$direccion <- gsub("\\<STO\\>+[[:space:]]", "SANTO ",pleis$direccion)
pleis$direccion <- gsub("\\<STA\\>[.]+[[:space:]]", "SANTA ",pleis$direccion) 
pleis$direccion <- gsub("\\<STA\\>[.]", "SANTA ",pleis$direccion) 
pleis$direccion <- gsub("\\<STA\\>+[[:space:]]", "SANTA ",pleis$direccion)
pleis$direccion <- gsub("\\<ST\\>+[[:space:]]", "SANTO ",pleis$direccion)
pleis$direccion <- gsub("\\<ST\\>[.]+[[:space:]]", "SANTO ",pleis$direccion)
pleis$direccion <- gsub("\\<ST\\>[.]", "SANTO ",pleis$direccion)
pleis$direccion <- gsub("\\<S\\>[.]+[[:space:]]", "SAN ",pleis$direccion)
pleis$direccion <- gsub("\\<S\\>[.]", "SAN ",pleis$direccion)
pleis$direccion <- gsub("\\<S\\>+[[:space:]]", "SAN ",pleis$direccion)


pleis$direccion <- gsub("[[:space:]]+\\<DELS\\>+[[:space:]]"," DE LOS ",pleis$direccion) 
pleis$direccion <- gsub("[[:space:]]+\\<DEL\\>+[[:space:]]"," ",pleis$direccion) 
pleis$direccion <- gsub("[[:space:]]+\\<DE\\>+[[:space:]]"," ",pleis$direccion)  


#Numbers
pleis$direccion <- gsub("KM+[[:space:]]+[0-9]+$", "",pleis$direccion)
pleis$direccion <- gsub("KM-[0-9]+$", "",pleis$direccion)
pleis$direccion <- gsub("\\<KM\\>$", "",pleis$direccion)
pleis$direccion <- gsub("\\<LOC\\>[.]", "",pleis$direccion)
pleis$direccion <- gsub("\\<L\\>[.]", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+[0-9]+\\-+\\<LOCAL\\>$", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+[0-9]+\\-+[[:space:]]+\\<LOCAL\\>$", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+[[:space:]]+\\<LOCAL\\>$", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+[0-9]+\\-+\\<LOCALES\\>$", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+[0-9]+\\-+[[:space:]]+\\<LOCALES\\>$", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+[[:space:]]+\\<LOCALES\\>$", "",pleis$direccion)
pleis$direccion <- gsub("\\<LOCAL\\>$", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+\\<B\\>$", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+\\<BLOQUE\\>$", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+\\<BLQ\\>$", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+\\<BLQ\\>[.]$", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+\\<P\\>[.]$", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+\\<P\\>[-]$", "",pleis$direccion)

pleis$direccion <- gsub("[[:space:]]+\\<DELS\\>+[[:space:]]"," DE LOS ",pleis$direccion) 
pleis$direccion <- gsub("[[:space:]]+\\<DEL\\>+[[:space:]]"," ",pleis$direccion) 
pleis$direccion <- gsub("[[:space:]]+\\<DE\\>+[[:space:]]"," ",pleis$direccion)  

#Translates
pleis$direccion <- gsub("\\<AGUSTI\\>", "AGUSTIN",pleis$direccion)
pleis$direccion <- gsub("\\<AJUNTAMENT\\>","AYUNTAMIENTO",pleis$direccion)
pleis$direccion <- gsub("\\<ALACANT\\>", "ALICANTE",pleis$direccion)
pleis$direccion <- gsub("\\<ALBERT\\>", "ALBERTO",pleis$direccion)
pleis$direccion <- gsub("\\<ALFONS\\>", "ALFONSO",pleis$direccion)
pleis$direccion <- gsub("\\<ALFRED\\>", "ALFREDO",pleis$direccion)
pleis$direccion <- gsub("\\<ALINYO\\>", "ALIÑO",pleis$direccion)
pleis$direccion <- gsub("\\<ALMOGAVERS\\>", "ALMOGAVERES",pleis$direccion)
pleis$direccion <- gsub("\\<ALPS\\>", "ALPES",pleis$direccion)
pleis$direccion <- gsub("\\<ANNA\\>", "ANA",pleis$direccion)
pleis$direccion <- gsub("\\<ANDALUSIA\\>", "ANDALUCIA",pleis$direccion)
pleis$direccion <- gsub("\\<ANGELS\\>", "ANGELES",pleis$direccion)
pleis$direccion <- gsub("\\<ANSELM\\>", "ANSELMO",pleis$direccion)
pleis$direccion <- gsub("\\<ANTIC\\>", "ANTIGUO",pleis$direccion)
pleis$direccion <- gsub("\\<ANTONI\\>", "ANTONIO",pleis$direccion)
pleis$direccion <- gsub("\\<ARQUEBISBE\\>", "ARZOBISPO",pleis$direccion)
pleis$direccion <- gsub("\\<ARCADI\\>", "ARCADIO",pleis$direccion)
pleis$direccion <- gsub("\\<ARMADES\\>", "ARMADAS",pleis$direccion)
pleis$direccion <- gsub("\\<ARTESANS\\>", "ARTESANOS",pleis$direccion)

pleis$direccion <- gsub("\\<BAIX\\>", "BAJO",pleis$direccion)
pleis$direccion <- gsub("\\<BISBE\\>", "OBISPO",pleis$direccion)
pleis$direccion <- gsub("\\<BISCAIA\\>", "VIZCAYA",pleis$direccion)
pleis$direccion <- gsub("\\<BOTICARI\\>", "BOTICARIO",pleis$direccion)

pleis$direccion <- gsub("\\<CAMIL\\>", "CAMILO",pleis$direccion)
pleis$direccion <- gsub("\\<CAMINAS\\>", "CAMINOS",pleis$direccion)
pleis$direccion <- gsub("\\<CAMIL\\>", "CAMILO",pleis$direccion)
pleis$direccion <- gsub("\\<CAMP\\>", "CAMPO",pleis$direccion)
pleis$direccion <- gsub("\\<CANALETES\\>", "CANALETAS",pleis$direccion)
pleis$direccion <- gsub("\\<CAPITA\\>", "CAPITAN",pleis$direccion)
pleis$direccion <- gsub("\\<CAPUTXINS\\>", "CAPUCHINOS",pleis$direccion)
pleis$direccion <- gsub("\\<CARME\\>", "CARMEN",pleis$direccion)
pleis$direccion <- gsub("\\<CARLES\\>", "CARLOS",pleis$direccion)
pleis$direccion <- gsub("\\<CASTELLO\\>", "CASTELLON",pleis$direccion)
pleis$direccion <- gsub("\\<CATALUNYA\\>", "CATALUÑA",pleis$direccion)
pleis$direccion <- gsub("\\<CATOLICS\\>", "CATOLICOS",pleis$direccion)
pleis$direccion <- gsub("\\<CIMADEVILA\\>", "CIMADEVILLA",pleis$direccion)
pleis$direccion <- gsub("\\<CIRCUMVALACIO\\>", "CIRCUNVALACION",pleis$direccion)
pleis$direccion <- gsub("\\<CIUTAT\\>", "CIUDAD",pleis$direccion)
pleis$direccion <- gsub("\\<CIUTADANS\\>", "CIUDADANOS",pleis$direccion)
pleis$direccion <- gsub("\\<COLOM\\>", "COLON",pleis$direccion)
pleis$direccion <- gsub("\\<COMERÇ\\>", "COMERCIO",pleis$direccion)
pleis$direccion <- gsub("\\<COMTE\\>+[[:space:]]", "CONDE ",pleis$direccion)
pleis$direccion <- gsub("\\<CONCA\\>", "CUENCA",pleis$direccion)
pleis$direccion <- gsub("\\<CONCELLO\\>", "CONCEJO",pleis$direccion)
pleis$direccion <- gsub("\\<CONCILI\\>", "CONCILIO",pleis$direccion)
pleis$direccion <- gsub("\\<CONSTITUCIO\\>", "CONSTITUCION",pleis$direccion)
pleis$direccion <- gsub("\\<COR\\>", "CORAZON",pleis$direccion)
pleis$direccion <- gsub("\\<CORNELI\\>", "CORNELIO",pleis$direccion)
pleis$direccion <- gsub("\\<CREU\\>", "CRUZ",pleis$direccion)
pleis$direccion <- gsub("\\<CRIST\\>", "CRISTO",pleis$direccion)
pleis$direccion <- gsub("\\<CRISTOFOR\\>", "CRISTOBAL",pleis$direccion)
pleis$direccion <- gsub("\\<CRISTOVAL\\>", "CRISTOBAL",pleis$direccion)

pleis$direccion <- gsub("\\<DESEMPARATS\\>", "DESAMPARADOS",pleis$direccion)
pleis$direccion <- gsub("\\<DEU\\>+[[:space:]]", "DIOS ",pleis$direccion)
pleis$direccion <- gsub("\\<DOUTOR\\>", "DOCTOR",pleis$direccion)
pleis$direccion <- gsub("^\\<12\\>", "DOCE",pleis$direccion)
pleis$direccion <- gsub("\\<DIPUTACIO\\>", "DIPUTACION",pleis$direccion)
pleis$direccion <- gsub("\\<D'ELX\\>", "DE ELCHE",pleis$direccion)
pleis$direccion <- gsub("\\<ELX\\>", "ELCHE",pleis$direccion)

pleis$direccion <- gsub("\\<EDUARD\\>", "EDUARDO",pleis$direccion)
pleis$direccion <- gsub("\\<EGLESIA\\>", "IGLESIA",pleis$direccion)
pleis$direccion <- gsub("\\<ENGINYER\\>", "INGENIERO",pleis$direccion)
pleis$direccion <- gsub("\\<ENRIC\\>", "ENRIQUE",pleis$direccion)
pleis$direccion <- gsub("\\<L'ESGLESIA\\>", "LA IGLESIA",pleis$direccion)
pleis$direccion <- gsub("\\<ESGLESIA\\>", "IGLESIA",pleis$direccion)
pleis$direccion <- gsub("\\<ESPANYA\\>", "ESPAÑA",pleis$direccion)
pleis$direccion <- gsub("\\<L'ESTACIO\\>", "LA ESTACION",pleis$direccion)
pleis$direccion <- gsub("\\<ESTACIO\\>", "ESTACION",pleis$direccion)
pleis$direccion <- gsub("\\<ESTUDIS\\>", "ESTUDIOS",pleis$direccion)
pleis$direccion <- gsub("\\<EXERCITO\\>", "EJERCITO",pleis$direccion)
pleis$direccion <- gsub("\\<EXERCIT\\>", "EJERCITO",pleis$direccion)

pleis$direccion <- gsub("\\<FLORS\\>", "FLORES",pleis$direccion)
pleis$direccion <- gsub("\\<FORCES\\>", "FUERZAS",pleis$direccion)
pleis$direccion <- gsub("\\<FRA\\>", "FRAY",pleis$direccion)
pleis$direccion <- gsub("\\<FRANCESC\\>", "FRANCISCO",pleis$direccion)

pleis$direccion <- gsub("\\<GAUDENCI\\>", "GAUDENCIO",pleis$direccion)
pleis$direccion <- gsub("\\<GERVASI\\>", "GERVASIO",pleis$direccion)
pleis$direccion <- gsub("\\<GIRONA\\>", "GERONA",pleis$direccion)

pleis$direccion <- gsub("\\<HOMENS\\>", "HOMBRES",pleis$direccion)

pleis$direccion <- gsub("[[:space:]]+I+[[:space:]]", " Y ",pleis$direccion) #conjunción copulativa: Pi y Margall, etc.
pleis$direccion <- gsub("\\<IGNASI\\>","IGNACIO",pleis$direccion)
pleis$direccion <- gsub("\\<ILLAS\\>","ISLAS",pleis$direccion)
pleis$direccion <- gsub("\\<INDUSTRI\\>","INDUSTRIA",pleis$direccion)
pleis$direccion <- gsub("\\<ISIDRE\\>","ISIDRO",pleis$direccion)

pleis$direccion <- gsub("\\<JACINT\\>","JACINTO",pleis$direccion)
pleis$direccion <- gsub("\\<JAUME\\>","JAIME",pleis$direccion)
pleis$direccion <- gsub("\\<JOAN\\>", "JUAN",pleis$direccion)
pleis$direccion <- gsub("\\<JOAQUIM\\>", "JOAQUIN ",pleis$direccion)
pleis$direccion <- gsub("\\<JORDI\\>", "JORGE",pleis$direccion)
pleis$direccion <- gsub("\\<JULIOL\\>", "JULIO",pleis$direccion)

pleis$direccion <- gsub("\\<L'ESCORIAL\\>", "EL ESCORIAL",pleis$direccion)

pleis$direccion <- gsub("\\<LLEIDA\\>", "LERIDA",pleis$direccion)
pleis$direccion <- gsub("\\<LIBERDADE\\>", "LIBERTAD",pleis$direccion)
pleis$direccion <- gsub("\\<LLIBERTAD\\>", "LIBERTAD",pleis$direccion)
pleis$direccion <- gsub("\\<LLIBERTAT\\>", "LIBERTAD",pleis$direccion)
pleis$direccion <- gsub("\\<LLUCIA\\>", "LUCIA",pleis$direccion)
pleis$direccion <- gsub("\\<LLUIS\\>", "LUIS",pleis$direccion)

pleis$direccion <- gsub("\\<MACIA\\>", "MACIAS",pleis$direccion)
pleis$direccion <- gsub("\\<MAGISTRAT\\>", "MAGISTRADO",pleis$direccion)
pleis$direccion <- gsub("\\<MAIG\\>", "MAYO",pleis$direccion)

pleis$direccion <- gsub("\\<MAJOR\\>", "MAYOR",pleis$direccion)

pleis$direccion <- gsub("\\<MARE\\>+[[:space:]]", "MADRE ",pleis$direccion)
pleis$direccion <- gsub("\\<MARITIM\\>", "MARITIMO",pleis$direccion)
pleis$direccion <- gsub("\\<MARIÑA\\>", "MARINA",pleis$direccion)
pleis$direccion <- gsub("\\<MERCAT\\>", "MERCADO",pleis$direccion)
pleis$direccion <- gsub("\\<MESTRE\\>", "MAESTRO",pleis$direccion)

pleis$direccion <- gsub("\\<NAPOLS\\>", "NAPOLES",pleis$direccion)
pleis$direccion <- gsub("\\<NATURAIS\\>", "NATURALES",pleis$direccion)
pleis$direccion <- gsub("\\<NOVA\\>", "NUEVA",pleis$direccion)

pleis$direccion <- gsub("\\<ONZE\\>", "ONCE",pleis$direccion)
pleis$direccion <- gsub("\\<OURENSE\\>", "ORENSE",pleis$direccion)
pleis$direccion <- gsub("\\<O SABIO\\>", "EL SABIO",pleis$direccion)

pleis$direccion <- gsub("\\<PALMERES\\>", "PALMERAS",pleis$direccion)
pleis$direccion <- gsub("\\<PARAGUAI\\>", "PARAGUAY",pleis$direccion)
pleis$direccion <- gsub("\\<PARALEL\\>", "PARALELO",pleis$direccion)
pleis$direccion <- gsub("\\<PARE\\>", "PADRE",pleis$direccion)
pleis$direccion <- gsub("\\<PARLAMENT\\>", "PARLAMENTO",pleis$direccion)
pleis$direccion <- gsub("\\<PASQUAL\\>", "PASCUAL",pleis$direccion)
pleis$direccion <- gsub("\\<PASSEIG\\>", "PASEO",pleis$direccion)
pleis$direccion <- gsub("\\<PAU\\>", "PAZ",pleis$direccion)
pleis$direccion <- gsub("\\<PEP\\>", "PEPE",pleis$direccion)
pleis$direccion <- gsub("\\<PERE\\>", "PEDRO",pleis$direccion)
pleis$direccion <- gsub("\\<PLATJA\\>", "PLAYA",pleis$direccion)
pleis$direccion<- gsub("\\<POBRESA\\>", "POBREZA",pleis$direccion)
pleis$direccion <- gsub("\\<PORT\\>", "PUERTO",pleis$direccion)
pleis$direccion <- gsub("\\<PRESIDENT\\>", "PRESIDENTE",pleis$direccion)
pleis$direccion <- gsub("\\<PRIMER\\>", "PRIMERO",pleis$direccion)

pleis$direccion <- gsub("\\<REGNE\\>", "REINO",pleis$direccion)
pleis$direccion <- gsub("\\<REIS\\>", "REYES",pleis$direccion)
pleis$direccion <- gsub("\\<RIU\\>", "RIO",pleis$direccion)
pleis$direccion <- gsub("\\<ROBERT\\>", "ROBERTO",pleis$direccion)

pleis$direccion <- gsub("\\<SAGRAT\\>", "SAGRADO",pleis$direccion)
pleis$direccion <- gsub("\\<SANT\\>", "SAN",pleis$direccion)
pleis$direccion <- gsub("\\<SANTISSIMA\\>", "SANTISIMA",pleis$direccion)
pleis$direccion <- gsub("\\<SANTISSIM\\>", "SANTISIMO",pleis$direccion)
pleis$direccion <- gsub("\\<SAVI\\>", "SABIO",pleis$direccion)
pleis$direccion <- gsub("\\<SEBASTIA\\>", "SEBASTIAN",pleis$direccion)
pleis$direccion <- gsub("\\<SEGON\\>", "SEGUNDO",pleis$direccion)
pleis$direccion <- gsub("\\<SENYORA\\>", "SEÑORA",pleis$direccion)
pleis$direccion <- gsub("\\<SETEMBRE\\>", "SEPTIEMBRE",pleis$direccion)
pleis$direccion <- gsub("\\<SETZE\\>", "SIETE",pleis$direccion)

pleis$direccion <- gsub("\\<TAQUIGRAF\\>", "TAQUIGRAFO",pleis$direccion)
pleis$direccion <- gsub("\\<TEIXEIRO\\>", "TEIJEIRO",pleis$direccion)
pleis$direccion <- gsub("\\<TEMPLERS\\>", "TEMPLARIOS",pleis$direccion)
pleis$direccion <- gsub("^\\<13\\>", "TRECE",pleis$direccion)
pleis$direccion <- gsub("\\<TRINITAT\\>", "TRINIDAD",pleis$direccion)
pleis$direccion <- gsub("\\<TRIOMF\\>", "TRIUNFO",pleis$direccion)

pleis$direccion <- gsub("\\<UNIVERSITAT\\>", "UNIVERSIDAD",pleis$direccion)

pleis$direccion <- gsub("\\<VERGE\\>", "VIRGEN",pleis$direccion)
pleis$direccion <- gsub("\\<VICENS\\>", "VICENTE",pleis$direccion)
pleis$direccion <- gsub("\\<VICENT\\>", "VICENTE",pleis$direccion)

pleis$direccion <- gsub("\\<XAVIER\\>", "JAVIER",pleis$direccion)
pleis$direccion <- gsub("\\<XOSE\\>", "JOSE",pleis$direccion)
pleis$direccion <- gsub("\\<XUNQUEIRA\\>", "JUNQUERA",pleis$direccion)
# 




pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.



pleis$direccion <- gsub("\\-+$", "", pleis$direccion)  #=============== We remove the hyphens on the right.
pleis$direccion <- gsub("[[:space:]]+$", "", pleis$direccion)  #=============== We remove all the spaces on the right.
pleis$direccion <- gsub("\\-+$", "", pleis$direccion)  #=============== We remove the hyphens on the right.




pleis$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DE\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DA\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^\\<DO\\>+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.
pleis$direccion <- gsub("^LA+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^LO+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^EL+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^LAS+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^LOS+[[:space:]]","",pleis$direccion)

pleis$direccion <- gsub("^O+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^OS+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^A+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^AS+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^ELS+[[:space:]]","",pleis$direccion)
pleis$direccion <- gsub("^LES+[[:space:]]","",pleis$direccion)

pleis$direccion <- gsub("^[[:space:]]", "", pleis$direccion)  #=============== We remove the spaces on the left.






#NATIONAL ROADS
pleis$direccion <- gsub("\\<N\\>[.]+[[:space:]]", "NACIONAL ",pleis$direccion)
pleis$direccion <- gsub("\\<N\\>[.]", "NACIONAL ",pleis$direccion)
pleis$direccion <- gsub("\\<N\\>[-]+[[:space:]]", "NACIONAL ",pleis$direccion)
pleis$direccion <- gsub("\\<N\\>[-]", "NACIONAL ",pleis$direccion)
pleis$direccion <- gsub("\\<NACIONAL\\>[-]", "NACIONAL ",pleis$direccion)
pleis$direccion <- gsub("KM[.]+[[:space:]]+[0-9]+$", "",pleis$direccion)
pleis$direccion <- gsub("\\<KM\\>[.]", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+$", "", pleis$direccion)  #=============== We remove the spaces on the right.



pleis$direccion <- gsub("\\/+[[:space:]]+\\/","",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+$", "", pleis$direccion)  #=============== We remove the spaces on the right.


#===============================================================================================PORTAL=======================================================
pleis$direccion <- gsub("[[:space:]]+\\<P\\>$", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+\\<PORTAL\\>[[:space:]]+[0-9]+$", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+$", "", pleis$direccion)  #=============== We remove the spaces on the right.

pleis$direccion <- gsub("[[:space:]]+\\<SOLAR\\>+[[:space:]]+[A-Z|0-9]+$", "",pleis$direccion)
pleis$direccion <- gsub("[[:space:]]+$", "", pleis$direccion)  #=============== We remove the spaces on the right.
#===============================================================================End of modifications of addresses ===============================================================0



#We create the candidate key: ensena-cp-direccion.
pleis$ensena_cp_direccion <- paste(pleis$ensena,"-",pleis$cp, "-", pleis$direccion)
iri$ensena_cp_direccion <- paste(iri$Enseña,"-",iri$`Código Postal`, "-", iri$Direccion1)

# head(pleis)


#Repeated elements in Pleis and Iri with this candidate key.
repeated_pleis <- which(duplicated(pleis$ensena_cp_direccion))
repeated_iri <- which(duplicated(iri$ensena_cp_direccion))

#To test repeated elements.
repeated_pleis_df <- pleis[repeated_pleis,]
repeated_iri_df < iri[repeated_iri,]

#We remove the repeated elements from Pleis and Iri.
pleis_not_rep <- pleis[-(repeated_pleis),]
iri_not_rep <- iri[-(repeated_iri),]

# View(head(iri_not_rep)) #traza

#TRAZA:
#iri_not_rep$Direccion1[iri_not_rep$`ID Tienda`==4727]

#We merge the two data frames by the candidate key.
# merge_iri_pleis <- merge(pleis_not_rep,iri_not_rep, by.x = "ensena_cp_direccion", by.y = "ensena_cp_direccion")
merge_iri_pleis <- merge(pleis_not_rep,iri_not_rep, by.x = "ensena_cp_direccion", by.y = "ensena_cp_direccion")

#TRAZA:
#merge_iri_pleis$Direccion1[merge_iri_pleis$`ID Tienda`==4727]

#View(merge_iri_pleis) #traza

#We obtain the indexed for duplicate values in ensena_cp_direccion with different "ID Tienda". = 0 elements.
# which(duplicated(merge_iri_pleis$`ID Tienda`)) 


#Left join, with dplyr, (with basis package doesn't run because they have different number of rows), to obtain all observations
#of Pleis and only their matches with Iris.
pleis_not_rep$m_code <- "pleis"
# iri_not_rep$m_code <- "iri"
iri_not_rep$m_code <- "iri"
# merge_iri_pleis_pleis <- left_join(pleis_not_rep, iri_not_rep, by = "ensena_cp_direccion")
merge_iri_pleis_pleis <- left_join(pleis_not_rep, iri_not_rep, by = "ensena_cp_direccion")
#View(merge_iri_pleis_pleis) #traza

#TRAZA:
#unique(merge_iri_pleis_pleis$Direccion1[merge_iri_pleis_pleis$`ID Tienda`==4727])

#List of repeated elements at the side of pleis, (0 elements)
# repeated_pleis_merge <- which(duplicated(merge_iri_pleis_pleis$ensena_cp_direccion))


#Right join between Pleis and Iris to obtain all observations of Iris and only their matches with Pleis.
# merge_iri_pleis_iri <- right_join(pleis_not_rep, iri_not_rep, by = "ensena_cp_direccion")
merge_iri_pleis_iri <- right_join(pleis_not_rep, iri_not_rep, by = "ensena_cp_direccion")
#View(merge_iri_pleis_iri) #traza

#List of repeated elements in Iri, (0 elements)
# repeated_iri <- which(duplicated(merge_iri_pleis_iri$ensena_cp_direccion))

#We need to do a "union", (similar to SQL union) of data frames.
#We add a new field to know whom belong each observation in the union.
pleis_iri_all <- rbind(merge_iri_pleis_pleis, merge_iri_pleis_iri) 

#View(pleis_iri_all)
#TRAZA:
#unique(pleis_iri_all$Direccion1[pleis_iri_all$`ID Tienda`==4727])


repeated_pleis_iri_all <- which(duplicated(pleis_iri_all$ensena_cp_direccion)) #It is the number of common observations in Pleis and Iri. This number has to be equal
#to the number of observations of "merge_iri_pleis".

#=====================================================================================================
#We are going to prepare data for looking at them in Qlik.
#We are going to extract two files with the elements that are in Pleis and in Iri but without common elements.

#We are going to match by provincia and poblacion; for this we need to adapt the provincia in Pleis.
merge_iri_pleis_pleis$provincia <- gsub("ALICANTE/ALACANT", "ALICANTE", merge_iri_pleis_pleis$provincia)
merge_iri_pleis_pleis$provincia <- gsub("BALEARS (ILLES)", "BALEARES", merge_iri_pleis_pleis$provincia)
merge_iri_pleis_pleis$provincia <- gsub("ARABA/ALAVA", "ALAVA", merge_iri_pleis_pleis$provincia)
merge_iri_pleis_pleis$provincia <- gsub("BIZKAIA", "VIZCAYA", merge_iri_pleis_pleis$provincia)
merge_iri_pleis_pleis$provincia <- gsub("CASTELLON/CASTELLO", "CASTELLON", merge_iri_pleis_pleis$provincia)
merge_iri_pleis_pleis$provincia <- gsub("CORUÑA (A)", "A CORUÑA", merge_iri_pleis_pleis$provincia)
merge_iri_pleis_pleis$provincia <- gsub("RIOJA (LA)", "LA RIOJA", merge_iri_pleis_pleis$provincia)
merge_iri_pleis_pleis$provincia <- gsub("VALENCIA/VALÈNCIA", "VALENCIA", merge_iri_pleis_pleis$provincia)

merge_iri_pleis_iri$provincia <- gsub("CORUÑA (A)", "A CORUÑA", merge_iri_pleis_iri$provincia)
merge_iri_pleis_iri$Municipio <- gsub("CORUÑA (A)", "A CORUÑA", merge_iri_pleis_iri$Municipio)


# View(merge_iri_pleis_iri[grepl("LEON",merge_iri_pleis_iri$provincia),]) #Test
# View(merge_iri_pleis_iri[grepl("CORUÑA",merge_iri_pleis_iri$provincia),])   
# View(merge_iri_pleis_pleis[grepl("CORUÑA",merge_iri_pleis_pleis$poblacion),])   
# 
# View(merge_iri_pleis_pleis[grepl("5934",merge_iri_pleis_pleis$cod),])   






#Elements in Pleis that not are common with Iri.
length(merge_iri_pleis_pleis$`ID Tienda`[is.na(merge_iri_pleis_pleis$`ID Tienda`)])
pleis_not_common_elements <- merge_iri_pleis_pleis[is.na(merge_iri_pleis_pleis$`ID Tienda`),]
#We add the next line to drawing the map in Qlik.
pleis_not_common_elements$CountryDataSourde <- rep("Spain")
write.csv(pleis_not_common_elements, file = "pleis_not_common_elements.csv")

#Elements in Iri that not are common with Pleis.
length(merge_iri_pleis_iri$cod[is.na(merge_iri_pleis_iri$cod)])
iri_not_common_elements <- merge_iri_pleis_iri[is.na(merge_iri_pleis_iri$cod),]
#We add the next line to drawing the map in Qlik.
iri_not_common_elements$CountryDataSourde <- rep("Spain")
write.csv(iri_not_common_elements, file = "iri_not_common_elements.csv")




#We are going to insert in a csv file the Pleis' cod and Id Tienda from Iri, to add to SQL Server.
write.csv(merge_iri_pleis[,c("cod", "ID Tienda")], file = "codes_pleis_iri.csv")

write.csv(repeated_pleis_df[,c("cod")], file = "repeated_elements_pleis.csv")








