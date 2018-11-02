#Royal Canin

# library(dplyr)
# library(readxl)
# 
# setwd("C:/Users/maria.purificacion/Documents/Royal_Canin")

rc <- read_excel("datos_11_09.xlsx")  



#We convert rc$Direccion1 and rc$ensena to upper case and remove tildes, (just in case).
rc$direccion <- toupper(rc$direccion)
rc$ensena <- toupper(rc$ensena)
rc$poblacion <- toupper(rc$poblacion)

#We exclude centers with ensena = TAREAS ADMINISTRATIVAS.
rc <- rc %>%
  filter(ensena != "TIENDA DE MASCOTAS") %>%
  filter(ensena != "TAREAS ADMINISTRATIVAS") %>%
  filter(ensena != "VETERINARIA")


from <- c("^[[:space:]]", "^LA+[[:space:]]", "^LO+[[:space:]]", "^EL+[[:space:]]", "^LAS+[[:space:]]", "^LOS+[[:space:]]", 
          "^A+[[:space:]]", "^O+[[:space:]]", "^AS+[[:space:]]", "^OS+[[:space:]]", "^LES+[[:space:]]", "^ELS+[[:space:]]")
to <- c("", "", "", "", "", "", "", "", "", "", "", "")

mi_gsub <- function(pattern, replacement, x, ...) {
  for(i in 1:length(pattern))
    x <<- gsub(pattern[i], replacement[i], x, ...)
  x
}



#We remove the accents.
rc$ensena <- gsub("Á", "A", rc$ensena)
rc$ensena <- gsub("É", "E", rc$ensena)
rc$ensena <- gsub("Í", "I", rc$ensena)
rc$ensena <- gsub("Ó", "O", rc$ensena)
rc$ensena <- gsub("Ú", "U", rc$ensena)
rc$ensena <- gsub("Ü", "U", rc$ensena)

rc$direccion <- gsub("Á", "A", rc$direccion)
rc$direccion <- gsub("É", "E", rc$direccion)
rc$direccion <- gsub("Í", "I", rc$direccion)
rc$direccion <- gsub("Ó", "O", rc$direccion)
rc$direccion <- gsub("Ú", "U", rc$direccion)
rc$direccion <- gsub("Ü", "U", rc$direccion)


#Acento invertido
rc$ensena <- gsub("À", "A", rc$ensena)
rc$ensena <- gsub("È", "E", rc$ensena)
rc$ensena <- gsub("Ì", "I", rc$ensena)
rc$ensena <- gsub("Ò", "O", rc$ensena)
rc$ensena <- gsub("Ù", "U", rc$ensena)

rc$direccion <- gsub("Ä", "A", rc$direccion)
rc$direccion <- gsub("Ë", "E", rc$direccion)
rc$direccion <- gsub("Ï", "I", rc$direccion)
rc$direccion <- gsub("Ö", "O", rc$direccion)
rc$direccion <- gsub("Ü", "U", rc$direccion)

rc$direccion <- gsbu("Ç","C", rc$direccion)


#Changes upon addresses.
rc$direccion <- gsub("^AV+[[:space:]]", "", rc$direccion)     
rc$direccion <- gsub("^AV[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^AV[.]", "", rc$direccion)
rc$direccion <- gsub("^AVD+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^AVD[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^AVD[.]", "", rc$direccion)
rc$direccion <- gsub("^AVDA[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("AVDA[.]", "", rc$direccion)
rc$direccion <- gsub("^AVDA,+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^AVDA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^AVDA", "", rc$direccion)
rc$direccion <- gsub("^AVENIDA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^AVINGUDA+[[:space:]]", "", rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^ANT+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^ANT[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^ANT[.]", "", rc$direccion)
rc$direccion <- gsub("^ANTIGUA+[[:space:]]", "", rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^ARR+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^ARR[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^ARR[.]", "", rc$direccion)
rc$direccion <- gsub("^ARRAVAL+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^ARRABAL+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^RAVAL+[[:space:]]", "", rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^AUT+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^AUT[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^AUT[.]", "", rc$direccion)
rc$direccion <- gsub("^AUTOPISTA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^AUTOVIA+[[:space:]]", "", rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^BARRIO+[[:space:]]", "", rc$direccion)   
rc$direccion <- gsub("^BARRIAL+[[:space:]]", "", rc$direccion) 
rc$direccion <- gsub("^BARRIS+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^BARRO+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^BAR[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^BAR[.]", "", rc$direccion)
rc$direccion <- gsub("^BA[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^BA[.]", "", rc$direccion)
rc$direccion <- gsub("^B[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^B[.]", "", rc$direccion)
rc$direccion <- gsub("^BDA[.]", "", rc$direccion)
rc$direccion <- gsub("^BDA[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^BD[.]", "", rc$direccion)
rc$direccion <- gsub("^BD[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^BD+[[:space:]]", "", rc$direccion)

rc$direccion <- gsub("^BARRIADA+[[:space:]]", "", rc$direccion)

mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^BOULEVARD+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^BOULEVAR+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^BULEVARD+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^BULEVAR+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^BOULE+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^BOULE[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^BOULE[.]", "", rc$direccion)
rc$direccion <- gsub("^BO[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^BO[.]", "", rc$direccion)
rc$direccion <- gsub("\\<BO\\>+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^BU[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^BU[.]", "", rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^C[.]C[.]+[[:space:]]", "", rc$direccion) 
rc$direccion <- gsub("^C[.]C[.]", "", rc$direccion) 
rc$direccion <- gsub("^CC[.]+[[:space:]]", "", rc$direccion) 
rc$direccion <- gsub("^C[.]C+[[:space:]]", "", rc$direccion) 
rc$direccion <- gsub("^CC[.]", "", rc$direccion) 
rc$direccion <- gsub("^CC+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CENTRO COMERCIAL+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^C[.] COMERCIAL+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^\\<CENTRO CIAL\\>[.]", "", rc$direccion)
rc$direccion <- gsub("^CENTRO C[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CENTRO C[.]", "", rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.



rc$direccion <- gsub("^C[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^C[.]", "", rc$direccion)
rc$direccion <- gsub("^C+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CALLEJON+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CALLE+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^C[/]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^C[/]", "", rc$direccion)
rc$direccion <- gsub("^CL+[[:space:]]","",rc$direccion)  
rc$direccion <- gsub("^CL[.]+[[:space:]]","",rc$direccion)  
rc$direccion <- gsub("^CL[.]","",rc$direccion) 
rc$direccion <- gsub("^CA[.]+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^CA[.]","",rc$direccion) 
rc$direccion <- gsub("^CA+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^COL[.]+[[:space:]]","",rc$direccion) 
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion)  
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<EL\\>+[[:space:]]"," ",rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.

rc$direccion <- gsub("^CAM+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CAM[.]", "", rc$direccion)
rc$direccion <- gsub("^CAM[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CAMI+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CAMI[.]", "", rc$direccion)
rc$direccion <- gsub("^CAMI[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CAMINO+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CNO+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CNO[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CNO[.]", "", rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^CAR+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CAR[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CAR[.]", "", rc$direccion)
rc$direccion <- gsub("^CARR+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CARR[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CARR[.]", "", rc$direccion)
rc$direccion <- gsub("^CARRER+[[:space:]]", "", rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^CARRETERA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CRTA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CRTA[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CRTA[.]", "", rc$direccion)
rc$direccion <- gsub("^CRA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CRA[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CRA[.]", "", rc$direccion)
rc$direccion <- gsub("^CRT+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CRT[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CRT[.]", "", rc$direccion)
rc$direccion <- gsub("^CR+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CR[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CR[.]", "", rc$direccion)
rc$direccion <- gsub("^CTRA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CTRA[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CTRA[.]", "", rc$direccion)
rc$direccion <- gsub("^CTA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CTA[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CTA[.]", "", rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^CORREDERA+[[:space:]]", "", rc$direccion)
# rc$direccion <- gsub("[^CORREDERA$]", "", rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion) 


rc$direccion <- gsub("^PL+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PL[.]+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PL[.]","",rc$direccion)
rc$direccion <- gsub("^PLA+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PLA[.]+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PLA[.]","",rc$direccion)
rc$direccion <- gsub("^PLAÇA+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PLZ+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PLZ[.]+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PLZ[.]","",rc$direccion)
rc$direccion <- gsub("^PZ+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PZ[.]+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PZ[.]","",rc$direccion)
rc$direccion <- gsub("^PZA+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PZA[.]+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PZA[.]","",rc$direccion)
rc$direccion <- gsub("^PLAZA+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PRAZA+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PTDA[.]+[[:space:]]","",rc$direccion)  
rc$direccion <- gsub("^PTDA[.]","",rc$direccion)
rc$direccion <- gsub("^PARTIDA ","",rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.



rc$direccion <- gsub("^PTO+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PTO[.]+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PTO[.]","",rc$direccion)
rc$direccion <- gsub("^PUERTO+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.



rc$direccion <- gsub("^GTA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^GTA[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^GTA[.]", "", rc$direccion)
rc$direccion <- gsub("^GLO+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^GLO[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^GLO[.]", "", rc$direccion)
rc$direccion <- gsub("^\\<GLORIETA\\>", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^GPO+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^GPO[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^GPO[.]", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.



rc$direccion <- gsub("^LUG+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^LUG[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^LUG[.]", "", rc$direccion)
rc$direccion <- gsub("^LUGAR+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.

rc$direccion <- gsub("\\<NUEVE\\>", "9",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^PSO+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PSO[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PSO[.]", "", rc$direccion)
rc$direccion <- gsub("^PO+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PO[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PO[.]", "", rc$direccion)
rc$direccion <- gsub("^PAS+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PAS[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PAS[.]", "", rc$direccion)
rc$direccion <- gsub("^PS+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PS[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PS[.]", "", rc$direccion)
rc$direccion <- gsub("^PASEO+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.



rc$direccion <- gsub("\\<PARCELA\\>+[[:space:]]", " ",rc$direccion)
rc$direccion <- gsub("\\<PARCELA\\>", "",rc$direccion)
rc$direccion <- gsub("\\<PORCEL\\>+[[:space:]]", " ",rc$direccion)
rc$direccion <- gsub("\\<PORCEL\\>", "",rc$direccion)
rc$direccion <- gsub("\\<PARCEL\\>+[[:space:]]", " ",rc$direccion)
rc$direccion <- gsub("\\<PARCEL\\>", "",rc$direccion)
rc$direccion <- gsub("\\<PARC\\>[.]+[[:space:]]", " ",rc$direccion)
rc$direccion <- gsub("\\<PARC\\>[.]", " ",rc$direccion)
rc$direccion <- gsub("\\<PARC\\>", " ",rc$direccion)
rc$direccion <- gsub("\\<PORC\\>[.]+[[:space:]]", " ",rc$direccion)
rc$direccion <- gsub("\\<PORC\\>[.]", " ",rc$direccion)
rc$direccion <- gsub("\\<PORC\\>+[[:space:]]", " ",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^PJE+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PJE[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PJE[.]", "", rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.

rc$direccion <- gsub("^PQE[.]+[[:space:]]+NACIONAL", "", rc$direccion)
rc$direccion <- gsub("^PQE+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PQE[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PQE[.]", "", rc$direccion)
rc$direccion <- gsub("^PQUE+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PQUE[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PQUE[.]+CIAL", "", rc$direccion)
rc$direccion <- gsub("^PQUE[.]", "", rc$direccion)
rc$direccion <- gsub("^PQ+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PQ[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PQ[.]", "", rc$direccion)
rc$direccion <- gsub("^\\<PARQUE\\>", "", rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^POB[.]+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^POB[.]","",rc$direccion)
rc$direccion <- gsub("^POBLA+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^POBLADO+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^P[.]E[.]+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^P[.]E+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^POL+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^POL[.]+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^POL[.]","",rc$direccion)
rc$direccion <- gsub("^PLG+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PLG[.]+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PLG[.]","",rc$direccion)
rc$direccion <- gsub("^PNO+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PNO[.]+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PNO[.]","",rc$direccion)
rc$direccion <- gsub("^PG+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PG[.]+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^PG[.]","",rc$direccion)
rc$direccion <- gsub("^P[.]I[.]+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^P[.]I[.]","",rc$direccion)
rc$direccion <- gsub("^\\<POLIGONO INDUSTRIAL\\>","",rc$direccion)
rc$direccion <- gsub("^POLIGONO+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.



rc$direccion <- gsub("^RDA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^RDA[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^RDA[.]", "", rc$direccion)
rc$direccion <- gsub("^R+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^R[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^R[.]", "", rc$direccion)
rc$direccion <- gsub("^\\<RONDA\\>", "", rc$direccion)
rc$direccion <- gsub("^\\<RESIDENCIAL\\>", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.



rc$direccion <- gsub("^\\<RAMBLA\\>", "", rc$direccion)
rc$direccion <- gsub("^\\<RAMBLE\\>", "", rc$direccion)
rc$direccion <- gsub("^\\<RBLA\\>[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^\\<RBLA\\>+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^RAM+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^RAM[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^RAM[.]", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^RSD+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^RSD[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^RSD[.]", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^SENDA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^SDA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^SDA[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^SDA[.]", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^TRV+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^TRV[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^TRV[.]", "", rc$direccion)
rc$direccion <- gsub("^TR+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^TR[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^TR[.]", "", rc$direccion)
rc$direccion <- gsub("^TRAV+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^TRAV[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^TRAV[.]", "", rc$direccion)
rc$direccion <- gsub("^TRVS+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^TRVS[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^TRVS[.]", "", rc$direccion)
rc$direccion <- gsub("^TRAVESIA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^TRAVESERA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^TRAVESSERA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^URB+[[:space:]]", "", rc$direccion)  #Mirar si interesa eliminar las urb que aparecen a mitad de dirección.
rc$direccion <- gsub("^URB[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^URB[.]", "", rc$direccion)
rc$direccion <- gsub("^URBANIZACION+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.



rc$direccion <- gsub("^RUA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^RUA DA+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^RUA DAS+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^RUA DO+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^RUA DOS+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^RUA DE+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^VIA+[[:space:]]", "", rc$direccion)  #Mirar si interesa eliminar las urb que aparecen a mitad de dirección.
rc$direccion <- gsub("^VIA[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^VIA[.]", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
rc$direccion <- gsub("^LA+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^LO+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^EL+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^LAS+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^LOS+[[:space:]]","",rc$direccion)

rc$direccion <- gsub("^O+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^OS+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^A+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^AS+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^ELS+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^LES+[[:space:]]","",rc$direccion)

rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.

rc$direccion <- gsub("^CAT+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CAT[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^CAT[.]", "", rc$direccion)
rc$direccion <- gsub("^CATEDRATICO+[[:space:]]", "", rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion) 


rc$direccion <- gsub("^DOCTOR+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^DOUTOR+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^DOC+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^DOC[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^DOC[.]", "", rc$direccion)
rc$direccion <- gsub("^DC+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^DC[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^DC[.]", "", rc$direccion)
rc$direccion <- gsub("^DR+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^DR[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^DR[.]", "", rc$direccion)
rc$direccion <- gsub("^DTOR+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^DTOR[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^DTOR[.]", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.

rc$direccion <- gsub("^DON+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^D[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^D[.]", "", rc$direccion)
rc$direccion <- gsub("^D[*]", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^MOSSEN+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.

rc$direccion <- gsub("^OBISPO+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.

rc$direccion <- gsub("^PIN+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PIN[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^PIN[.]", "", rc$direccion)
rc$direccion <- gsub("^PINTOR+[[:space:]]", "", rc$direccion)
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.



rc$direccion <- gsub("^RAD+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^RAD[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^RAD[.]", "", rc$direccion)
rc$direccion <- gsub("^RADIOFONISTA", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
mi_gsub(from, to, rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.



#We remove again the Don and its friends. Let see if this is neccessary.
rc$direccion <- gsub("^DON+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^D[.]+[[:space:]]", "", rc$direccion)
rc$direccion <- gsub("^D[.]", "", rc$direccion)
rc$direccion <- gsub("^D[*]", "", rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
rc$direccion <- gsub("^LA+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^LO+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^EL+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^LAS+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^LOS+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^O+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^OS+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^A+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^AS+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^ELS+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^LES+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


rc$direccion <- gsub("D[']", "", rc$direccion) 
rc$direccion <- gsub("^IND[.]", "", rc$direccion)  
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
rc$direccion <- gsub("^PROF[.]", "", rc$direccion)  


rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.

#============================================================================================================
rc$direccion <- gsub("[[:space:]]+\\<DELS\\>+[[:space:]]"," DE LOS ",rc$direccion) 
rc$direccion <- gsub("[[:space:]]+\\<DEL\\>+[[:space:]]"," ",rc$direccion)   
rc$direccion <- gsub("[[:space:]]+\\<DE\\>+[[:space:]]"," ",rc$direccion)  


#We remove content in parenthesis at the end of direccion.
rc$direccion <- gsub("[()]", "", rc$direccion)
#============================================================================================================

rc$direccion <- gsub("[[:space:]]+\\<PLANTA\\>+[[:space:]]+[A-Z]+$","",rc$direccion)
rc$direccion <- gsub("[[:space:]]+$", "", rc$direccion)  #=============== We remove the spaces on the right.

#Numbers to text.
rc$direccion <- gsub("\\<DOCE\\>", "12",rc$direccion)
rc$direccion <- gsub("\\<DIECIOCHO\\>", "18",rc$direccion)
rc$direccion <- gsub("^\\<TRECE\\>", "13",rc$direccion)

rc$direccion <- gsub(", "," ",rc$direccion)
rc$direccion <- gsub(","," ", rc$direccion)


#Numbers
#We are going to remove the number in the address.
# rc$direccion <- gsub("[[:space:]]+BAJO+$","",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+BJ+$","",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+[0-9]+$","",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+[0-9]+\\-+[A-Z]+$","",rc$direccion) 
# rc$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+[Y]$","",rc$direccion) #Numbers with preposition "Y" in the middle.
# rc$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\<PTA\\>$","",rc$direccion) #Numbers with "pta" in the middle.

#To number with hyphen
# rc$direccion <- gsub("[[:space:]]+[0-9]+\\-+[0-9]+\\-+[0-9]+$","",rc$direccion) #For addresses with three numbers.
# rc$direccion <- gsub("[[:space:]]+[0-9]+\\-+[0-9]+$","",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+[0-9]+\\_+[0-9]+$","",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+[0-9]+\\,+[0-9]+\\,+[0-9]+$","",rc$direccion) #Numbers with comma between then.
# rc$direccion <- gsub("[[:space:]]+[0-9]+\\,+[0-9]+$","",rc$direccion) #Numbers with comma between then.
# rc$direccion <- gsub("[[:space:]]+[0-9]+\\,+[[:space:]]+[0-9]+\\,+[[:space:]]+[0-9]+$","",rc$direccion) #Numbers with comma and spaces between then.
# rc$direccion <- gsub("[[:space:]]+[0-9]+\\-+$","",rc$direccion) 
# rc$direccion <- gsub("[[:space:]]+[0-9]+\\_+$","",rc$direccion) 
#There are numbers plus hyphen plus space plus number.
# rc$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+[0-9]+$","",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+$","",rc$direccion)

#rc$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+$", "",rc$direccion)



##===========================================================================================================================   
# rc$direccion <- gsub("[0-9]+\\_+[0-9]+$","",rc$direccion) 
# rc$direccion <- gsub("BAJO+$","",rc$direccion)
# rc$direccion <- gsub("BJ+$","",rc$direccion)
# # rc$direccion <- gsub("[0-9]+$","",rc$direccion) #PONGO ESTO MÁS ABAJO.
# rc$direccion <- gsub("[0-9]+\\-+[A-Z]+$","",rc$direccion) 
# rc$direccion <- gsub("[0-9]+[[:space:]]+\\-+[[:space:]]+[0-9]+[[:space:]]+[A-Z]+$","",rc$direccion)
# # rc$direccion <- gsub("^?![0-9]+[[:space:]]+[A-Z]+$","",rc$direccion)
# rc$direccion <- gsub("[0-9]+[[:space:]]+[A-Z]+$","",rc$direccion)
# rc$direccion <- gsub("[0-9]+[[:space:]]+[Y]$","",rc$direccion) #Numbers with preposition "Y" in the middle.
# rc$direccion <- gsub("[0-9]+[[:space:]]+\\<PTA\\>$","",rc$direccion) #Numbers with "pta" in the middle.
# rc$direccion <- gsub("[0-9]+[A-Z]+\\-+[0-9]+[A-Z]+$","",rc$direccion) #add THIS=============================================================%%%%%%%%%%%%%%%%%%%%%%%%%%%
# rc$direccion <- gsub("[0-9]+[A-Z]+$","",rc$direccion) 

#To number with hyphen
# rc$direccion <- gsub("[0-9]+\\-+[0-9]+\\-+[0-9]+$","",rc$direccion) #For addresses with three numbers.
# rc$direccion <- gsub("[0-9]+\\-+[0-9]+\\-$","",rc$direccion)
# rc$direccion <- gsub("[0-9]+\\-+[0-9]+$","",rc$direccion)
# rc$direccion <- gsub("[0-9]+\\,+[0-9]+$","",rc$direccion) #Numbers with comma between then.
# rc$direccion <- gsub("[0-9]+[/]+[0-9]+$","",rc$direccion) #Numbers with slash between then.
# rc$direccion <- gsub("[0-9]+\\-+$","",rc$direccion) 
# 
# #There are numbers plus hyphen plus space plus number.
# rc$direccion <- gsub("[0-9]+[[:space:]]+\\-+[0-9]+$","",rc$direccion)
# rc$direccion <- gsub("[0-9]+[[:space:]]+\\-+$","",rc$direccion)
# 
# rc$direccion <- gsub("[0-9]+\\-+[[:space:]]+[A-Z]+\\º$","",rc$direccion)
# rc$direccion <- gsub("[0-9]+\\-+[A-Z]+\\º$","",rc$direccion) 
# ##===========================================================================================================================
# 
# rc$direccion <- gsub("[0-9]+$","",rc$direccion)
# 
# 
# #There are numbers plus hyphen plus space plus number.
# #To remove comma between numbers.
# rc$direccion <- gsub("[0-9]+\\,+$","",rc$direccion) 
# rc$direccion <- gsub(" S[/]N$","",rc$direccion) 
# rc$direccion <- gsub("S[/]N$","",rc$direccion) 
# rc$direccion <- gsub("[[:space:]]+\\<SN\\>$","",rc$direccion)   
# 
# rc$direccion <- gsub(" S[-]N$","",rc$direccion)
# rc$direccion <- gsub("S[-]N$","",rc$direccion)
# rc$direccion <- gsub(" N[º]$","",rc$direccion) 
# 
# rc$direccion <- gsub("[[:space:]]+N[.]$","",rc$direccion) 
# rc$direccion <- gsub("N[.][0-9]$","",rc$direccion) #We already removed the comma.
# rc$direccion <- gsub("[[:space:]]+N[.][0-9]$","",rc$direccion) 
# rc$direccion <- gsub("[0-9]+$","",rc$direccion)
#We remove the comma at the end.
# rc$direccion <- gsub(", ","",rc$direccion)
# rc$direccion <- gsub(","," ",rc$direccion)

# rc$direccion <- gsub("[0-9]+\\-+[[:space:]]+[A-Z]+\\º$","",rc$direccion) #===============================================================BAJO============================
# rc$direccion <- gsub("[0-9]+\\-+[A-Z]+\\º$","",rc$direccion) 

#=========================================================================================================



rc$direccion <- gsub("[[:space:]]+\\<XENDA\\>+$","",rc$direccion)
rc$direccion <- gsub("\\<COMPANY\\>","COMPANYS",rc$direccion)
# rc$direccion <- gsub("ALEXANDRE ROSELLO","ALEXANDRE ROSSELLO",rc$direccion)
rc$direccion <- gsub("\\<ROSELLO\\>","ROSSELLO",rc$direccion)

#We remove the prepositions "de/del" in Iri/Direccion1.
rc$direccion <- gsub("[[:space:]]+\\<DELS\\>+[[:space:]]"," DE LOS ",rc$direccion)
rc$direccion <- gsub("[[:space:]]+\\<DEL\\>+[[:space:]]"," ",rc$direccion)
rc$direccion <- gsub("[[:space:]]+\\<DE\\>+[[:space:]]"," ",rc$direccion)  




#==========================================================================================================
rc$direccion <- gsub("\\<ANTON\\>[.]+[[:space:]]", "ANTONIO ",rc$direccion)
rc$direccion <- gsub("\\<ANTON\\>[.]", "ANTONIO ",rc$direccion)

rc$direccion <- gsub("ARQUI[.]+[[:space:]]","ARQUITECTO ",rc$direccion)
rc$direccion <- gsub("\\<ARQUI\\>[.]","ARQUITECTO ",rc$direccion) 
rc$direccion <- gsub("AUZOA$","",rc$direccion)

rc$direccion <- gsub("^CTE[.]+[[:space:]]", "COMANDANTE ", rc$direccion)
rc$direccion <- gsub("^CTE[.]", "COMANDANTE", rc$direccion)
rc$direccion <- gsub("^CTE+[[:space:]]", "COMANDANTE ", rc$direccion)

rc$direccion <- gsub("\\CONJTO\\>[.]+[[:space:]]", "CONJUNTO ", rc$direccion)
rc$direccion <- gsub("\\CONJTO\\>[.]", "CONJUNTO ", rc$direccion)

# rc$direccion <- gsub("\\DGUEZ\\>[.]+[[:space:]]", "DOMINGUEZ ", rc$direccion)
# rc$direccion <- gsub("\\DGUEZ\\>[.]", "DOMINGUEZ ", rc$direccion)
# rc$direccion <- gsub("\\DGUEZ\\>+[[:space:]]", "DOMINGUEZ ", rc$direccion)

rc$direccion <- gsub("\\<EDIF\\>[.]+[[:space:]]", "EDIFICIO ",rc$direccion)
rc$direccion <- gsub("\\<EDIF\\>[.]", "EDIFICIO ",rc$direccion)
rc$direccion <- gsub("\\<EDI\\>[.]+[[:space:]]", "EDIFICIO ",rc$direccion)
rc$direccion <- gsub("\\<EDI\\>[.]", "EDIFICIO ",rc$direccion)
rc$direccion <- gsub("\\<EDF\\>[.]+[[:space:]]", "EDIFICIO ",rc$direccion)
rc$direccion <- gsub("\\<EDF\\>[.]", "EDIFICIO ",rc$direccion)
rc$direccion <- gsub("\\<ED\\>[.]+[[:space:]]", "EDIFICIO ",rc$direccion)
rc$direccion <- gsub("\\<ED\\>[.]", "EDIFICIO ",rc$direccion)

rc$direccion <- gsub("\\<ESCRIT\\>[.]+[[:space:]]", "ESCRITOR ",rc$direccion)
rc$direccion <- gsub("\\<ESCRIT\\>[.]", "ESCRITOR ",rc$direccion)
rc$direccion <- gsub("\\<ESCRIT\\>[.]+[[:space:]]", "ESCRITOR ",rc$direccion)

rc$direccion <- gsub("FCO+[[:space:]]","FRANCISCO ",rc$direccion)
rc$direccion <- gsub("FCO[.]+[[:space:]]","FRANCISCO ",rc$direccion)
rc$direccion <- gsub("FCO[.]","FRANCISCO ",rc$direccion)
rc$direccion <- gsub("\\<FDEZ\\>+[[:space:]]", "FERNANDEZ ",rc$direccion)
rc$direccion <- gsub("\\<FDEZ\\>[.]+[[:space:]]", "FERNANDEZ ",rc$direccion)
rc$direccion <- gsub("\\<FDEZ\\>[.]", "FERNANDEZ ",rc$direccion)
rc$direccion <- gsub("\\<FDEZ\\>$", "FERNANDEZ",rc$direccion)
rc$direccion <- gsub("\\<FED\\>+[.]", "FEDERICO ",rc$direccion)
rc$direccion <- gsub("FELIP+[[:space:]]","FELIPE ",rc$direccion)

rc$direccion <- gsub("\\<GCIA\\>+[[:space:]]", "GARCIA ",rc$direccion)
rc$direccion <- gsub("\\<GCIA\\>[.]+[[:space:]]", "GARCIA ",rc$direccion)
rc$direccion <- gsub("\\<GCIA\\>[.]", "GARCIA ",rc$direccion)

rc$direccion <- gsub("\\<GLEZ\\>+[[:space:]]", "GONZALEZ ",rc$direccion)
rc$direccion <- gsub("\\<GLEZ\\>[.]+[[:space:]]", "GONZALEZ ",rc$direccion)
rc$direccion <- gsub("\\<GLEZ\\>[.]", "GONZALEZ ",rc$direccion)

rc$direccion <- gsub("GRAL[.]+[[:space:]]","GENERAL ",rc$direccion)
rc$direccion <- gsub("GRAL[.]","GENERAL ",rc$direccion)
rc$direccion <- gsub("GRA[.]+[[:space:]]","GENERAL ",rc$direccion)
rc$direccion <- gsub("GRA[.]","GENERAL ",rc$direccion)

rc$direccion <- gsub("\\<HDEZ\\>+[[:space:]]", "HERNANDEZ ",rc$direccion)
rc$direccion <- gsub("\\<HDEZ\\>[.]+[[:space:]]", "HERNANDEZ ",rc$direccion)
rc$direccion <- gsub("\\<HDEZ\\>[.]", "HERNANDEZ ",rc$direccion)

rc$direccion <- gsub("HISTOR[.]+[[:space:]]","HISTORIADOR ",rc$direccion) 
rc$direccion <- gsub("HISTOR[.]","HISTORIADOR ",rc$direccion) 
rc$direccion <- gsub("HIST[.]+[[:space:]]","HISTORIADOR ",rc$direccion) 
rc$direccion <- gsub("HIST[.]","HISTORIADOR ",rc$direccion) 

rc$direccion <- gsub("HNOS[.]+[[:space:]]","HERMANOS ",rc$direccion)
rc$direccion <- gsub("HNOS[.]","HERMANOS ",rc$direccion)
rc$direccion <- gsub("HNOS+[[:space:]]","HERMANOS ",rc$direccion)

rc$direccion <- gsub("ING[.]", "INGENIERO ",rc$direccion)
rc$direccion <- gsub("INGEN[.]", "INGENIERO ",rc$direccion)

rc$direccion <- gsub("JOAQ[.]", "JOAQUIN ",rc$direccion)

rc$direccion <- gsub("JURIZMENDI","JUDIZMENDI",rc$direccion)

rc$direccion <- gsub("KALEA$","",rc$direccion) 
rc$direccion <- gsub("\\<KALE\\>","",rc$direccion) 

rc$direccion <- gsub("M[ª]", "MARIA",rc$direccion)
rc$direccion <- gsub("M[*]", "MARIA",rc$direccion)
rc$direccion <- gsub("\\<M\\>[.]", "MARIA ",rc$direccion)
rc$direccion <- gsub("^M+[[:space:]]", "MARIA ",rc$direccion)   
rc$direccion <- gsub("[[:space:]]+M+[[:space:]]", "MARIA ",rc$direccion) 

rc$direccion <- gsub("\\<MARAGALL\\>", "MARGALL",rc$direccion)
rc$direccion <- gsub("\\<MTNEZ\\>+[[:space:]]", "MARTINEZ ",rc$direccion)
rc$direccion <- gsub("\\<MTNEZ\\>[.]+[[:space:]]", "MARTINEZ ",rc$direccion)
rc$direccion <- gsub("\\<MTNEZ\\>[.]", "MARTINEZ ",rc$direccion)
rc$direccion <- gsub("\\<MNEZ\\>+[[:space:]]", "MARTINEZ ",rc$direccion)
rc$direccion <- gsub("\\<MNEZ\\>[.]+[[:space:]]", "MARTINEZ ",rc$direccion)
rc$direccion <- gsub("\\<MNEZ\\>[.]", "MARTINEZ ",rc$direccion)
rc$direccion <- gsub("\\<MONTSE\\>", "MONTSERRAT",rc$direccion)

rc$direccion <- gsub("\\<N\\>[.]+[[:space:]]+\\<SRA\\>[.]+[[:space:]]", "NUESTRA SEÑORA ",rc$direccion)
rc$direccion <- gsub("\\<N\\>[.]+[[:space:]]+\\<SRA\\>[.]", "NUESTRA SEÑORA ",rc$direccion)
rc$direccion <- gsub("\\<N\\>[.]+\\<SRA\\>[.]+[[:space:]]", "NUESTRA SEÑORA ",rc$direccion)
rc$direccion <- gsub("\\<N\\>[.]+\\<SRA\\>[.]", "NUESTRA SEÑORA ",rc$direccion)
rc$direccion <- gsub("\\<NTRA\\>[.]+[[:space:]]", "NUESTRA ",rc$direccion)
rc$direccion <- gsub("\\<NTRA\\>[.]", "NUESTRA ",rc$direccion)
rc$direccion <- gsub("\\<NTRA\\>+[[:space:]]", "NUESTRA ",rc$direccion)

rc$direccion <- gsub("\\<NTRO\\>[.]+[[:space:]]", "NUESTRO ",rc$direccion)
rc$direccion <- gsub("\\<NTRO\\>[.]", "NUESTRO ",rc$direccion)
rc$direccion <- gsub("\\<NTRO\\>+[[:space:]]", "NUESTRO ",rc$direccion)



rc$direccion <- gsub("\\<JOSE ORTEGA Y GASSET\\>", "ORTEGA Y GASSET",rc$direccion)
rc$direccion <- gsub("\\<JOSE ORTEGA GASSET\\>", "ORTEGA Y GASSET",rc$direccion)
rc$direccion <- gsub("\\<ORTEGA GASSET\\>", "ORTEGA Y GASSET",rc$direccion)

rc$direccion <- gsub("\\<PERIOD\\>+[[:space:]]", "PERIODISTA ",rc$direccion)
rc$direccion <- gsub("\\<PERIOD\\>[.]", "PERIODISTA ",rc$direccion)
rc$direccion <- gsub("\\<PINTOR JOAQUIN SOROLLA\\>", "SOROLLA",rc$direccion)
rc$direccion <- gsub("\\<PINTOR SOROLLA\\>", "SOROLLA",rc$direccion)
rc$direccion <- gsub("\\<JOAQUIN SOROLLA\\>", "SOROLLA",rc$direccion)

rc$direccion <- gsub("\\<REI JAUME I\\>", "JAIME I",rc$direccion)
rc$direccion <- gsub("\\<REY JAIME I\\>", "JAIME I",rc$direccion)
rc$direccion <- gsub("\\<REY JUAN CARLOS I\\>", "JUAN CARLOS I",rc$direccion)
rc$direccion <- gsub("\\<RDGUEZ\\>+[[:space:]]", "RODRIGUEZ ",rc$direccion)
rc$direccion <- gsub("\\<RDGUEZ\\>[.]+[[:space:]]", "RODRIGUEZ ",rc$direccion)
rc$direccion <- gsub("\\<RDGUEZ\\>[.]", "RODRIGUEZ ",rc$direccion)
rc$direccion <- gsub("\\<RGUEZ\\>[.]+[[:space:]]","RODRIGUEZ ",rc$direccion)
rc$direccion <- gsub("\\<RGUEZ\\>[.]","RODRIGUEZ ",rc$direccion)
rc$direccion <- gsub("\\<RDEZ\\>+[[:space:]]", "RODRIGUEZ ",rc$direccion)
rc$direccion <- gsub("\\<RDEZ\\>[.]+[[:space:]]", "RODRIGUEZ ",rc$direccion)
rc$direccion <- gsub("\\<RDEZ\\>[.]", "RODRIGUEZ ",rc$direccion)

rc$direccion <- gsub("\\<SRA\\>[.]+[[:space:]]", "SEÑORA ",rc$direccion)
rc$direccion <- gsub("\\<SRA\\>[.]", "SEÑORA ",rc$direccion)
rc$direccion <- gsub("\\<SRA\\>+[[:space:]]", "SEÑORA ",rc$direccion)
rc$direccion <- gsub("SINDICA[.]+[[:space:]]", "SINDICALISTA ",rc$direccion)
rc$direccion <- gsub("SINDICA+[[:space:]]", "SINDICALISTA ",rc$direccion)
rc$direccion <- gsub("\\<SINDICA\\>[.]", "SINDICALISTA ",rc$direccion)

rc$direccion <- gsub("STOS+[[:space:]]", "SANTOS ",rc$direccion)
rc$direccion <- gsub("STOS[.]+[[:space:]]", "SANTOS ",rc$direccion)
rc$direccion <- gsub("STOS[.]", "SANTOS ",rc$direccion)
rc$direccion <- gsub("\\<STOS\\>", "SANTOS",rc$direccion)
rc$direccion <- gsub("\\<STO\\>[.]+[[:space:]]", "SANTO ",rc$direccion)   
rc$direccion <- gsub("\\<STO\\>[.]", "SANTO ",rc$direccion)   
rc$direccion <- gsub("\\<STO\\>+[[:space:]]", "SANTO ",rc$direccion)
rc$direccion <- gsub("\\<STA\\>[.]+[[:space:]]", "SANTA ",rc$direccion)   
rc$direccion <- gsub("\\<STA\\>[.]", "SANTA ",rc$direccion)   
rc$direccion <- gsub("\\<STA\\>+[[:space:]]", "SANTA ",rc$direccion)
rc$direccion <- gsub("\\<ST\\>+[[:space:]]", "SANTO ",rc$direccion)
rc$direccion <- gsub("\\<ST\\>[.]+[[:space:]]", "SANTO ",rc$direccion)
rc$direccion <- gsub("\\<ST\\>[.]", "SANTO ",rc$direccion)
rc$direccion <- gsub("\\<S\\>[.]+[[:space:]]", "SAN ",rc$direccion)
rc$direccion <- gsub("\\<S\\>[.]", "SAN ",rc$direccion)
rc$direccion <- gsub("\\<S\\>+[[:space:]]", "SAN ",rc$direccion)



#======================================================================================================
# rc$direccion <- gsub("KM+[[:space:]]+[0-9]+$", "",rc$direccion)
# rc$direccion <- gsub("KM-[0-9]+$", "",rc$direccion)
# rc$direccion <- gsub("\\<KM\\>$", "",rc$direccion)
# rc$direccion <- gsub("\\<LOC\\>[.]", "",rc$direccion)
# rc$direccion <- gsub("\\<L\\>[.]", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+[0-9]+\\-+\\<LOCAL\\>$", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+[0-9]+\\-+[[:space:]]+\\<LOCAL\\>$", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+[[:space:]]+\\<LOCAL\\>$", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+[0-9]+\\-+\\<LOCALES\\>$", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+[0-9]+\\-+[[:space:]]+\\<LOCALES\\>$", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+[0-9]+[[:space:]]+\\-+[[:space:]]+\\<LOCALES\\>$", "",rc$direccion)
# rc$direccion <- gsub("\\<LOCAL\\>$", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+\\<B\\>$", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+\\<BLOQUE\\>$", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+\\<BLQ\\>$", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+\\<BLQ\\>[.]$", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+\\<BL\\>[-]$", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+\\<P\\>[.]$", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+\\<P\\>[-]$", "",rc$direccion)

rc$direccion <- gsub("[[:space:]]+\\<DELS\\>+[[:space:]]"," DE LOS ",rc$direccion) 
rc$direccion <- gsub("[[:space:]]+\\<DEL\\>+[[:space:]]"," ",rc$direccion)   
rc$direccion <- gsub("[[:space:]]+\\<DE\\>+[[:space:]]"," ",rc$direccion) 



#Translates
rc$direccion <- gsub("\\<ABATXOLO\\>", "ABACHOLO",rc$direccion)
rc$direccion <- gsub("\\<AGUSTI\\>", "AGUSTIN",rc$direccion)
rc$direccion <- gsub("\\<AJUNTAMENT\\>","AYUNTAMIENTO",rc$direccion)
rc$direccion <- gsub("\\<ALACANT\\>", "ALICANTE",rc$direccion)
rc$direccion <- gsub("\\<ALBERT\\>", "ALBERTO",rc$direccion)
rc$direccion <- gsub("\\<ALFONS\\>", "ALFONSO",rc$direccion)
rc$direccion <- gsub("\\<ALFRED\\>", "ALFREDO",rc$direccion)
rc$direccion <- gsub("\\<ALINYO\\>", "ALIÑO",rc$direccion)
rc$direccion <- gsub("\\<ALMOGAVERS\\>", "ALMOGAVERES",rc$direccion)
rc$direccion <- gsub("\\<ALPS\\>", "ALPES",rc$direccion)
rc$direccion <- gsub("\\<ANNA\\>", "ANA",rc$direccion)
rc$direccion <- gsub("\\<ANDALUSIA\\>", "ANDALUCIA",rc$direccion)
rc$direccion <- gsub("\\<ANDREU\\>", "ANDRES",rc$direccion)
rc$direccion <- gsub("\\<ANGELS\\>", "ANGELES",rc$direccion)
rc$direccion <- gsub("\\<ANSELM\\>", "ANSELMO",rc$direccion)
rc$direccion <- gsub("\\<ANTIC\\>", "ANTIGUO",rc$direccion)
rc$direccion <- gsub("\\<ANTIGA\\>", "ANTIGUA",rc$direccion)
rc$direccion <- gsub("\\<ANTONI\\>", "ANTONIO",rc$direccion)
rc$direccion <- gsub("\\<ARAGO\\>", "ARAGON",rc$direccion)
rc$direccion <- gsub("\\<ARCADI\\>", "ARCADIO",rc$direccion)
rc$direccion <- gsub("\\<ARQUEBISBE\\>", "ARZOBISPO",rc$direccion)
rc$direccion <- gsub("\\<ARMADES\\>", "ARMADAS",rc$direccion)
rc$direccion <- gsub("\\<ARTESANS\\>", "ARTESANOS",rc$direccion)

rc$direccion <- gsub("\\<BAIX\\>", "BAJO",rc$direccion)
rc$direccion <- gsub("\\<BASKONIA\\>", "VASCONIA",rc$direccion)
rc$direccion <- gsub("\\<BENVINGUTS\\>", "BIENVENIDOS",rc$direccion)
rc$direccion <- gsub("\\<BENVINGUT\\>", "BIENVENIDO",rc$direccion)
rc$direccion <- gsub("\\<BISBE\\>", "OBISPO",rc$direccion)
rc$direccion <- gsub("\\<BISCAIA\\>", "VIZCAYA",rc$direccion)
rc$direccion <- gsub("\\<BOTICARI\\>", "BOTICARIO",rc$direccion)

rc$direccion <- gsub("\\<CAMINAS\\>", "CAMINOS",rc$direccion)
rc$direccion <- gsub("\\<CAMIL\\>", "CAMILO",rc$direccion)
rc$direccion <- gsub("\\<CAMP\\>", "CAMPO",rc$direccion)
rc$direccion  <- gsub("\\<CANALETES\\>", "CANALETAS",rc$direccion)
rc$direccion <- gsub("\\<CAPITA\\>", "CAPITAN",rc$direccion)
rc$direccion <- gsub("\\<CAPUTXINS\\>", "CAPUCHINOS",rc$direccion)
rc$direccion <- gsub("\\<CARME\\>", "CARMEN",rc$direccion)
rc$direccion <- gsub("\\<CARLES\\>", "CARLOS",rc$direccion)
rc$direccion <- gsub("\\<CASTELA\\>", "CASTILLA",rc$direccion)
rc$direccion <- gsub("\\<CASTELLO\\>", "CASTELLON",rc$direccion)
rc$direccion <- gsub("\\<CATALUNYA\\>", "CATALUÑA",rc$direccion)
rc$direccion <- gsub("\\<CATOLICS\\>", "CATOLICOS",rc$direccion)
rc$direccion <- gsub("\\<CIMADEVILA\\>", "CIMADEVILLA",rc$direccion)
rc$direccion <- gsub("\\<CIRCUMVALACIO\\>", "CIRCUNVALACION",rc$direccion)
rc$direccion <- gsub("\\<CIUTAT\\>", "CIUDAD",rc$direccion)
rc$direccion <- gsub("\\<CIUTADANS\\>", "CIUDADANOS",rc$direccion)
rc$direccion <- gsub("\\<COLOM\\>", "COLON",rc$direccion)
rc$direccion <- gsub("\\<COMERÇ\\>", "COMERCIO",rc$direccion)
rc$direccion <- gsub("\\<COMTE\\>", "CONDE",rc$direccion)
rc$direccion <- gsub("\\<CONCA\\>", "CUENCA",rc$direccion)
rc$direccion <- gsub("\\<CONCELLO\\>", "CONCEJO",rc$direccion)
rc$direccion <- gsub("\\<CONCILI\\>", "CONCILIO",rc$direccion)
rc$direccion <- gsub("\\<CONSTITUCIO\\>", "CONSTITUCION",rc$direccion)
rc$direccion <- gsub("\\<COR\\>", "CORAZON",rc$direccion)
rc$direccion <- gsub("\\<CORNELI\\>", "CORNELIO",rc$direccion)
rc$direccion <- gsub("\\<CORTS\\>", "CORTES",rc$direccion)
rc$direccion <- gsub("\\<CREU\\>", "CRUZ",rc$direccion)
rc$direccion <- gsub("\\<CRIST\\>", "CRISTO",rc$direccion)
rc$direccion <- gsub("\\<CRISTOFOR\\>", "CRISTOBAL",rc$direccion)
rc$direccion <- gsub("\\<CRISTOVAL\\>", "CRISTOBAL",rc$direccion)

rc$direccion <- gsub("\\<DESEMPARATS\\>", "DESAMPARADOS",rc$direccion)
rc$direccion <- gsub("\\<DEU\\>+[[:space:]]", "DIOS ",rc$direccion)
rc$direccion <- gsub("\\<DOUTOR\\>", "DOCTOR",rc$direccion)
rc$direccion <- gsub("\\<DIPUTACIO\\>", "DIPUTACION",rc$direccion)
rc$direccion <- gsub("\\<D'ELX\\>", "DE ELCHE",rc$direccion)
rc$direccion <- gsub("\\<ELX\\>", "ELCHE",rc$direccion)

rc$direccion <- gsub("\\<EDUARD\\>", "EDUARDO",rc$direccion)
rc$direccion <- gsub("\\<EGLESIA\\>", "IGLESIA",rc$direccion)
rc$direccion <- gsub("\\<ENGINYER\\>", "INGENIERO",rc$direccion)
rc$direccion <- gsub("\\<ENRIC\\>", "ENRIQUE",rc$direccion)
rc$direccion <- gsub("\\<L'ESGLESIA\\>", "LA IGLESIA",rc$direccion)
rc$direccion <- gsub("\\<ESGLESIA\\>", "IGLESIA",rc$direccion)
rc$direccion <- gsub("\\<ESPANYA\\>", "ESPAÑA",rc$direccion)
rc$direccion <- gsub("\\<ESPIRITO\\>", "ESPIRITU",rc$direccion)
rc$direccion <- gsub("\\<L'ESTACIO\\>", "LA ESTACION",rc$direccion)
rc$direccion <- gsub("\\<ESTACIO\\>", "ESTACION",rc$direccion)
rc$direccion <- gsub("\\<ESTUDIS\\>", "ESTUDIOS",rc$direccion)
rc$direccion <- gsub("\\<EXERCITO\\>", "EJERCITO",rc$direccion)
rc$direccion <- gsub("\\<EXERCIT\\>", "EJERCITO",rc$direccion)

rc$direccion <- gsub("\\<FLORS\\>", "FLORES",rc$direccion)
rc$direccion <- gsub("\\<FORCES\\>", "FUERZAS",rc$direccion)
rc$direccion <- gsub("\\<FRA\\>", "FRAY",rc$direccion)
rc$direccion <- gsub("\\<FREI\\>", "FRAY",rc$direccion)
rc$direccion <- gsub("\\<FRANCESC\\>", "FRANCISCO",rc$direccion)

rc$direccion <- gsub("\\<GAUDENCI\\>", "GAUDENCIO",rc$direccion)
rc$direccion <- gsub("\\<GERVASI\\>", "GERVASIO",rc$direccion)
rc$direccion <- gsub("\\<GIRONA\\>", "GERONA",rc$direccion)
rc$direccion <- gsub("\\<GUIPUZKOA\\>", "GUIPUZCOA",rc$direccion)

rc$direccion <- gsub("\\<HOMENS\\>", "HOMBRES",rc$direccion)

rc$direccion <- gsub("[[:space:]]+\\<I\\>+[[:space:]]", " Y ",rc$direccion) #conjunción copulativa: Pi y Margall, etc.
rc$direccion <- gsub("\\<IGNASI\\>","IGNACIO",rc$direccion)
rc$direccion <- gsub("\\<ILLAS\\>","ISLAS",rc$direccion)
rc$direccion <- gsub("\\<INDUSTRI\\>","INDUSTRIA",rc$direccion)
rc$direccion <- gsub("\\<ISIDRE\\>","ISIDRO",rc$direccion)

rc$direccion <- gsub("\\<JACINT\\>","JACINTO",rc$direccion)
rc$direccion <- gsub("\\<JAUME\\>","JAIME",rc$direccion)
rc$direccion <- gsub("\\<JOAN\\>", "JUAN",rc$direccion)
rc$direccion <- gsub("\\<JOAQUIM\\>", "JOAQUIN ",rc$direccion)
rc$direccion <- gsub("\\<JORDI\\>", "JORGE",rc$direccion)
rc$direccion <- gsub("\\<JULIOL\\>", "JULIO",rc$direccion)

rc$direccion <- gsub("\\<L'ESCORIAL\\>", "EL ESCORIAL",rc$direccion)

rc$direccion <- gsub("\\<LLEIDA\\>", "LERIDA",rc$direccion)
rc$direccion <- gsub("\\<LIBERDADE\\>", "LIBERTAD",rc$direccion)
rc$direccion <- gsub("\\<LLIBERTAD\\>", "LIBERTAD",rc$direccion)
rc$direccion <- gsub("\\<LLIBERTAT\\>", "LIBERTAD",rc$direccion)
rc$direccion <- gsub("\\<LLUCIA\\>", "LUCIA",rc$direccion)
rc$direccion <- gsub("\\<LLUIS\\>", "LUIS",rc$direccion)

rc$direccion <- gsub("\\<MACIA\\>", "MACIAS",rc$direccion)
rc$direccion <- gsub("\\<MAIG\\>", "MAYO",rc$direccion)
rc$direccion <- gsub("\\<MAGISTRAT\\>", "MAGISTRADO",rc$direccion)
rc$direccion <- gsub("\\<MAIOR\\>", "MAYOR",rc$direccion)
rc$direccion <- gsub("\\<MAJOR\\>", "MAYOR",rc$direccion)
rc$direccion <- gsub("\\<MARE\\>+[[:space:]]", "MADRE ",rc$direccion)
rc$direccion <- gsub("\\<MARITIM\\>", "MARITIMO",rc$direccion) #We replace exactly "maritim" not words which contain "maritim".
rc$direccion <- gsub("\\<MARIÑA\\>", "MARINA",rc$direccion)
rc$direccion <- gsub("\\<MERCAT\\>", "MERCADO",rc$direccion)
rc$direccion <- gsub("\\<MESTRE\\>", "MAESTRO",rc$direccion)

rc$direccion <- gsub("\\<NAPOLS\\>", "NAPOLES",rc$direccion)
rc$direccion <- gsub("\\<NATURAIS\\>", "NATURALES",rc$direccion)
rc$direccion <- gsub("\\<NICOLAU\\>", "NICOLAS",rc$direccion)
rc$direccion <- gsub("\\<NOVA\\>", "NUEVA",rc$direccion)

rc$direccion <- gsub("\\<ONZE\\>", "ONCE",rc$direccion)
rc$direccion <- gsub("\\<ORTIGUEIRA\\>", "ORTIGUERA",rc$direccion)
rc$direccion <- gsub("\\<OURENSE\\>", "ORENSE",rc$direccion)
rc$direccion <- gsub("\\<O SABIO\\>", "EL SABIO",rc$direccion)

rc$direccion <- gsub("\\<PALMERES\\>", "PALMERAS",rc$direccion)
rc$direccion <- gsub("\\<PARAGUAI\\>", "PARAGUAY",rc$direccion)
rc$direccion <- gsub("\\<PARALEL\\>", "PARALELO",rc$direccion)
rc$direccion <- gsub("\\<PARE\\>", "PADRE",rc$direccion)
rc$direccion <- gsub("\\<PARLAMENT\\>", "PARLAMENTO",rc$direccion)
rc$direccion <- gsub("\\<PASQUAL\\>", "PASCUAL",rc$direccion)
rc$direccion <- gsub("\\<PASSEIG\\>", "PASEO",rc$direccion)
rc$direccion <- gsub("\\<PAU\\>", "PAZ",rc$direccion)
rc$direccion <- gsub("\\<PEDRAIO\\>", "PEDRAYO",rc$direccion)
rc$direccion <- gsub("\\<PEP\\>", "PEPE",rc$direccion)
rc$direccion <- gsub("\\<PERE\\>", "PEDRO",rc$direccion)
rc$direccion <- gsub("\\<PLATJA\\>", "PLAYA",rc$direccion)
rc$direccion <- gsub("\\<POBRESA\\>", "POBREZA",rc$direccion)
rc$direccion <- gsub("\\<PORT\\>", "PUERTO",rc$direccion)
rc$direccion <- gsub("\\<PRESIDENT\\>", "PRESIDENTE",rc$direccion)
rc$direccion <- gsub("\\<PRIMER\\>", "PRIMERO",rc$direccion)

rc$direccion <- gsub("\\<REGNE\\>", "REINO",rc$direccion)
rc$direccion <- gsub("\\<REIS\\>", "REYES",rc$direccion)
rc$direccion <- gsub("\\<RIU\\>", "RIO",rc$direccion)
rc$direccion <- gsub("\\<ROBERT\\>", "ROBERTO",rc$direccion)

rc$direccion <- gsub("\\<SAGRAT\\>", "SAGRADO",rc$direccion)
rc$direccion <- gsub("\\<SANT\\>", "SAN",rc$direccion)
rc$direccion <- gsub("\\<SANTISSIMA\\>", "SANTISIMA",rc$direccion)
rc$direccion <- gsub("\\<SANTISSIM\\>", "SANTISIMO",rc$direccion)
rc$direccion <- gsub("\\<SAVI\\>", "SABIO",rc$direccion)
rc$direccion <- gsub("\\<SEBASTIA\\>", "SEBASTIAN",rc$direccion)
rc$direccion <- gsub("\\<SEGON\\>", "SEGUNDO",rc$direccion)
rc$direccion <- gsub("\\<SENYORA\\>", "SEÑORA",rc$direccion)
rc$direccion <- gsub("\\<SEPTEMBRE\\>", "SEPTIEMBRE",rc$direccion)
rc$direccion <- gsub("\\<SETEMBRE\\>", "SEPTIEMBRE",rc$direccion)
rc$direccion <- gsub("\\<SETZE\\>", "SIETE",rc$direccion)

rc$direccion <- gsub("\\<TAQUIGRAF\\>", "TAQUIGRAFO",rc$direccion)
rc$direccion <- gsub("\\<TARRADELLES\\>", "TARRADELLAS",rc$direccion)
rc$direccion <- gsub("\\<TEIXEIRO\\>", "TEIJEIRO",rc$direccion)
rc$direccion <- gsub("\\<TEMPLERS\\>", "TEMPLARIOS",rc$direccion)
rc$direccion <- gsub("\\<TORNEIROS\\>", "TORNEROS",rc$direccion)
rc$direccion <- gsub("\\<TRINITAT\\>", "TRINIDAD",rc$direccion)
rc$direccion <- gsub("\\<TRIOMF\\>", "TRIUNFO",rc$direccion)

rc$direccion <- gsub("\\<UNIVERSITAT\\>", "UNIVERSIDAD",rc$direccion)

rc$direccion <- gsub("\\<VERGE\\>", "VIRGEN",rc$direccion)
rc$direccion <- gsub("\\<VICENS\\>", "VICENTE",rc$direccion)
rc$direccion <- gsub("\\<VICENT\\>", "VICENTE",rc$direccion)

rc$direccion <- gsub("\\<XAVIER\\>", "JAVIER",rc$direccion)
rc$direccion <- gsub("\\<XOSE\\>", "JOSE",rc$direccion)
rc$direccion <- gsub("\\<XUNQUEIRA\\>", "JUNQUERA",rc$direccion)



rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.


# rc$direccion <- gsub("\\-+$", "", rc$direccion)  #=============== We remove the hyphens on the right.
# rc$direccion <- gsub("[[:space:]]+$", "", rc$direccion)  #=============== We remove all the spaces on the right.
# rc$direccion <- gsub("\\-+$", "", rc$direccion)  #=============== We remove the hyphens on the right.


rc$direccion <- gsub("^\\<DELS\\>+[[:space:]]","",rc$direccion)  
rc$direccion <- gsub("^\\<DEL\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DE\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DOS\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DO\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^\\<DAS\\>+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^\\<DA\\>+[[:space:]]","",rc$direccion) 
rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.
rc$direccion <- gsub("^LA+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^LO+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^EL+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^LAS+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^LOS+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^O+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^OS+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^A+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^AS+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^ELS+[[:space:]]","",rc$direccion)
rc$direccion <- gsub("^LES+[[:space:]]","",rc$direccion)

rc$direccion <- gsub("^[[:space:]]", "", rc$direccion)  #=============== We remove the spaces on the left.



#NATIONAL ROADS
# rc$direccion <- gsub("\\<N\\>[.]+[[:space:]]", "NACIONAL ",rc$direccion)
# rc$direccion <- gsub("\\<N\\>[.]", "NACIONAL ",rc$direccion)
# rc$direccion <- gsub("\\<N\\>[-]+[[:space:]]", "NACIONAL ",rc$direccion)
# rc$direccion <- gsub("\\<N\\>[-]", "NACIONAL ",rc$direccion)
# rc$direccion <- gsub("\\<NACIONAL\\>[-]", "NACIONAL ",rc$direccion)
# rc$direccion <- gsub("KM[.]+[[:space:]]+[0-9]+$", "",rc$direccion)
# rc$direccion <- gsub("\\<KM\\>[.]", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+$", "", rc$direccion)  #=============== We remove the spaces on the right.

# 
# rc$direccion <- gsub("\\/+[[:space:]]+\\/","",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+$", "", rc$direccion)  #=============== We remove the spaces on the right.

#===============================================================================================PORTAL=======================================================
# rc$direccion <- gsub("[[:space:]]+\\<P\\>$", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+\\<PORTAL\\>[[:space:]]+[0-9]+$", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+$", "", rc$direccion)  #=============== We remove the spaces on the right.
# 
# 
# rc$direccion <- gsub("[[:space:]]+\\<SOLAR\\>+[[:space:]]+[A-Z|0-9]+$", "",rc$direccion)
# rc$direccion <- gsub("[[:space:]]+$", "", rc$direccion)  #=============== We remove the spaces on the right.



rc$ensena_poblacion_direccion <- paste(rc$ensena,"-",rc$poblacion,"-",rc$direccion)



repeated_rc <- which(duplicated(rc$ensena_poblacion_direccion))


#To test repeated elements.
repeated_rc_df <- rc[repeated_rc,]

# View(repeated_rc_df)

search_rep_cod_rc <- function(code) {
  tmp <- which(repeated_rc_df$cod == code)
  clave <- repeated_rc_df[tmp,"ensena_poblacion_direccion"]
  original <- which(rc$ensena_poblacion_direccion == clave$ensena_poblacion_direccion)
  original_code <- rc[original,"cod"]
  return(original_code)
}

result_rc <- sapply(repeated_rc_df$cod, FUN = search_rep_cod_rc)

#We convert the result_rc in a vector of codes.
result_rc_vector <- unlist(result_rc, use.names = FALSE)
#For comparissons
# write.csv(result_rc_vector, "result_rc_vector.csv")


#We obtains the indexes of repeated and original elements.
codes_repeated_and_originals_elements <- which(rc$cod  %in% result_rc_vector)


#And we sustratc these elements to the dataframe which we are going to search by ensena.
rc_not_repeated <- rc[-c(codes_repeated_and_originals_elements),]

# View(repeated_rc_df)
# 
# nrow(repeated_rc_df)



#Test
# View(rc_not_repeated[grepl("44347", rc_not_repeated$cod),])


# View(rc_not_repeated[grepl("ZU", rc_not_repeated$ensena),])

#To filter by addresses within a city.
# terranova <- rc_not_repeated[grepl("TERRANOVA", rc_not_repeated$ensena),]
# 
# # View(interzoo)
# 
# View(terranova[grepl("BARCELONA", terranova$poblacion),])












