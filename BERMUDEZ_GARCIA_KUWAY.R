library(magrittr)
library(dplyr)
library(modeest)
library(sjstats)
library(readxl) 
#---------------------------------------------------------------#
#Archivo de datos 1 (COLECCIONZOOLOGICAPARQUEDELASLEYENDAS.xlsx)
#---------------------------------------------------------------#
#a. Lean el archivo COLECCIONZOOLOGICAPARQUEDELASLEYENDAS.xlsx y almacénenlo como datos1.
datos1 = read_xlsx("COLECCIONZOOLOGICAPARQUEDELASLEYENDAS.xlsx")
head(datos1)
#b. Después de haber leído los datos, renombre las columnas
datos1 %>% 
  mutate(NUM = paste(`N°`)) %>% 
  mutate(NCOMUN = paste(`Nombre Común`)) %>%
  mutate(NCIENT = paste(`Nombre Científico`)) %>%
  mutate(GENERO = paste(Genero)) %>%
  mutate(FAMILIA = paste(Familia)) %>%
  mutate(CANTIDAD = paste(`Cantidad de Individuos.`)) %>% 
  mutate(CANTIDAD = as.numeric(CANTIDAD))%>%
  mutate(DISTRIBUCION = paste(`Distribución Natural`)) %>%
  select(NUM,NCOMUN,NCIENT,GENERO,FAMILIA,CANTIDAD,DISTRIBUCION) %>% 
  filter(CANTIDAD!=1286)-> datos1
View(datos1)
#d. ¿Cuál es o cuáles son los géneros más frecuentes?
head(datos1)
datos1 %>% 
  summarise(moda =mfv(GENERO)) 
# el genero mas frecuente es Ara, Columba


#e. Obtenga e interprete la mediana de la cantidad de individuos cuya distribución es doméstica.
datos1 %>% 
  filter(DISTRIBUCION =="Doméstico") %>% 
  summarise(mediana = median(CANTIDAD))
#Al menos la mitad de los individuos cuya distribucion es domestica es de 7 o menos .

#f. ¿Qué genero presenta mayor coeficiente de variabilidad de la cantidad de individuos: Ara o Columba?
datos1 %>% 
  filter(GENERO=="Ara") %>%
  summarise(CV1 = cv(CANTIDAD)*100)# cv=62.9


datos1 %>% 
  filter(GENERO=="Columba") %>% 
  summarise(CV2 = cv(CANTIDAD)*100)# cv=46.6
# El genero Ara presente mayor coeficiente de variabilidad de la cantidad de individuos respecto al genero Columba
#ADICIONAL#
datos1 %>% 
  filter(GENERO %in%c("Ara","Columba")) %>%
  group_by(GENERO) %>%          
  summarise(CV = cv(CANTIDAD)*100)

#g. ¿Cuál es la distribución natural más frecuente?

datos1 %>% 
  summarise(moda =mfv(DISTRIBUCION)) 
# La distribucion natural mas frecuente es Silvestre

# h. El Parque de Las Leyendas decide albergar a un ejemplar más de cada especie de la
# familia Felidae. Cree un nuevo dataframe denominado datos1b en el que aparezca
# una nueva columna denominada CANTIDAD.NUEVA para los ejemplares de esta
# familia. Luego de ello, extraiga e interprete la media aritmética de la nueva variable
# calculada.

datos1 %>% 
  filter(FAMILIA =="Felidae") %>% 
  mutate(CANTIDAD.NUEVA=CANTIDAD+1) %>% 
  summarise(MEDIA = mean(CANTIDAD.NUEVA))->datos1b
datos1b # media aritmetica(CANTIDAD.NUEVA)=3.92
# En promedio hay 4 ejemplares en la cantidad nueva, de la especie Felidae

#----------------------------------------------------------------#  
#2. Archivo de datos 2 (Sesiones del Concejo Metropolitano.csv)
#----------------------------------------------------------------#
#a. Lean el archivo Sesiones del Concejo Metropolitano.csv y almacénenlo como
# datos2.
datos2 =read.csv("Sesiones del Concejo Metropolitano.csv",encoding = "UTF-8")
View(datos2)
head(datos2)
#b. Después de haber leído los datos, renombre las columnas:
datos2 %>% 
  rename("TIPO.SESION"="Tipo.de.sesión","CORRELATIVO"="Nº.correlativo.de.las.sesiones","MES"="Mes","DIA"="Día","ACUERDOS"="Nº.de.Acuerdos.de.Concejo.aprobados",
         "ORDENANZAS"="Nº.de.Ordenanzas.aprobadas","MODALIDAD"="Presencial...Virtual") %>% 
  select(TIPO.SESION,CORRELATIVO,MES,DIA,ACUERDOS,ORDENANZAS,MODALIDAD) ->datos2
#c. ¿En qué tipo de sesión hubo un mayor número promedio de ordenanzas aprobadas?

datos2 %>% 
  filter(TIPO.SESION=="Extraordinaria") %>% 
  summarise(PROM1 =mean(ORDENANZAS))#promedio=0.14
datos2 %>% 
  filter(TIPO.SESION=="Ordinaria") %>% 
  summarise(PROM2 =mean(ORDENANZAS))#promedio=2.76
#El promedio de ordenanzas aprobadas del tipo de sesion Ordinaria es mayor al de sesion Extraordinaria

# d. ¿Cuál es la cantidad más frecuente de acuerdos aprobados en las sesiones
# extraordinarias?

datos2 %>% 
  filter(TIPO.SESION=="Extraordinaria") %>% 
  summarise(moda =mfv(ACUERDOS))#moda =1
#La cantidad  más frecuente de acuerdos aprobados en las sesiones Extraordinarias es 1.

# e. ¿Cuál es la modalidad más frecuente en la que se han llevado a cabo las sesiones
# ordinarias?
datos2 %>% 
  filter(TIPO.SESION=="Ordinaria") %>%
  summarise(moda =mfv(MODALIDAD))# moda= Virtual
#La modalidad mas frecuente en la que se han llevadoa cabo las sesiones ordinarias es Virtual 


# f. Obtengan e interpreten el percentil 80 del número de acuerdos de las sesiones
# ordinarias.

datos2 %>% 
  filter(TIPO.SESION=="Ordinaria") %>%
  summarise(P80 = quantile(ACUERDOS,0.80))#P80=13.4
#Al menos el 80% de las sesiones Ordinarias tiene un numero de acuerdo menor o igual a 13.4 

# g. Obtengan e interpreten el primer cuartil para la variable DIA.
datos2 %>% 
  summarise(Q1 = quantile(DIA,0.25))#Q1=11
#Al menos el 25% de las sesiones se realizaron el dia 11 o antes

#-----------------------------------#
# 3. Archivo de datos 3 (CAPES.csv)
#-----------------------------------#

# a. Lean el archivo CAPES.csv y almacénenlo como datos3

datos3 = read.csv("CAPES.csv")
View(datos3)

# b. Completen la afirmación:
#   Al menos el 15% de los beneficiarios (conocidos como bolsistas) recibió … reales o
# menos como apoyo económico
datos3 %>% 
  summarise(P15 = quantile(VL_BOLSA,0.25))
#Al menos el 15% de los beneficiarios (conocidos como bolsistas) recibió 765 reales o
# menos como apoyo económico 

# c. ¿Cuál es la desviación estándar del valor de la bolsa de aquellos que pagan el
# beneficio como BOLSISTA DE MESTRADO (beneficiario de maestría)?
datos3 %>% 
  filter(DS_MODALIDADE_PAGAMENTO =="BOLSISTA DE MESTRADO") %>% 
  summarise(DESV = sd(VL_BOLSA))# desviacion estandar=0
# La desviacion estandar de los que pagan como BOLSITA DE MESTRADO(beneficiario de maestria) es de 0

# d. Comparen el monto de beneficio entre estudiantes de género masculino y femenino.
datos3 %>% 
  filter(TP_GENERO =="M") %>% 
  summarise(mediana = median(VL_BOLSA))# mediana=765
datos3 %>% 
  filter(TP_GENERO =="F") %>% 
  summarise(mediana = median(VL_BOLSA))# mediana=765
#El monto de beneficio entre estudiantes de genero masculino y femenino son aproximandamente iguales y la cantidad es de 765 reales

# e. Calculen el rango del valor de la bolsa recibido por los estudiantes de la
# UNIVERSIDADE FEDERAL DO RIO DE JANEIRO en el mes de junio del 2013.
Rango = function(s){max(s)-min(s)}
datos3 %>% 
  filter(NM_ENTIDADE_ENSINO =="UNIVERSIDADE FEDERAL DO RIO DE JANEIRO") %>% 
  filter(AN_REFERENCIA =="2013") %>% 
  filter(ME_REFERENCIA =="6") %>% 
  summarise(RANGO =Rango(VL_BOLSA))
# El rango es de reales 735
  
# f. ¿Cuál es la modalidad de pago más común en la UNIVERSIDADE FEDERAL DO
# CEARA?
datos3 %>% 
  filter(NM_ENTIDADE_ENSINO =="UNIVERSIDADE FEDERAL DO CEARA") %>% 
  summarise(moda =mfv(DS_MODALIDADE_PAGAMENTO))#moda=TUTOR
#La modalidad de pago mas frecuente de la UNIVERSIDADDE FEDERAL DOCEARA es TUTOR






