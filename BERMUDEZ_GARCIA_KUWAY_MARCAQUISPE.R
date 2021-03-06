#Archivo de datos 1 (202007-PDE-REP-00247) 
datos1a = read.csv2("202007-PDE-REP-00247 - A.csv" )#a
datos1a$X<-NULL
head(datos1a)
View(datos1a)
datos1a[5,]#b
datos1b = read.csv2("202007-PDE-REP-00247 - B.csv",skip = 3)#C
head(datos1b)
datos1b[,2:3]#d
#PRIMER ERROR header por defecto es TRUE pero para este caso se tien q hacer 
#FALSE sino tomaria la primera fila como cabezera y no hay cabezera en este 
#archivo y nos estariamos comiendo una fila
datos1c = read.csv2("202007-PDE-REP-00247 - C.csv",header =FALSE,
          col.names = c("ANIO","MES","CONCESION",
                        "ENTIDAD PRESTADORA","TIPO TRAFICO",
                        "TOTAL PASAJEROS"))#e
head(datos1c)
View(datos1c)
x=datos1c[6,3]#f
typeof(x)#g
#Archivo de datos 2 (202007-PDE-REP-00274)
datos2a = read.csv2("202007-PDE-REP-00274-A.csv")#a
datos2a$X<-NULL
head(datos2a)
View(datos2a)
datos2a[104:105,]#b
datos2b = read.csv2("202007-PDE-REP-00274-B.csv",
          col.names = c("ANIO","MES","CONCESION","ENTIDAD PRESTADORA",
                        "FERROCARRIL","TIPO DE MATERIAL RODANTE",
                        "KM RECORRIDOS")
                    ,skip = 1)#c
head(datos2b)                    
View(datos2b)
typeof(datos2b$KM.RECORRIDOS)#d
typeof(datos2b[,7])
datos2c = read.csv2("202007-PDE-REP-00274-C.csv",na.strings = "?")#e
datos2c$X<-NULL
head(datos2c)
#Archivo de datos 3 (202007-PDE-REP-00075)
datos3a = read.csv2("202007-PDE-REP-00075-1.csv")#a
datos3a$X<-NULL
head(datos3a)
aero=(datos3a$AEROPUERTO)#b
str(aero)#c
datos3b = read.csv2("202007-PDE-REP-00075-2.csv",comment = "%")#d / comment=% elimina filas que inicien con %
datos3b$X<-NULL
head(datos3b)
View(datos3b)
datos3b[152,2]#e
datos3c = read.csv("202007-PDE-REP-00075-3.csv",col.names=c("ANIO",
                    "MES","CONCESION","ENTIDAD PRESTADORA","AEROPUERTO",
                    "TIPO TRAFICO","PASAJEROS DESENMBARQUE",
                    "PASAJEROS EMBARQUE")) #f
head(datos3c)
datos3c[1,8]
typeof(datos3c[1,8])#g
#Archivo de datos 4 (Adquisiciones)
install.packages("readxl")

library(readxl) 
datos4a = read_xls("ADQUISICIONES - Version 1.xls")#a
head(datos4a)
View(datos4a)
datos4a[1:5,2]#b
datos4b= read_xls("ADQUISICIONES - Version 2.xls",
                  col_names = c("PROVEEDOR","PRODUCTO","CANTIDAD"),
                  range = "C1:E10")#c
datos4b
View(datos4b)
siete=(datos4b[7,])#d
is.vector(siete)
is.data.frame(siete)
typeof(siete)
str(siete)
#Archivo de datos 5 (4.3. TRÁFICO ORIGINADO DESDE LÍNEAS FIJAS DE ABONADO)
datos5a = read_excel(
  "4.3. TRÁFICO ORIGINADO DESDE LÍNEAS FIJAS DE ABONADO.xlsx",skip =2)#a
View(datos5a)
head(datos5a)
datos5a$Tráfico
is.double(datos5a$Tráfico)#b
datos5b=read_excel("4.3. TRÁFICO ORIGINADO DESDE LÍNEAS FIJAS DE ABONADO.xlsx",
                   sheet = "Diccionario")#c
head(datos5b)
View(datos5b)
datos5b[2,2]#d

