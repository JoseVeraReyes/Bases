library(tidyverse)
library(readxl)
library(reshape)
library(lubridate)

base_2017 <- read_excel("E:/OneDrive - montgomery/INTELIGENCIA/SEGUIMIENTO VENTAS/2017.xlsx")
base_2018 <- read_excel("E:/OneDrive - montgomery/INTELIGENCIA/SEGUIMIENTO VENTAS/2018.xlsx")
base_2019 <- read_excel("E:/OneDrive - montgomery/INTELIGENCIA/SEGUIMIENTO VENTAS/2019.xlsx")
base_antes_2020 <- read_excel("E:/OneDrive - montgomery/INTELIGENCIA/SEGUIMIENTO VENTAS/marzo 2020.xlsx")
base_2020 <- read_excel("E:/OneDrive - montgomery/INTELIGENCIA/SEGUIMIENTO VENTAS/DATOS.xlsx")
Gastos <- read_excel("E:/OneDrive - montgomery/INTELIGENCIA/SEGUIMIENTO VENTAS/GASTOS.xlsx")
Gastos =  mutate(Gastos, Cantidad = Cantidad * -1)

TOTAL_BASEA = rbind(base_2017,base_2018,base_2019,base_antes_2020,base_2020)
TOTAL_BASEA$Fecha=as.Date(TOTAL_BASEA$Fecha)
TOTAL_BASEA = select(TOTAL_BASEA, -(Total))
TOTAL_BASEA = select(TOTAL_BASEA, -("Util. Bruta"))

#iNGRESOS#

TOTAL_BASEB = TOTAL_BASEA

CONDICIONALB = ifelse(TOTAL_BASEB$Tipo == "FC", 1.12, 1)
TOTAL_BASEB =  mutate(TOTAL_BASEB, Preciopublico = PreciosinIVA * CONDICIONALB)
TOTAL_BASEB = select(TOTAL_BASEB, -(PreciosinIVA))
TOTAL_BASEB =  mutate(TOTAL_BASEB, TOTAL = Preciopublico * Cantidad)
TOTAL_BASEB =  mutate(TOTAL_BASEB, DESCUENTO = TOTAL_BASEB$`%Descto`/ 100)
TOTAL_BASEB =  rename(TOTAL_BASEB, c(`%Descto`="desc"))
TOTAL_BASEB = select(TOTAL_BASEB, -(desc))

CONDICIONAL_TIPOB = ifelse(TOTAL_BASEB$Tipo == "FC", "FACTURA",
                           ifelse(TOTAL_BASEB$Tipo == "NV", "NOTA DE VENTA",TOTAL_BASEB$Tipo))
TOTAL_BASEB =cbind(TOTAL_BASEB,CONDICIONAL_TIPOB)
TOTAL_BASEB = select(TOTAL_BASEB, -(Tipo))
TOTAL_BASEB =  rename(TOTAL_BASEB, c(CONDICIONAL_TIPOB="TIPO TRANSACCION"))

CUENTA_CONTABLE = "INGRESOS"
TOTAL_BASEB =cbind(TOTAL_BASEB,CUENTA_CONTABLE)


#costo 

Gastos$Fecha=as.Date(Gastos$Fecha)
Gastos = select(Gastos, -(Total))
Gastos = select(Gastos, -("Util. Bruta"))

TOTAL_BASE_COSTO =  rbind(TOTAL_BASEA,Gastos)
CONDICIONAL_costo = ifelse(TOTAL_BASE_COSTO$Tipo == "FC", 1.12, 1)
TOTAL_BASE_COSTO =  mutate(TOTAL_BASE_COSTO, Preciopublico = (TOTAL_BASE_COSTO$PreciosinIVA * CONDICIONAL_costo))
TOTAL_BASE_COSTO =  mutate(TOTAL_BASE_COSTO, TOTAL = -1 * TOTAL_BASE_COSTO$Costo)
TOTAL_BASE_COSTO =  mutate(TOTAL_BASE_COSTO, DESCUENTO = TOTAL_BASE_COSTO$`%Descto`/ 100)
TOTAL_BASE_COSTO =  rename(TOTAL_BASE_COSTO, c(`%Descto`="desc"))
TOTAL_BASE_COSTO = select(TOTAL_BASE_COSTO, -(desc))
TOTAL_BASE_COSTO = select(TOTAL_BASE_COSTO, -(PreciosinIVA))

IS_NA = is.na(TOTAL_BASE_COSTO$TOTAL)
TOTAL_BASE_COSTO =cbind(TOTAL_BASE_COSTO,IS_NA)
CUENTA_CONTABLEc = ifelse(TOTAL_BASE_COSTO$IS_NA == 1,"EGRESOS", "EGRESOS")
TOTAL_BASE_COSTO =cbind(TOTAL_BASE_COSTO,CUENTA_CONTABLEc)
TOTAL_BASE_COSTO = select(TOTAL_BASE_COSTO, -("IS_NA"))
TOTAL_BASE_COSTO =  rename(TOTAL_BASE_COSTO, c(CUENTA_CONTABLEc="CUENTA_CONTABLE"))



CONDICIONAL_TIPO_COSTO = ifelse(TOTAL_BASE_COSTO$Tipo == "FC", "COSTOS DE VENTAS",
                                ifelse(TOTAL_BASE_COSTO$Tipo == "NV", "COSTOS DE VENTAS",TOTAL_BASE_COSTO$Tipo))

TOTAL_BASE_COSTO =cbind(TOTAL_BASE_COSTO,CONDICIONAL_TIPO_COSTO)
TOTAL_BASE_COSTO = select(TOTAL_BASE_COSTO, -(Tipo))
TOTAL_BASE_COSTO =  rename(TOTAL_BASE_COSTO, c(CONDICIONAL_TIPO_COSTO="TIPO TRANSACCION"))

TOTAL_BASE = rbind(TOTAL_BASEB,TOTAL_BASE_COSTO)
TOTAL_BASE = select(TOTAL_BASE, -("Preciopublico"))
TOTAL_BASE = select(TOTAL_BASE, -("Costo"))
TOTAL_BASE = select(TOTAL_BASE, -("DESCUENTO"))

TIPO_CLIENTE = ifelse(TOTAL_BASE$Cliente == "CONSUMIDOR FINAL","CONSUMIDOR FINAL", "CLIENTES REGISTRADOS")
TOTAL_BASE =cbind(TOTAL_BASE,TIPO_CLIENTE)

AÑO = year(TOTAL_BASE$Fecha)
TOTAL_BASE =cbind(TOTAL_BASE,AÑO)

MES = month(TOTAL_BASE$Fecha)
TOTAL_BASE =cbind(TOTAL_BASE,MES)
NOMBRE_MES= format(TOTAL_BASE$Fecha,"%B")
TOTAL_BASE =cbind(TOTAL_BASE,NOMBRE_MES)

DIA = day(TOTAL_BASE$Fecha)
TOTAL_BASE =cbind(TOTAL_BASE,DIA)
NOMBRE_DIA= format(TOTAL_BASE$Fecha,"%A")
TOTAL_BASE =cbind(TOTAL_BASE,NOMBRE_DIA)

SEMANA = week(TOTAL_BASE$Fecha)
TOTAL_BASE =cbind(TOTAL_BASE,SEMANA)
SEMANA_MES=format(TOTAL_BASE$Fecha,"%u")
TOTAL_BASE =cbind(TOTAL_BASE,SEMANA_MES)

GRUPO_PRODUCT_DOS = ifelse(grepl("PAPEL", TOTAL_BASE$Descripcion), "PAPEL Y HOJAS DE TODO TIPO",
                           ifelse(grepl("H.", TOTAL_BASE$Descripcion), "PAPEL Y HOJAS DE TODO TIPO",
                                  ifelse(grepl("RESMAS", TOTAL_BASE$Descripcion), "BONDS Y RESMAS",
                                         ifelse(grepl("BOND", TOTAL_BASE$Descripcion), "BONDS Y RESMAS",
                                        ifelse(grepl("SUELDOS Y SALARIOS", TOTAL_BASE$Descripcion), "SUELDOS Y SALARIOS",
                                      ifelse(grepl("ADELANTO", TOTAL_BASE$Descripcion), "SUELDOS Y SALARIOS",
                                      ifelse(grepl("TRABAJO CAR", TOTAL_BASE$Descripcion), "CARPINTERO",
                                    ifelse(grepl("COMISION", TOTAL_BASE$Descripcion), "COMISION POR VENTAS",
                                 ifelse(grepl("ALICUOTA", TOTAL_BASE$Descripcion), "ALICUOTA",
                                        ifelse(grepl("PAGO", TOTAL_BASE$Descripcion), "PAGO",
                              ifelse(grepl("CHOCOLATE", TOTAL_BASE$Descripcion), "CONFITES",
                          ifelse(grepl("MILKY", TOTAL_BASE$Descripcion), "CONFITES",
                          ifelse(grepl("MENTA", TOTAL_BASE$Descripcion), "CONFITES",
                                 ifelse(grepl("GALLETA", TOTAL_BASE$Descripcion), "CONFITES",
                              ifelse(grepl("TUMIX", TOTAL_BASE$Descripcion), "CONFITES",
                             ifelse(grepl("MANÍ", TOTAL_BASE$Descripcion), "CONFITES",
                                    ifelse(grepl("KATABOOM", TOTAL_BASE$Descripcion), "CONFITES",
                              ifelse(grepl("SNICKERS", TOTAL_BASE$Descripcion), "CONFITES",
                                             ifelse(grepl("RECARGA", TOTAL_BASE$Descripcion), "RECARGAS",
                                                ifelse(grepl("IMPRESION",TOTAL_BASE$Descripcion),"IMPRESIONES","OTROS"))))))))))))))))))))

GRUPO_PRODUCT <- ifelse(grepl("COPIA", TOTAL_BASE$Descripcion), "COPIAS",ifelse(grepl("PLOT", TOTAL_BASE$Descripcion), "PLOTEOS", 
                          ifelse(grepl("SCAN", TOTAL_BASE$Descripcion), "SCANEOS",
                                 ifelse(grepl("AGUA", TOTAL_BASE$Descripcion), "AGUAS",
            ifelse(grepl("PANT", TOTAL_BASE$Descripcion), "PANTALONES",
                   ifelse(grepl("CAMI", TOTAL_BASE$Descripcion), "CAMISAS",
                          ifelse(grepl("BVD", TOTAL_BASE$Descripcion), "BVD",      
                          ifelse(grepl("CHOMP", TOTAL_BASE$Descripcion), "CHOMPAS",
                                 ifelse(grepl("LICRA", TOTAL_BASE$Descripcion), "LICRA",                        
                                 ifelse(grepl("PULLOVER", TOTAL_BASE$Descripcion), "PULLOVER",      
                                 ifelse(grepl("CARTUCHERA", TOTAL_BASE$Descripcion), "CARTUCHERAS - CARTERAS Y BOLSOS",
                               ifelse(grepl("CARTERA", TOTAL_BASE$Descripcion), "CARTUCHERAS - CARTERAS Y BOLSOS",
                               ifelse(grepl("BOLSO", TOTAL_BASE$Descripcion), "CARTUCHERAS - CARTERAS Y BOLSOS",
                                 ifelse(grepl("CART.", TOTAL_BASE$Descripcion), "CARTULINAS",  
                                 ifelse(grepl("CARPET", TOTAL_BASE$Descripcion), "CARPETAS",
          ifelse(grepl("VINCHA", TOTAL_BASE$Descripcion), "VINCHA",                                 
           ifelse(grepl("FOLDE", TOTAL_BASE$Descripcion), "CARPETAS",
                  ifelse(grepl("ARCHIV", TOTAL_BASE$Descripcion), "CARPETAS",        
                  ifelse(grepl("MOÑO", TOTAL_BASE$Descripcion), "MOÑOS",
                  ifelse(grepl("BOL. ", TOTAL_BASE$Descripcion), "BOLIGRAFOS",
                         ifelse(grepl("BOLIG", TOTAL_BASE$Descripcion), "BOLIGRAFOS",
                                ifelse(grepl("CALENT. ", TOTAL_BASE$Descripcion), "CALENTADORES",
                ifelse(grepl("C.D/A", TOTAL_BASE$Descripcion), "CUADERNOS",
                ifelse(grepl("C. COSIDO", TOTAL_BASE$Descripcion), "CUADERNOS",
               ifelse(grepl("C. PAUTADO", TOTAL_BASE$Descripcion), "CUADERNOS",
              ifelse(grepl("CUADERNO", TOTAL_BASE$Descripcion), "CUADERNOS",
           ifelse(grepl("ETIQU", TOTAL_BASE$Descripcion), "ETIQUETAS",               
              ifelse(grepl("FOMI", TOTAL_BASE$Descripcion), "FOMIX",
              ifelse(grepl("MOCHI", TOTAL_BASE$Descripcion), "MOCHILAS",
              ifelse(grepl("LAPI", TOTAL_BASE$Descripcion), "LAPICES Y CRAYONES",
              ifelse(grepl("CRAYON", TOTAL_BASE$Descripcion), "LAPICES Y CRAYONES",
              ifelse(grepl("TEMPERA", TOTAL_BASE$Descripcion), "LAPICES Y CRAYONES",
             ifelse(grepl("PINCEL", TOTAL_BASE$Descripcion), "PINCELES Y ACUARELAS",
           ifelse(grepl("ACUARE", TOTAL_BASE$Descripcion), "PINCELES Y ACUARELAS",        
            ifelse(grepl("SOBRE", TOTAL_BASE$Descripcion), "SOBRES",
            ifelse(grepl("CINTA", TOTAL_BASE$Descripcion), "CINTA",
             ifelse(grepl("GOMA", TOTAL_BASE$Descripcion), "GOMA",
          ifelse(grepl("MARC. ", TOTAL_BASE$Descripcion), "MARCADORES",
           ifelse(grepl("MARCADOR", TOTAL_BASE$Descripcion), "MARCADORES",         
             ifelse(grepl("CLIPS", TOTAL_BASE$Descripcion), "CLIPS",
          ifelse(grepl("GRABACION", TOTAL_BASE$Descripcion), "GRABACION",                
              ifelse(grepl("CD", TOTAL_BASE$Descripcion), "CDs",                   
  ifelse(grepl("BORRAD", TOTAL_BASE$Descripcion), "BORRADORES",
         ifelse(grepl("MEMORIA", TOTAL_BASE$Descripcion), "PENDRIVES Y TARJETAS",
         ifelse(grepl("PENDRIVE", TOTAL_BASE$Descripcion), "PENDRIVES Y TARJETAS",          
         ifelse(grepl("MOUSE W", TOTAL_BASE$Descripcion), "MOUSES",
                ifelse(grepl("MOUSE G", TOTAL_BASE$Descripcion), "MOUSES",
                       ifelse(grepl("MOUSE P", TOTAL_BASE$Descripcion), "MOUSES",
              ifelse(grepl("PAPEL", TOTAL_BASE$Descripcion), "PAPEL Y HOJAS DE TODO TIPO",
                     GRUPO_PRODUCT_DOS)))))))))))))))))))))))))))))))))))))))))))))))))

TOTAL_BASE =cbind(TOTAL_BASE,GRUPO_PRODUCT)

BASE_RENTABILIDAD = TOTAL_BASE

















                            
                            
                            