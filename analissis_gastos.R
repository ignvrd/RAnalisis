############################################################
####              ANALISIS DE GASTOS E INGRESOS       ######
############################################################

library("dplyr")
library("ggplot2")
library("tidyr")
library("data.table")
library("lubridate") 

fic <- "C:/Users/IVD007/Downloads/movimientos.csv"

columnas <- c("Fecha valor",
              "Fecha de operación",
               "Importe","Moneda","Concepto",
               "Entidad","Nombre de producto","Tipo de producto","Tipo de movimiento","Categoría","Nota")

#Conceptos a tener en cuenta en el análisis de gatos y su estructuración. Adicionalmente a los activos y los 
#pasivos como primera parte del análisis es bueno diferencias los niveles de gatos entre aquellos a los que 
#no se puede renunciar y que son fijosy los que están sujetos a eliminación, es decir los variables. Esto diferencia, 
#adicionalmente a los gastos e ingresos, pero en el nivel de gastos un primer nivel de diferenciación entre los tipos
# de gatos y lo que se puede hacer. No hace falta conocer el detalle de la factura para ello. 

#El siguiente objetivo es conseguir acceder a los sitios de las compañías para controlar en el caso de que haya
#que hacer cambios en las cuentas

#Transformación de los tipos
movtos <- read.csv(head=TRUE,file=fic)
#Transformacion a numérico
movtos$Importe <- as.numeric(gsub(",",".",gsub("\\.","",movtos$Importe)))
movtos <- rename(movtos, "Fecha.de.operacion" = "Fecha.de.operaciÃ³n" , 
                          "Categoria" = "CategorÃ.a"  ) 
#Transformación a fechas            
movtos$Fecha.valor <-  as.Date(movtos$Fecha.valor,"%d/%m/%y")
movtos$Fecha.de.operacion <- as.Date(movtos$`Fecha.de.operaciÃ³n`,"%d/%m/%y")

#Desglose de dias para análisis
movtos$dias  <- day(movtos$Fecha.valor)
movtos$meses <- month(movtos$Fecha.valor)
movtos$años  <- year(movtos$Fecha.valor)
#Organizació de gastos
movtos$Tipo.Gasto <- "Pendiente"


############################################################
####              INGRESOS              ####################
############################################################
#Ingresos,Nomina, Rendimientos, SinClas
ingreso <- movtos[movtos$Tipo.de.movimiento %in% "Ingreso",] 
#Analisis
#¿Qué porcentaje de la nómina va a ingresos
#¿Existen patrones en los rendimientos y la nómina
#¿Qué rendimientos está dando el mercado para tener el dinero parado
#¿Qué excepciones hay en los ingresos. Ejemplo. Las devoluciones

#Diferentes conceptos de ingresos
nominas <- ingreso  %>% filter(ingreso$Categoria == "NÃ³mina")
nominas_santander <- nominas %>% filter(nominas$Entidad == "Santander")
nominas_bankia <- nominas %>% filter(nominas$Entidad == "Bankia")

#OpenBank,Ing Direct,Bankia, Santander
rendtos <- ingreso  %>% filter(ingreso$Categoria == "Rendimientos")
#Devoluciones
rendtos[rendtos$Tipo.de.producto == "Tarjeta de crÃ©dito", ]
transfr <- ingreso  %>% filter(ingreso$Categoria == "Transferencias")
sinclas <- ingreso  %>% filter(ingreso$Categoria == "Sin clasificar") 

#Intereses- Rendimientos
#Representación gráfica de los ingresos
p <- ggplot(data=nominas_santander, aes(x = Fecha.valor, y = Importe))+ geom_point() + stat_smooth()
ggplot(data=rendtos[rendtos$Tipo.de.producto != "Tarjeta de crÃ©dito", ], aes(x = Fecha.valor, y = Importe, group=Nombre.de.producto,color=Nombre.de.producto))+ geom_point()

############################################################
####              GASTOS              ######################
############################################################
#Reintegro en cajero

movtos[gastos$Tipo.de.movimiento == "No computable", ]
#Fijos por categorías 
movtos[movtos$Tipo.de.movimiento %in% "Gasto" &
       movtos$Categoria %in% "Agua" &
       movtos$Categoria %in% "Comunidad" |
       movtos$Categoria %in% "Electricidad" |
       movtos$Categoria %in%  "Gas" |
       movtos$Categoria %in% "Internet" |
       movtos$Categoria %in% "TelevisiÃ³n" | 
       movtos$Categoria %in% "Solidaridad" | 
       movtos$Categoria %in% "Seguro salud" |
       movtos$Categoria %in% "Mm autos" |  
       movtos$Categoria %in% "Seguro auto" |
       movtos$Categoria %in% "Asociaciones" | 
       movtos$Categoria %in% "Gasolina" | 
       movtos$Categoria %in% "Deporte" |
       movtos$Categoria %in% "Transportes", ]$Tipo.Gasto <- "Fijo"

#Variables por notas
movtos[grep( "Amazon.es",movtos$Nota ), ]$Tipo.Gasto <- "variable" 
movtos[grep( "Amzn mktp",movtos$Nota ), ]$Tipo.Gasto <- "variable" 
movtos[grep( "Kindle",movtos$Nota ), ]$Tipo.Gasto <- "variable"

movtos[grep( "Reintegro en cajero",movtos$Nota ), ]$Tipo.Gasto <- "variable"
movtos[movtos$Categoria %in% "Efectivo", ]$Tipo.Gasto <- "variable"  

movtos[movtos$Categoria %in% "Traspasos y transferencias", ]$Tipo.Gasto <-"No computable"

movtos[movtos$Tipo.de.movimiento %in% "Gasto" &
       movtos$Categoria %in% "Restaurante" | 
       movtos$Categoria %in% "Estudios" | 
       movtos$Categoria %in% "Belleza" | 
       movtos$Categoria %in% "EspectÃ¡culos" |
       movtos$Categoria %in% "Supermercado" |
       movtos$Categoria %in% "Ropa" | 
       movtos$Categoria %in% "LibrerÃa" | 
       movtos$Categoria %in% "Material deportivo", ]$Tipo.Gasto <- "variable"
       

movtos[ movtos$Categoria %in% "Parking y peaje" | 
        movtos$Categoria %in% "Farmacia" | 
        movtos$Categoria %in% "Hogar" | 
        movtos$Categoria %in% "Hotel", ]$Tipo.Gasto <- "variable"

##Excepciones
movtos[ grep( "Facturacion tarjeta credito",movtos$Nota ), ]$Tipo.Gasto <- "No computable"


grep( "Paypal *colegiodere",movtos$Nota ) 

movtos[ movtos$Categoria %in% 
Mantenimiento vehÃ­culo
MÃ©dico

Transferencias
Cargos bancario
NÃ³mina


#Variables por notas

Paypal *colegiodere


movtos[grep( "Pendiente",movtos$Tipo.Gasto ), ]

gasolina <- gastos %>% filter(gastos$CategorÃ.a == "Gasolina" | gastos$CategorÃ.a == "Transportes" )


#Los gastos fijos son: comunidad, agua, electricidad, gas, telecomunicaciones,..
#Agua. Bimensuales. Santander.






######################
##Gastos fijos
######################

# Es necesario diferenciar los gastos fijos y los gastos variables
# Es también necesario detectar las anomalías. Los números tienen que cuadrar
# Hay elementos dubplicados como la facturación de las tarjetas de crédito
# El análisis de los gastos de amazon como variable hay que pormenarizarlo y ver cuál es 
# el porcentaje del gasto
# Detección de los patrones de gasto por fijo y variable
# Análisis temporal. Flujo de caja mensual y bimensual.  


agua <- (gastos  %>% filter(gastos$CategorÃ.a =="Agua") %>% select("Fecha.valor","Importe") <- "Fijo"
gastos.TipoGasto <- "Fijo"
comunidad <- gastos  %>% filter(gastos$CategorÃ.a =="Comunidad") %>% select("Fecha.valor","Importe")
gas <- gastos  %>% filter(gastos$CategorÃ.a == "Gas") %>% select("Fecha.valor","Importe")
electricidad <- gastos  %>% filter(gastos$CategorÃ.a == "Electricidad") %>% select("Fecha.valor","Importe")
#Netflix
telco <- gastos  %>% filter(gastos$CategorÃ.a == "Internet") %>% select("Fecha.valor","Importe")
solidaridad <- gastos  %>% filter(gastos$CategorÃ.a == "Solidaridad") %>% select("Fecha.valor","Importe")
#Seguros: salud, hogar, coche
seguros <- gastos %>% filter(gastos$CategorÃ.a %in% c("Seguro salud","Mm autos","Seguro auto","Otros gastos") ) %>% select("Fecha.valor","Importe","Categoria")
#Karate,csif,isaca, pmi, carreras,Project mgmt institute
cuotas <-  gastos %>% filter(gastos$Categoria == "Deporte" | gastos$CategorÃ.a == "Asociaciones" ) %>% select("Fecha.valor","Importe")

######################
##Gastos Variables: Gasolina, Compra, Amazon, Cajero, Resturante, Ocio
######################
##Amzn mktp,Kindle svcs
##Leroy
##Ahorra mas, compra
##udemy
##Restaurante,Espectáculos
##Supermercado
##Cargos bancarios
#Gasolina, Transportes: Metro, Renfe, Taxi, Cabify
gasolina <- gastos %>% filter(gastos$CategorÃ.a == "Gasolina" | gastos$CategorÃ.a == "Transportes" ) %>% select("Fecha.valor","Importe")



gastos %>% group_by(gastos$meses) %>% select(meses,Importe)
gastos[ gastos$Importe == 1200 , ]

tapply(gastos$Importe, gastos$meses, summary)

#facturación de la tarjeta de crédito que hay que eliminar para evitar la doble contabilidad
grep("Facturacion",gastos$Tipo.de.movimiento)

#gastos de amazon para la confirmación como consecuencia 
gastos[grep("Amzn mktp", gastos$Nota)$Importe, ]
amazon <- gastos %>% filter(gastos$Nota %like% c("Amazon.es,Amzn mktp")) %>% select("Fecha.valor","Importe")



"")

##Representación agrupados por meses para ver los patrones de gastos por meses
p <- ggplot(data=gastos, aes(x = Fecha.valor, y = Importe))+ geom_point()
ggplot(data=gastos[gastos$meses== 4 & gastos$años == 2021 & abs(gastos$Importe) < 1000,], aes(x = Fecha.valor, y = Importe))+ geom_point()