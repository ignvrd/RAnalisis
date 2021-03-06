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
              "Fecha de operaci�n",
               "Importe","Moneda","Concepto",
               "Entidad","Nombre de producto","Tipo de producto","Tipo de movimiento","Categor�a","Nota")

#Conceptos a tener en cuenta en el an�lisis de gatos y su estructuraci�n. Adicionalmente a los activos y los 
#pasivos como primera parte del an�lisis es bueno diferencias los niveles de gatos entre aquellos a los que 
#no se puede renunciar y que son fijosy los que est�n sujetos a eliminaci�n, es decir los variables. Esto diferencia, 
#adicionalmente a los gastos e ingresos, pero en el nivel de gastos un primer nivel de diferenciaci�n entre los tipos
# de gatos y lo que se puede hacer. No hace falta conocer el detalle de la factura para ello. 

#El siguiente objetivo es conseguir acceder a los sitios de las compa��as para controlar en el caso de que haya
#que hacer cambios en las cuentas

#Transformaci�n de los tipos
movtos <- read.csv(head=TRUE,file=fic)
#Transformacion a num�rico
movtos$Importe <- as.numeric(gsub(",",".",gsub("\\.","",movtos$Importe)))
movtos <- rename(movtos, "Fecha.de.operacion" = "Fecha.de.operación" , 
                          "Categoria" = "Categor�.a"  ) 
#Transformaci�n a fechas            
movtos$Fecha.valor <-  as.Date(movtos$Fecha.valor,"%d/%m/%y")
movtos$Fecha.de.operacion <- as.Date(movtos$`Fecha.de.operación`,"%d/%m/%y")

#Desglose de dias para an�lisis
movtos$dias  <- day(movtos$Fecha.valor)
movtos$meses <- month(movtos$Fecha.valor)
movtos$a�os  <- year(movtos$Fecha.valor)
#Organizaci� de gastos
movtos$Tipo.Gasto <- "Pendiente"


############################################################
####              INGRESOS              ####################
############################################################
#Ingresos,Nomina, Rendimientos, SinClas
ingreso <- movtos[movtos$Tipo.de.movimiento %in% "Ingreso",] 
#Analisis
#�Qu� porcentaje de la n�mina va a ingresos
#�Existen patrones en los rendimientos y la n�mina
#�Qu� rendimientos est� dando el mercado para tener el dinero parado
#�Qu� excepciones hay en los ingresos. Ejemplo. Las devoluciones

#Diferentes conceptos de ingresos
nominas <- ingreso  %>% filter(ingreso$Categoria == "Nómina")
nominas_santander <- nominas %>% filter(nominas$Entidad == "Santander")
nominas_bankia <- nominas %>% filter(nominas$Entidad == "Bankia")

#OpenBank,Ing Direct,Bankia, Santander
rendtos <- ingreso  %>% filter(ingreso$Categoria == "Rendimientos")
#Devoluciones
rendtos[rendtos$Tipo.de.producto == "Tarjeta de crédito", ]
transfr <- ingreso  %>% filter(ingreso$Categoria == "Transferencias")
sinclas <- ingreso  %>% filter(ingreso$Categoria == "Sin clasificar") 

#Intereses- Rendimientos
#Representaci�n gr�fica de los ingresos
p <- ggplot(data=nominas_santander, aes(x = Fecha.valor, y = Importe))+ geom_point() + stat_smooth()
ggplot(data=rendtos[rendtos$Tipo.de.producto != "Tarjeta de crédito", ], aes(x = Fecha.valor, y = Importe, group=Nombre.de.producto,color=Nombre.de.producto))+ geom_point()

############################################################
####              GASTOS              ######################
############################################################
#Reintegro en cajero

movtos[gastos$Tipo.de.movimiento == "No computable", ]
#Fijos por categor�as 
movtos[movtos$Tipo.de.movimiento %in% "Gasto" &
       movtos$Categoria %in% "Agua" &
       movtos$Categoria %in% "Comunidad" |
       movtos$Categoria %in% "Electricidad" |
       movtos$Categoria %in%  "Gas" |
       movtos$Categoria %in% "Internet" |
       movtos$Categoria %in% "Televisión" | 
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
       movtos$Categoria %in% "Espectáculos" |
       movtos$Categoria %in% "Supermercado" |
       movtos$Categoria %in% "Ropa" | 
       movtos$Categoria %in% "Librer�a" | 
       movtos$Categoria %in% "Material deportivo", ]$Tipo.Gasto <- "variable"
       

movtos[ movtos$Categoria %in% "Parking y peaje" | 
        movtos$Categoria %in% "Farmacia" | 
        movtos$Categoria %in% "Hogar" | 
        movtos$Categoria %in% "Hotel", ]$Tipo.Gasto <- "variable"

##Excepciones
movtos[ grep( "Facturacion tarjeta credito",movtos$Nota ), ]$Tipo.Gasto <- "No computable"


grep( "Paypal *colegiodere",movtos$Nota ) 

movtos[ movtos$Categoria %in% 
Mantenimiento vehículo
Médico

Transferencias
Cargos bancario
Nómina


#Variables por notas

Paypal *colegiodere


movtos[grep( "Pendiente",movtos$Tipo.Gasto ), ]

gasolina <- gastos %>% filter(gastos$Categor�.a == "Gasolina" | gastos$Categor�.a == "Transportes" )


#Los gastos fijos son: comunidad, agua, electricidad, gas, telecomunicaciones,..
#Agua. Bimensuales. Santander.






######################
##Gastos fijos
######################

# Es necesario diferenciar los gastos fijos y los gastos variables
# Es tambi�n necesario detectar las anomal�as. Los n�meros tienen que cuadrar
# Hay elementos dubplicados como la facturaci�n de las tarjetas de cr�dito
# El an�lisis de los gastos de amazon como variable hay que pormenarizarlo y ver cu�l es 
# el porcentaje del gasto
# Detecci�n de los patrones de gasto por fijo y variable
# An�lisis temporal. Flujo de caja mensual y bimensual.  


agua <- (gastos  %>% filter(gastos$Categor�.a =="Agua") %>% select("Fecha.valor","Importe") <- "Fijo"
gastos.TipoGasto <- "Fijo"
comunidad <- gastos  %>% filter(gastos$Categor�.a =="Comunidad") %>% select("Fecha.valor","Importe")
gas <- gastos  %>% filter(gastos$Categor�.a == "Gas") %>% select("Fecha.valor","Importe")
electricidad <- gastos  %>% filter(gastos$Categor�.a == "Electricidad") %>% select("Fecha.valor","Importe")
#Netflix
telco <- gastos  %>% filter(gastos$Categor�.a == "Internet") %>% select("Fecha.valor","Importe")
solidaridad <- gastos  %>% filter(gastos$Categor�.a == "Solidaridad") %>% select("Fecha.valor","Importe")
#Seguros: salud, hogar, coche
seguros <- gastos %>% filter(gastos$Categor�.a %in% c("Seguro salud","Mm autos","Seguro auto","Otros gastos") ) %>% select("Fecha.valor","Importe","Categoria")
#Karate,csif,isaca, pmi, carreras,Project mgmt institute
cuotas <-  gastos %>% filter(gastos$Categoria == "Deporte" | gastos$Categor�.a == "Asociaciones" ) %>% select("Fecha.valor","Importe")

######################
##Gastos Variables: Gasolina, Compra, Amazon, Cajero, Resturante, Ocio
######################
##Amzn mktp,Kindle svcs
##Leroy
##Ahorra mas, compra
##udemy
##Restaurante,Espect�culos
##Supermercado
##Cargos bancarios
#Gasolina, Transportes: Metro, Renfe, Taxi, Cabify
gasolina <- gastos %>% filter(gastos$Categor�.a == "Gasolina" | gastos$Categor�.a == "Transportes" ) %>% select("Fecha.valor","Importe")



gastos %>% group_by(gastos$meses) %>% select(meses,Importe)
gastos[ gastos$Importe == 1200 , ]

tapply(gastos$Importe, gastos$meses, summary)

#facturaci�n de la tarjeta de cr�dito que hay que eliminar para evitar la doble contabilidad
grep("Facturacion",gastos$Tipo.de.movimiento)

#gastos de amazon para la confirmaci�n como consecuencia 
gastos[grep("Amzn mktp", gastos$Nota)$Importe, ]
amazon <- gastos %>% filter(gastos$Nota %like% c("Amazon.es,Amzn mktp")) %>% select("Fecha.valor","Importe")



"")

##Representaci�n agrupados por meses para ver los patrones de gastos por meses
p <- ggplot(data=gastos, aes(x = Fecha.valor, y = Importe))+ geom_point()
ggplot(data=gastos[gastos$meses== 4 & gastos$a�os == 2021 & abs(gastos$Importe) < 1000,], aes(x = Fecha.valor, y = Importe))+ geom_point()