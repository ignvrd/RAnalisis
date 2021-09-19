
#Importacion de paquetes necesarios
#dplyr
install.packages("dplyr")
install.packages("xlsx")

library(xlsx)
library("dplyr")

#Configuración del servicio. 
L1_servicios = c("adaptativo", "operativo","soporte a usuario","SICAM","correctivo","evolutivo");
L2_servicios = c("adaptativo", "operativo","soporte a usuario","SICAM","correctivo","evolutivo");

campos_sistemas_minsait <- c("LOTE","APLICACION","MODULO","DESCRIPCION",
                            "CRITICIDAD","TIPIFICACION","ID_JIRA","TECNOLOGIA","AMBITO","SUBDIRECCION",
                            "RESPONSABLE_IAM","JEFE_SERVICIO",
                            "FECHA_INCORPORACIÓN_SERVICIO","FECHA PLENO SERVICIO",
                            "OBSERVACIONES","COORDINADOR (provisional)");

campos_sistemas_iam <- c("LOTE_ACTUAL", "SUBDIRECCION_ANTIGUA", "APLICACION", 
                        "DESCRIPCION", "NUEVO_LOTE", "SUBDIRECCION_NUEVA","DEPARTAMENTO", 
                        "AMBITO", "VERIFICADO.IM", "X");

campos_sistemas_cmdb <- c("CODIGO","APLICACION","DESCRIPCION","SUBDIRECCION_IAM",
                          "UNIDAD_IAM","DEPARTAMENTO_IAM",
                          "UNIDAD_AYTO","AREA_AYTO",
                          "USUARIO_FINAL","ESTADO");



L1_ambitos = c("Administración Digital","Recursos humanos",
               "Gestión Económica y financiera",
               "Ingresos");

L2_ambitos = c("Urbanismo","Ciudad","Atención a la ciudadanía","Sectorial");

L2_contacto = ""
L2_jefes_servicio = ""


#
sistemas_minsait_raw <- data.frame(read.csv(file="C:/Users/IVD007/Downloads/SMG2_PORTFOLIO_120821_CSV.csv",sep=";",
                                            col.names=campos_sistemas,header=TRUE));
sistemas_portfolio_raw <- data.frame(read.csv(file="C:/Users/IVD007/Downloads/NUEVO_PORTFOLIO7_CSV.csv",sep=";",
                                              col.names=campos_sistemas_iam,header=TRUE));
sistemas_cmdb_raw <- data.frame(read.csv(file="C:/Users/IVD007/Downloads/Catalogo_Sistemas_CSV.csv",sep=";",
                                         col.names=campos_sistemas_cmdb,header=TRUE));


aplicaciones_minsait <- data.frame("APLICACION"=c(unique(sistemas_minsait_raw$APLICACION)))
aplicaciones_iam     <- data.frame("APLICACION"=c(unique(sistemas_portfolio_raw$APLICACION)));
aplicaciones_cmdb    <- data.frame("APLICACION"=c(unique(sistemas_cmdb_raw$APLICACION)));

aplicaciones_iam$APLICACION[!((aplicaciones_iam$APLICACION %in% aplicaciones_minsait$APLICACION))]
aplicaciones_minsait$APLICACION[!((aplicaciones_minsait$APLICACION %in% aplicaciones_iam$APLICACION))]
aplicaciones_cmdb$APLICACION[((aplicaciones_cmdb$APLICACION %in% aplicaciones_iam$APLICACION))]

#Coherencia - IAM
sistemas_portfolio_raw %>% filter(sistemas_portfolio_raw$AMBITO %in% L1_ambitos) %>% filter (NUEVO_LOTE!="L1") %>% select(APLICACION,NUEVO_LOTE,AMBITO)
sistemas_portfolio_raw %>% filter(sistemas_portfolio_raw$AMBITO %in% L2_ambitos) %>% filter (NUEVO_LOTE!="L2") %>% select(APLICACION,NUEVO_LOTE,AMBITO)

#Coherencia - MINSAIT
sistemas_minsait_raw %>% filter(sistemas_minsait_raw$AMBITO %in% L1_ambitos) %>% filter (LOTE!="L1") %>% select(APLICACION,LOTE,AMBITO)
sistemas_minsait_raw %>% filter(sistemas_minsait_raw$AMBITO %in% L2_ambitos) %>% filter (LOTE!="L2") %>% select(APLICACION,LOTE,AMBITO)

sistemas_minsait_raw[ sistemas_minsait_raw$APLICACION == "TRAPE", "SUBDIRECCION"  ]!= 
sistemas_portfolio_raw[sistemas_portfolio_raw$APLICACION == "TRAPE", "SUBDIRECCION_ANTIGUA"] 



A <- read.csv(file="C:/Users/IVD007/Downloads/WEB_APP.txt",sep=";",header=TRUE)


   filter( ) & sistemas_minsait_raw$SUBDIRECCION != sistemas_portfolio_raw$SUBDIRECCION 
  
  NUEVO_LOTE[sistemas_portfolio_raw$AMBITO %in% L1_ambitos]
sistemas_portfolio_raw$NUEVO_LOTE[sistemas_portfolio_raw$AMBITO %in% L2_ambitos]





#Lectura del portfolio
#Campos: Validado;Lote;µmbito;Aplicaci¢n;M¢dulo;Descripci¢n;Criticidad;Tipificaci¢n;ID_JIRA;
#.Tecnolog¡a;Subdirecci¢n;Responsable IAM;Jefe Servicio;Coordinador (provisional);Observaciones ;
nportfolio <- read.csv(file="C:/CMDB/SMG2_PORTFOLIO_CSV.csv",sep=";",header=TRUE)
nportfolio$ESTADO_PORTFOLIO <- "ANTIGUAS"
PERFI,PETIC,RPTMV -> No tienen ámbito
//**
	CONOT	Convenio con el colegio de notarios
	CTITR	Cambio de Titularidad IBI / TRU. Aplicación WEB para ATM
	ECEDE	Emisión de certificados de deuda
	PATMV	Proyecto Acreditación Telemática impuesto Municipal Vehículos
	PBING	Procesos BATCH de Ingresos Gestión de Ingresos - Datos usuarios UWEB
	CIUAB_VIRTUOSO	Plataforma de Gobierno Abierto desarrollada en el marco de un proyecto de red.es en el que participan las ciudades de Madrid, Zaragoza, Santiago y La Coruña. El cometido de dicha plataforma será la publicación en el portal de datos abiertos de la información extraída de los distintos sistemas de información del ayuntamiento. Volcada y tratada sobre MICROSTRATEGY, recuperada mediante un API 
CIUAB	CIUAB_SOLR	
CIUAB	CIUAB_RSAPI_CA*//

nportfolio$Aplicaci.n <- "CONOT"
nportfolio$Aplicaci.n <- "CTITR"
nportfolio$Aplicaci.n <- "ECEDE"
nportfolio$Aplicaci.n <- "PATMV"
nportfolio$Aplicaci.n <- "PBING"
nportfolio$Aplicaci.n <- "CIUAB"

nportfolio$ESTADO_PORTFOLIO <- "PENDIENTE_TRASPASO"

length(nportfolio$Aplicaci.n)
# Revision de la presentacion de INDRA
"

#Sistemas de INFO CMDB
#Campos Code;Nombre;Descripción Funcional;Tipo;Estado;Unidad Responsable IAM;Unidad Responsable Ayuntamiento
cmdbssii <- read.csv(file="C:/CMDB/20210916_CMDB_SISTEMAS_INFORMACION.csv",sep=";",header=TRUE)
cmdbelswssii <- read.csv(file="C:/CMDB/20210916_CMDB_ELTOS_SW.csv",sep=";", header=TRUE)
#SISTEMAS DE LA CMDB EN EL PORTFOLIO:
cmdbssiiportf <- cmdbssii[cmdbssii$Nombre %in% unique(Nportfolio$Aplicaci.n), ]
###LOS DEL TRASPASO
#cmdbssiiportfn <- cmdbssii[cmdbssii$Nombre %in% c("CONOT","CTITR","ECEDE","PATMV","PBING","CIUAB"), ] 
#cmdbssiiportf <- rbind(cmdbssiiportf,cmdbssiiportfn)

length(cmdbssiiportf$Nombre)
length(unique(cmdbssiiportf$Nombre))
#Estan en el cmdb pero no se contemplan -> 
#cmdbssii[!(cmdbssii$Nombre %in% unique(nportfolio$Aplicaci.n)), ]$Nombre
#ESTAN EN EL PORTFOLIO PERO NO EN LA CMDB: 27
write.xlsx2(nportfolio[ (!Nportfolio$Aplicaci.n %in% unique(cmdbssii$Nombre)),] %>% select(Aplicaci.n,Descripci.n),
            file = "C:/CMDB/Prueba_ESCRITURA_6.xls",append=TRUE,sheetName = "NO_CMDB",col.names = TRUE)



portssii <- merge(x=nportfolio,y=cmdbssii, 
                by.x="Aplicaci.n",by.y="Nombre", 
                sort = TRUE, all.x = TRUE, suffixes = c("PORT","CMDB"))
length(portssii$Aplicaci.n)
#Juntamos los dos excell para los campos 
portssiiel <- merge(x=portssii,y=cmdbelswssii,
                    by.x="Aplicaci.n",by.y="Sistema.de.InformaciÃ³n",
                    sort = TRUE, all.x = TRUE , suffixes = c("SSII","ELSW"))
length(portssiiel$Aplicaci.n)

portssiiel <-  rename(portssiiel,c("Aplicacion" = "Aplicaci.n","ambito" = "µmbito","Tecnologia" = "Tecnolog.a",
                                   "Modulo" = "M.dulo","Descripcion" = "Descripci.n","Descripcion.FuncionalSSII" = "DescripciÃ³n.FuncionalSSII",
                                   "Plan.Actuacion.Obsolescencia" = "Plan.ActuaciÃ³n.Obsolescencia","Coordinador.provisional" = "Coordinador..provisional.",
                                   "Descripcion.FuncionalELSW"="DescripciÃ³n.FuncionalELSW","Tipificacion" = "Tipificaci.n","Subdireccion" = "Subdirecci.n"))

##portssiiel <- portssiiel %>% select(-"Validado",-"ID_JIRA",-"X",-"Criticidad",-"Tipificacion")

portssiiel <- portssiiel %>% select("CodeSSII","Aplicacion","Modulo","Nombre","CodeELSW","Contrato.Principal","Lote","EstadoSSII","EstadoELSW","ambito","Descripcion","Descripcion.FuncionalSSII","Descripcion.FuncionalELSW",
                                    "Subdireccion","Responsable.IAM","Unidad.Responsable.IAM","Unidad.Responsable.Ayuntamiento","Jefe.Servicio","Coordinador.provisional","Contacto","Unidad.Contacto","SubdireccionContacto",
                                    "Producto","Tecnologia","Tipo","Tipo.Desarrollo","Tipo.Gestor.BBDD","Obsoleto","Plan.Actuacion.Obsolescencia","Observaciones",
                                     "Validado","ID_JIRA","X","Criticidad","Tipificacion")





##write.csv2(portssiiel, file = "C:/CMDB/Prueba_ESCRITURA_2.csv")
write.xlsx2(portssiiel, file = "C:/CMDB/Prueba_ESCRITURA_8.xls",sheetName = "Portfolio+CMDB",col.names = TRUE)


write.xlsx2( portssiiel[ portssiiel$Aplicacion == "SIGMA" & portssiiel$Modulo == "SIGMA", ] %>% select ("Aplicacion","Modulo","CodeELSW"), 
             file = "C:/CMDB/MODULOS_SIGMA.xls",
             col.names = TRUE)

CATASTRO
SECCENS
SIGRA

SIGMA3V04
CATASTRO
GUIAURB
MALLA

GEOSERVICIOS
ORTOFOTOS
GEODATABASES
ARCGISPORTAL
ARCGISONLINE
COORDINACIÓN
DEMADISLA
DEMADBOSQUE
DEMAD3D
PRADORETIRO####

###HAY QUE CRUZAR EL PORTFOLIO CON JIRA

##HAY QUE CRUZAR EL PORTFOLIO OCN CONFLUENCE



cmdbssii$Nombre
tail(sort(cmdbelswssii$`Sistema.de.InformaciÃ³n`))
cmdbelswssii$Nombre %in% Nportfolio_nombres
cmdbelswssii[ cmdbelswssii$Nombre %in% por, ]$Nombre

cmd <- unique(sort(cmdbssii$Nombre))   
por <- unique(sort(Nportfolio_nombres))
Sistema de Información;Code;Nombre;Descripción Funcional;Estado;Contacto;Unidad Contacto;SubdireccionContacto;Contrato Principal;Tipo Gestor BBDD;Obsoleto;Plan Actuación Obsolescencia;Tipo Desarrollo;Producto

Unidad Contacto;SubdireccionContacto;Contrato Principal

sort(unique(cmdbssii[cmdbssii$Nombre %in% unique(Nportfolio_nombres), ]$Nombre))
pruebas_ <- sort(cmdbelswssii[cmdbelswssii$`Sistema.de.InformaciÃ³n` %in% unique(Nportfolio_nombres), ],cmdbelswssii$`Sistema.de.InformaciÃ³n`)

pruebas <- pruebas_[order(pruebas_$`Sistema.de.InformaciÃ³n`),]

< - pruebas_ %>% select("Sistema.de.InformaciÃ³n", "Contacto", "Unidad.Contacto","SubdireccionContacto","Contrato.Principal","Tipo.Desarrollo","Contrato.Principal")
pruebas_$Fuente <- "CMDB"
pruebas_$Fuente <- "CMDB"

pruebas_$`Sistema.de.InformaciÃ³n`

write.table(pruebas_, file = "C:/CMDB/Prueba_ESCRITURA.csv", append = FALSE, sep = ";", col.names = TRUE)

%>% select("Unidad Contacto","SubdireccionContacto","Contrato Principal")
length(unique(cmdbssii[cmdbssii$Nombre %in% unique(Nportfolio_nombres), ]$Nombre))
length(unique(cmdbelswssii[cmdbelswssii$`Sistema.de.InformaciÃ³n` %in% unique(Nportfolio_nombres), ]$`Sistema.de.InformaciÃ³n`))