
install.packages("pdftools")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tesseract") 
install.packages("tabulizer")
install.packages("stringr")
install.packages("gtools")
install.packages("gmailr")
install.packages("magrittr")
install.packages("tidyr")

library(pdftools)
library(dplyr)

#El objetivo es tener control de las facturas y de los gatos. Controlar las finanzas y diferenciar los 
#gastos fijos de los variables, manejando correctamente 
#Electricidad. Mensual. Cure Energia. Iberdrola. URL: https://www.curenergia.es/webclifr/#/login.  
#IGVIRSDU. Cuenta de cargo en: XXX
#Gas.Madrileña de Gas URL: https://ov.madrilena.es/saveduser. 
#y el mantenimiento de la caldera. IGNACIO.VIRSEDA@GMAIL.COM
#Caldera. Saunier. URL https://areaclientes.sauniertec.com/login      
#Agua. BiMensual. Canal de Isabel II. URL: https://oficinavirtual.canaldeisabelsegunda.es/recytal/private/consultar_facturas.htm
# 50859770P
#Comunidad
#Seguros
#Seguro hogar. URL https://www.mapfre.es/oim/ValidarIdentificacionAction.do?pid1=20151020-pmp-neg-menuservclienteareaclientes01
#50859770P/
#Seguro coche URL. https://www.mutua.es/, FORD FOCUS 1596CC, Tercero Plus. 8315DYY 

#Nomina.Conceptos

class(txt)
txt <- pdf_text("C:/Users/IVD007/Downloads/Tu_informe_economico.pdf")
CLAVECONCEPTODEDUCCIONABONOS
RECIBOLIQ.DEHABERESPERIODODIASCOMPT.
txt <- (gsub("   ","|",unlist(pdf_text("C:/Users/IVD007/Downloads/202108NominaAyto.pdf") 
             %>% strsplit(split = "\n"))))
txt <- txt[ !(txt %in% "") ]

