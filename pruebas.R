library(dplyr)
library(data.table)
library(lubridate)

isValidEmail <- function(x) {
        grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}


nombre <- "datos_originales/01-Candidatos_Datos_Personales.csv"
#fichero_candidatos <- read.csv2(nombre, sep = "~", quote = "", fileEncoding = "UTF-8", stringsAsFactors = TRUE)
candidatos_maestro <- tbl_df(fichero_candidatos)
candidatos_maestro <- filter(candidatos_maestro, NOMBRE != "XXXXXX")
candidatos_maestro <- filter(candidatos_maestro, NOMBRE != "")
candidatos_maestro <- filter(candidatos_maestro, EMAIL != "")
emails_incorectos <- filter(candidatos_maestro,!isValidEmail(EMAIL))
candidatos_maestro <- filter(candidatos_maestro, isValidEmail(EMAIL))
print("Estos son los emails inválidos que hemos eliminado")
print(emails_incorectos$EMAIL)
candidatos_maestro <- filter(candidatos_maestro, ESTADO != "Incorporado")
candidatos_maestro <- filter(candidatos_maestro, !(EMAIL %like% "@infojobs"))
candidatos_maestro <- filter(candidatos_maestro, !(EMAIL %like% "@infoempleo"))
candidatos_maestro <- filter(candidatos_maestro, !(EMAIL %like% "@vacio.com"))

#agrupamos por email
emails_repetidos <- candidatos_maestro %>% group_by(EMAIL) %>%
        summarise(count = n()) %>% 
        arrange(desc(count)) %>% filter(count > 1)
print(emails_repetidos)

emails_fechamax <- candidatos_maestro %>%
        filter(EMAIL %in% emails_repetidos$EMAIL ) %>%
        group_by(EMAIL) %>%
        summarise(max_fecha = max(dmy_hms(FECHA_ACTUALIZACION))) 
print(emails_fechamax)



