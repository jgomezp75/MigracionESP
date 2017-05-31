library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(scales)

isValidEmail <- function(x) {
        grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}


nombre <- "datos_originales/01-Candidatos_Datos_Personales.csv"
fichero_candidatos <- read.csv2(nombre, sep = "~", quote = "", fileEncoding = "UTF-8", stringsAsFactors = TRUE)
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

candidatos_maestro <- candidatos_maestro %>% 
        arrange(desc(dmy_hms(FECHA_ACTUALIZACION)), desc(ID_CANDIDATO)) %>%
        distinct(EMAIL,.keep_all = TRUE) %>%
        arrange(ID_CANDIDATO)

#agrupamos por email
emails_repetidos <- candidatos_maestro %>% group_by(EMAIL) %>%
        summarise(count = n()) %>% 
        arrange(desc(count)) %>% filter(count > 1)
print(emails_repetidos)

fechas <- select(candidatos_maestro,ID_CANDIDATO,FECHA_ACTUALIZACION)
fechas$FECHA_ACTUALIZACION <- as.POSIXct(date(dmy_hms(fechas$FECHA_ACTUALIZACION)))
ggplot(fechas, aes(x = FECHA_ACTUALIZACION, y = ..count..)) + 
        geom_histogram(aes(fill=..count..)) +
        labs(title="Histograma de Candidatos Actualizados (mensual)") +
        labs(x="Fecha", y="Número de Actualizaciones") + 
        scale_x_datetime(breaks = date_breaks("2 months"),
                         labels = date_format("%Y-%b"),
                         limits = c(as.POSIXct("2015-06-01"), 
                                    as.POSIXct(now()))
                          )


fechas <- select(candidatos_maestro,ID_CANDIDATO,FECHA_ALTA)
fechas$FECHA_ALTA <- as.POSIXct(date(dmy_hms(fechas$FECHA_ALTA)))
ggplot(fechas, aes(FECHA_ALTA)) + 
        geom_histogram(aes(fill=..count..)) +
        labs(title="Histograma de Candidatos dados de alta (mensual)") +
        labs(x="Fecha", y="Número de Actualizaciones") + 
        scale_x_datetime(breaks = date_breaks("2 months"),
                         labels = date_format("%Y-%b"),
                         limits = c(as.POSIXct("2015-06-01"), 
                                    as.POSIXct(now()))
        )


