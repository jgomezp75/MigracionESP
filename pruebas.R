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


nombre <- "datos_originales/19-Procesos_Masivos.csv"
fichero_masivos <- read.csv2(nombre, sep = "~", quote = "", fileEncoding = "UTF-8", stringsAsFactors = TRUE)
masivos_maestro <- tbl_df(fichero_masivos)

nombre <- "datos_originales/20-Procesos_Especialistas.csv"
fichero_especialistas <- read.csv2(nombre, sep = "~", quote = "", fileEncoding = "UTF-8", stringsAsFactors = TRUE)
especialistas_maestro <- tbl_df(fichero_especialistas)

nombre <- "datos_originales/21-Procesos_Becarios.csv"
fichero_becarios <- read.csv2(nombre, sep = "~", quote = "", fileEncoding = "UTF-8", stringsAsFactors = TRUE)
becarios_maestro <- tbl_df(fichero_becarios)

gestores_filtro <- c("Naiara Martínez", "Elena Herbosa", "Sara Abad", "Susana Gutierrez", "")
procesos_activos <- especialistas_maestro %>%
                        filter(ESTADO_PROCESO == "Abierto", GESTOR %in% gestores_filtro) %>%
                        select(ID_PROCESO)
masivos_filtro <- c("12841","12842")
procesos_activos <- masivos_maestro %>%
        filter(ACTIVO == "Sí", ID_PROCESO %in% masivos_filtro) %>%
        select(ID_PROCESO) %>%
        bind_rows(procesos_activos)
becarios_filtro <- c("Naiara Martínez", "Elena Herbosa", "Sara Abad", "Susana Gutierrez", "")
procesos_activos <- becarios_maestro %>%
        filter(ESTADO_PROCESO == "Abierto", GESTOR %in% becarios_filtro) %>%
        select(ID_PROCESO) %>%
        bind_rows(procesos_activos)


nombre <- "datos_originales/19-Procesos_Masivos.csv"
fichero_masivos <- read.csv2(nombre, sep = "~", quote = "", fileEncoding = "UTF-8", stringsAsFactors = TRUE)
masivos_maestro <- tbl_df(fichero_masivos)




fileNames <- list.files(path = "datos_originales", pattern="*.csv", full.names=T, recursive=FALSE)
fileNamesOut <- file.path("temp",list.files(path = "datos_originales", pattern="*.csv", recursive=FALSE))
for(x in 1:length(fileNames)) {
        t<-fread(fileNames[x], sep = "~", quote = "", encoding = "UTF-8", stringsAsFactors = TRUE, fill = TRUE, data.table = TRUE)
        print (fileNames[x])
        if ("ID_PROCESO" %in% colnames(t)) {
                t <- t %>% filter(ID_PROCESO %in% procesos_activos$ID_PROCESO)
        }
        else {
                if ("ID_CANDIDATO" %in% colnames(t)) {
                        t <- t %>% filter(ID_CANDIDATO %in% candidatos_maestro$ID_CANDIDATO)
                }
                else {
                        print ("FILTRO POR OFERTA")
                }
        }
        fwrite(t,fileNamesOut[x], sep = "~")
        rm(t)
        gc()
}



