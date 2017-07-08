rm(t)
nombre <- "temp/03-Candidatos_Formacion_Parte2.csv"
t <- leerArchivo(nombre)
t$TITULO <- as.character(t$TITULO)
t$TITULO[t$TITULO==""] <- paste( as.character(t$NIVEL[t$TITULO==""]), as.character(t$AREA[t$TITULO==""]))
t$TITULO <- as.factor(t$TITULO)
