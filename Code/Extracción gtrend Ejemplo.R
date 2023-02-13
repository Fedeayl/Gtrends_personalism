library(dplyr)

# Entre 2004 y 2022, en América Latina se presentaron 656 candidatos presidenciales
# La idea es automatizar la extracción de datos, para que sea viable realizarla

# La base es una dataset, con la misma estructura del siguiente ejemplo
# La columna date define la ventana temporal para la extracción de datos. 
# ¿6 meses antes de la elección sería adecuado?

# Defino Data como base de ejemplo
Data <- rbind.data.frame(
             c("José Mujica","Frente Amplio", 2009, "2009-06-01 2009-11-29", "UY"),
             c("Luis Alberto Lacalle", "Partido Nacional", 2009, "2009-06-01 2009-11-29", "UY"),
             c("Mauricio Macri", "Cambiemos", 2015, "2015-04-24 2015-10-24", "AR"),
             c("Daniel Scioli", "Frente para la victoria", 2015, "2015-04-24 2015-10-24", "AR"))

# Asigno nombres
names(Data) <- c("candidate", "party", "year", "date", "geo") 

Data$year <- as.numeric(Data$year) # Paso a numérico (útil luego)

ext <- list(Data$candidate, Data$party, Data$date, Data$geo) # Transformo en lista, para pmap

# Defino una función, modificando gtrends acorde a la estructura de los datos
# De paso defino los argumentos constantes, category=396 significa "politics"

extractgoogle <- function(x1,x2,y,z){
                        gtrendsR::gtrends(keyword = c(x1, x2), time=y, geo = z, 
                           category = 396, onlyInterest = T, gprop = "web")}

# Realizo la extración, usando la base creada como argumentos variables (cada fila es una extracción)
EXCT <- purrr::pmap(.l =ext , .f = extractgoogle)

#### Cálculo sobre la extracción ####

# Convierto lista a dataframe
DF <- unlist(EXCT, recursive=FALSE)
DF <-  rlist::list.rbind(DF)

# Creo variable año para agrupar
DF$year <- as.numeric(format(DF$date,'%Y')) 

# Resumo por año y sumo hits
Sum <- doBy::summary_by(DF, hits~keyword+geo+year, FUN=sum, na.rm=T)

# Vuelvo a Data, para completar los datos

Data <- left_join(Data, Sum, by=c("candidate" = "keyword", "year", "geo"))
Data <- Data %>% rename (hits.candidate = hits.sum)

Data <- left_join(Data, Sum, by=c("party" = "keyword", "year", "geo"))
Data <- Data %>% rename (hits.party = hits.sum)

# Y calcvulo el indicador
Data$hits.total <- Data$hits.candidate + Data$hits.party
Data$indicador <-  Data$hits.candidate / Data$hits.total

print(Data)

