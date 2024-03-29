###############     FUNCIONES ###################     

### FUNCIÓN PARA EXTRACIÓN DE DATOS POLÍTICOS DE GOOGLE TRENDS ####
### EN PAREJAS DE CANDIDATO-PARTIDOS ###


extractgoogle <- function(x1,x2,y,z){
        gtrendsR::gtrends(keyword = c(x1, x2), time=y, geo = z, 
                          category = 396, onlyInterest = T, gprop = "web")}


# Es necesario que los datos estén estructurados en 4 variables: 
#       * Candidato y Partido como keywords (string)
#       * El período de extracción en el formato "YYYY-MM-DD YYYY-MM-DD" (string)
#       * Geo según la codificación de google "XX" (string)
# Category = 396 corresponde a "politics"
# Onlyinterest = T solo muestra las cifras, no los términos extraidos
# gprop = "web" refiere solamente a las búsquedas web (no YouTube, por ejemplo)


### FUNCIÓN PARA PASAR LISTA A DF ### 
# simplemente un resumen para no complicar el código


unlistGtrend <- function(x){
        
        require(rlist)
        require(dplyr)
        
        # Vuelvo a dataframe para operar más fácil
        x <- unlist(x, recursive = F)
        x <- rlist::list.rbind(x)
        x$hits <- as.numeric(x$hits)
        
        # Creo variable Year para agrupar
        x$year <- as.numeric(format(x$date,'%Y')) 
        
        
        # Arreglo el problema cuando hay cambios en el año de extracción para la misma elección
        # Muy poco elegante solución
        
        x$year <- ifelse(x$geo == "CO" & x$year == 2021, 2022, x$year)
        x$year <- ifelse(x$geo == "CO" & x$year == 2017, 2018, x$year)
        x$year <- ifelse(x$geo == "CO" & x$year == 2013, 2014, x$year)
        x$year <- ifelse(x$geo == "CO" & x$year == 2009, 2010, x$year)
        x$year <- ifelse(x$geo == "CO" & x$year == 2005, 2006, x$year)
        
        x$year <- ifelse(x$geo == "CR" & x$year == 2021, 2022, x$year)
        x$year <- ifelse(x$geo == "CR" & x$year == 2017, 2018, x$year)
        x$year <- ifelse(x$geo == "CR" & x$year == 2013, 2014, x$year)
        x$year <- ifelse(x$geo == "CR" & x$year == 2009, 2010, x$year)
        x$year <- ifelse(x$geo == "CR" & x$year == 2005, 2006, x$year)      
        
        x$year <- ifelse(x$geo == "DO" & x$year == 2011, 2012, x$year) 
        
        x$year <- ifelse(x$geo == "EC" & x$year == 2020, 2021, x$year) 
        x$year <- ifelse(x$geo == "EC" & x$year == 2016, 2017, x$year) 
        x$year <- ifelse(x$geo == "EC" & x$year == 2012, 2013, x$year) 
        x$year <- ifelse(x$geo == "EC" & x$year == 2008, 2009, x$year)
        
        x$year <- ifelse(x$geo == "SV" & x$year == 2018, 2019, x$year)
        x$year <- ifelse(x$geo == "SV" & x$year == 2013, 2014, x$year)
        x$year <- ifelse(x$geo == "SV" & x$year == 2008, 2009, x$year)
        
        x$year <- ifelse(x$geo == "PA" & x$year == 2018, 2019, x$year)
        x$year <- ifelse(x$geo == "PA" & x$year == 2013, 2014, x$year)
        x$year <- ifelse(x$geo == "PA" & x$year == 2008, 2009, x$year)
        
        x$year <- ifelse(x$geo == "PY" & x$year == 2017, 2018, x$year)
        x$year <- ifelse(x$geo == "PY" & x$year == 2012, 2013, x$year)
        x$year <- ifelse(x$geo == "PY" & x$year == 2007, 2008, x$year)
        
        x$year <- ifelse(x$geo == "PE" & x$year == 2020, 2021, x$year)
        x$year <- ifelse(x$geo == "PE" & x$year == 2015, 2016, x$year)
        x$year <- ifelse(x$geo == "PE" & x$year == 2010, 2011, x$year)
        x$year <- ifelse(x$geo == "PE" & x$year == 2005, 2006, x$year)
        
        x$year <- ifelse(x$geo == "VE" & x$year == 2017, 2018, x$year)
        #x$year <- ifelse(x$geo == "VE" & x$year == 2012, 2013, x$year)
        
        return(x)
        
}



### FUNCIÓN PARA ARMADO DE BASE FINAL ###
# La intención aquí es crear la función que, a partir de los datos extraidos, 
# los agrega y calcula el índice de personalismo.

#### Cálculo sobre la extracción ####


Personalism_index <- function(Extraction, Data){
        
        require(rlist)
        require(dplyr)
        require(doBy)
        
        x <- Extraction
        y <- Data
        
        stopifnot(is.list(x)) # la entrada debe ser tipo lista
        
        needednames <- c("Candidate","Party") # Nombres de Date
        
        if (sum(names(y) %in% needednames) < 2) {
                stop("Data debe contener las siguientes variables: 
                     Candidate, Party")
        }
        
        
        # Paso a dataframe para poder operar 
        x <- unlistGtrend(x)

        # Resumo por año y sumo hits
        Sum <- doBy::summary_by(x, hits~keyword+geo+year, FUN=sum, na.rm=T)
        
        # Voy a Data, para agregar las nuevas columnas con la extracción resumida
        y <- left_join(y, Sum, by=c("Candidate"="keyword", "Year"="year", "GEO_code"="geo"))
        y <- y %>% rename (hits.candidate = hits.sum)
        
        y <- left_join(y, Sum, by=c("Party" = "keyword", "Year"="year", "GEO_code"="geo"))
        y <- y %>% rename (hits.party = hits.sum)
        
        # Y calcvulo el indicador
        y$indicador <-  y$hits.candidate / (y$hits.candidate + y$hits.party)
        
        return(y)
}





