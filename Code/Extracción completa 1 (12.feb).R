
###########  CÓDIGO PARA EXTRACCIÓN DE DATOS DE GOOGLE TREND  ###########
######            Y CÁLCULO DEL ÍNDICE DE PERSONALISMO        ######
######                  SEGMENTADO MANUALMENTE                ######

# Importo Base
Base <- rio::import(here::here("Data", "Base Candidatos LA.xlsx"))

# Fijo intervalo temporal para la extaracción (180 días antes de la elección)
# Pero me aseguro que ninguna fecha sea anterior al período habilitado (>2003)
Base$Date_i <- as.Date(Base$Date_election,format="%d/%m/%Y")-180
inicio <- as.Date("2004-01-01 UTC")
Base$Date_i <-dplyr::if_else(Base$Date_i < inicio, as.Date("2004-01-01 UTC"), Base$Date_i)
Base$Date <- paste(as.Date(Base$Date_i,format="%d/%m/%Y"),
                   as.Date(Base$Date_election,format="%d/%m/%Y"))





# Filtro los candidatos independientes
Base <- Base[Base$Party != "Independiente",]

# Y me quedo con las variables necesarias para la extracción
Base_ext <- Base |> select(Candidate, Party, Date, GEO_code, Year)

# Defino una función, modificando gtrends acorde a la estructura de los datos
# De paso defino los argumentos constantes, category=396 significa "politics"

extractgoogle <- function(x1,x2,y,z){
        gtrendsR::gtrends(keyword = c(x1, x2), time=y, geo = z, 
                          category = 396, onlyInterest = T, gprop = "web")}

# Para no superar el límite de extracción, debo dividir la base en segmentos de 
# 90 pares candidatos-partidos cada uno
# Y cada segmento debo transformarlo en lista para poder utilizar pmap

ext1 <- Base_ext[1:90,]
ext1L <- with(ext1, list(Candidate, Party, Date, GEO_code))

# Realizo la extracción
E1 <- purrr::pmap(.l =ext1L , .f = extractgoogle)

Sys.sleep(30) # Agrego un tiempo de espera para evitar el riesgo de error en la extracción

# Esto se debe repetir otras 7 veces para cubrir la totalidad de los datos

# 2
ext2 <- Base_ext[91:180,]
ext2L <- with(ext2, list(Candidate, Party, Date, GEO_code))
E2 <- purrr::pmap(.l =ext2L , .f = extractgoogle)
Sys.sleep(30)
# 3
ext3 <- Base_ext[181:270,]
ext3L <- with(ext3, list(Candidate, Party, Date, GEO_code))
E3 <- purrr::pmap(.l =ext3L , .f = extractgoogle)
Sys.sleep(30)
# 4
ext4 <- Base_ext[271:360,]
ext4L <- with(ext4, list(Candidate, Party, Date, GEO_code))
E4 <- purrr::pmap(.l =ext4L , .f = extractgoogle)
Sys.sleep(30)
# 5
ext5 <- Base_ext[361:450,]
ext5L <- with(ext5, list(Candidate, Party, Date, GEO_code))
E5 <- purrr::pmap(.l =ext5L , .f = extractgoogle)
Sys.sleep(30) # Espero una hora, si no alcanza el límite
# 6
ext6 <- Base_ext[451:540,]
ext6L <- with(ext6, list(Candidate, Party, Date, GEO_code))
E6 <- purrr::pmap(.l =ext6L , .f = extractgoogle)
Sys.sleep(30)
# 7
ext7 <- Base_ext[541:630,]
ext7L <- with(ext7, list(Candidate, Party, Date, GEO_code))
E7 <- purrr::pmap(.l =ext7L , .f = extractgoogle)
Sys.sleep(30)
# 8
ext8 <- Base_ext[631:nrow(Base_ext),]
ext8L <- with(ext8, list(Candidate, Party, Date, GEO_code))
E8 <- purrr::pmap(.l =ext8L , .f = extractgoogle)

#trendecon::


# Realizo el cálculo sobre la extracción
E1p <- Personalism_index(Extraction=E1, Data=ext1)
E2p <- Personalism_index(E2, ext2)
E3p <- Personalism_index(E3, ext3)
E4p <- Personalism_index(E4, ext4)
E5p <- Personalism_index(E5, ext5)
E6p <- Personalism_index(E6, ext6)
E7p <- Personalism_index(E7, ext7)
E8p <- Personalism_index(E8, ext8)

# Junto las tablas

Index <- E1p |> full_join(E2p) |> full_join(E3p) |> 
                full_join(E4p) |> full_join(E5p) |> 
                full_join(E6p) |> full_join(E7p) |> 
                full_join(E8p)



