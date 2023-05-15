#### EXTRACCIÓN DE TWEETS DE LOS CANDIDATOS ####

## Este código busca extraer los últimos 3200 tweets máximo (límtie de la API)
# emitidos por los candidatos presidenciales latinoamericanos post 2010.
# Se utiliza el paquete "rtweet"

# Importo las credenciales de autentificación para la API
source(here::here("Code", "Twitter token.R"))

#Importo la base con las cuentas de los candidatos
base <- rio::import(here::here("Data", "TWITTER Base Candidatos LA.xlsx"))

# Necesito un vector con todas las cuentas a extraer, sin repetidos
extaccounts <- unique(base$Twitter_account)

# Para no alcanzar los límites de extracción, divido en paquetes de 30 cuentas.
# Luego extraigo utilizado rtweet, y exporto los datos
# La extracción debe realizarse con varias horas entre paquetes


# 1
ext1 <- extaccounts[1:30]
extract1 <- rtweet::get_timelines(user = ext1, n=Inf, retryonratelimit = TRUE)
rio::export(extract1, here::here("Data", "E_Twitter_1.csv"), format="csv")
# 2
ext2 <- extaccounts[61:90]
extract2 <- rtweet::get_timelines(user = ext2, n=Inf, retryonratelimit = TRUE)
rio::export(extract2, here::here("Data", "E_Twitter_2.csv"), format="csv")
# 3
ext3 <- extaccounts[91:120]
extract3 <- rtweet::get_timelines(user = ext3, n=Inf, retryonratelimit = TRUE)
rio::export(extract3, here::here("Data", "E_Twitter_3.csv"), format="csv")
# 4
ext4 <- extaccounts[121:150]
extract4 <- rtweet::get_timelines(user = ext4, n=Inf, retryonratelimit = TRUE)
rio::export(extract4, here::here("Data", "E_Twitter_4.csv"), format="csv")
# 5
ext5 <- extaccounts[151:180]
extract5 <- rtweet::get_timelines(user = ext5, n=Inf, retryonratelimit = TRUE)
rio::export(extract5, here::here("Data", "E_Twitter_5.csv"), format="csv")
# 6
ext6 <- extaccounts[181:210]
extract6 <- rtweet::get_timelines(user = ext6, n=Inf, retryonratelimit = TRUE)
rio::export(extract6, here::here("Data", "E_Twitter_6.csv"), format="csv")
# 7
ext7 <- extaccounts[211:240]
extract7 <- rtweet::get_timelines(user = ext7, n=Inf, retryonratelimit = TRUE)
rio::export(extract7, here::here("Data", "E_Twitter_7.csv"), format="csv")
# 8
ext8 <- extaccounts[241:270]
extract8 <- rtweet::get_timelines(user = ext8, n=Inf, retryonratelimit = TRUE)
rio::export(extract8, here::here("Data", "E_Twitter_8.csv"), format="csv")
# 9
ext9 <- extaccounts[271:300]
extract9 <- rtweet::get_timelines(user = ext9, n=Inf, retryonratelimit = TRUE)
rio::export(extract9, here::here("Data", "E_Twitter_9.csv"), format="csv")
# 10
ext10 <- extaccounts[301:length(extaccounts)]
extract10<- rtweet::get_timelines(user = ext10, n=Inf, retryonratelimit = TRUE)
rio::export(extract10, here::here("Data", "E_Twitter_10.csv"), format="csv")

# Juntos las extracciones en una sola base
library(dplyr)
BaseEx <- full_join(extract1, extract2)
BaseEx <- full_join(BaseEx, extract3)
BaseEx <- full_join(BaseEx, extract4)
BaseEx <- full_join(BaseEx, extract5)
BaseEx <- full_join(BaseEx, extract6)
BaseEx <- full_join(BaseEx, extract7)
BaseEx <- full_join(BaseEx, extract8)
BaseEx <- full_join(BaseEx, extract9)
BaseEx <- full_join(BaseEx, extract10)

# Agrego el nombre y el país de los candidatos propietarios de cada cuenta
candidatos <- cbind.data.frame(base$Candidate, base$Twitter_account)
names(candidatos) <- c("Name", "screen_name")
# Exporto los datos
BaseEx <- left_join(BaseEx, candidatos)

rio::export(BaseEx, here::here("Data", "Base Completa Twitter.csv"), format="csv")


