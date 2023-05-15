#### Prueba de extracciones 
#### Términos exactos vs aproximados ####

# Importo la base
Data <- read.csv("https://raw.githubusercontent.com/Fedeayl/Gtrends_personalism/main/Data/Base_comparacion_exactvsaprox.csv")

# En la base está computado el índice a nivel de sistema para cada forma de extracción.
# El valor corresponde al promedio de los obtenidos en 5 extracciones para cada forma. 

# Veo la base
print(Data)

# Para El Salvador, los valores del índice son en 3 de 4 elecciones mayores utilizando
# los términos aproximados. 
Data[Data$Country == "EL SALVADOR",]

# Un gráfico 
with(Data, plot(x=PI_Exc, y=PI_Alg, main="Exact vs Aprox", 
                xlab="Exact term ", ylab="Aprox terms", pch=19, xlim = c(0, 1), ylim=c(0,1)))
abline(a = 0, b=1)

# Hay algunas observaciones por debajo de la linea y=x
sum(Data$PI_Exc>Data$PI_Alg) # 12 casos
# Para las demás, el valor del índice calculado con los términos aproximados es mayor. 

# En general, hay variaciones importantes 
# El coeficiente de correlación es de 0.46
with(Data,cor(PI_Exc, PI_Alg, use = "complete.obs"))

