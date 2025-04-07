#ACTIVIDAD TRANSVERSAL 
#-------------------------------------
#cargar paquetes
install.packages("tidyverse")
library(tidyverse)

#Cargar archivo CSV salarios mujeres
print("2. Cargando el dataset desde un archivo CSV...")
ruta <- "C:/Users/bjuar/OneDrive/Documentos/WomenIn/salarios_mujeres.csv"

#cargar datos en un data frame
datos_a <- read_csv(ruta)

#Revisar el DataSet
print("3. Explorando el dataset cargado...")

#ver las primeras filas
print("Primeras 6 filas del dataset:")
head(datos_a)

#informacion general del dataset
print("Información general del dataset:")
str(datos_a)

#dimension del dataset
print("Dimensiones del dataset (filas x columnas):")
dim(datos_a)

#nombres de las columnas
print("Nombres de las columnas:")
names(datos_a)

#resumen estadistico del dataset
print("Resumen estadístico del dataset:")
summary(datos_a)

#Identificacion de problemas en los datos
print("4. Identificando problemas en los datos...")

#Contar valores nulos
total_nulos <- sum(is.na(datos_a))
print(paste("Total de valores nulos en el dataset:", total_nulos))

# Valores nulos por columna
print("Número de valores NA por columna:")
print(colSums(is.na(datos_a)))

# Identificar registros duplicados
duplicados <- datos_a %>% filter(duplicated(.))
print(paste("Número de registros duplicados:", nrow(duplicados)))

# Analisis estadisticos
print("5. Cálculo de estadísticas descriptivas...")

# Seleccionar variables de interés
variables <- datos_a %>% select(Edad, Salario)

# Calcular la media de la edad
media <- datos_a %>% summarise(Media_Edad = mean(Edad, na.rm = TRUE))
print(paste("Media de Edad:", media$Media_Edad))

# Calcular la mediana de la edad
mediana <- datos_a %>% summarise(Mediana_Edad = median(Edad, na.rm = TRUE))
print(paste("Mediana de Edad:", mediana$Mediana_Edad))

# Calcular la moda del salario
moda <- datos_a %>%
  count(Salario) %>%
  filter(n == max(n)) %>%
  pull(Salario)
print(paste("Moda de Salario:", moda))

# Filtrado de datos
print("6. Filtrando datos por edades de 25 y 30 años...")

datos_filtrados <- datos_a %>% filter(Edad %in% c(25, 30))

print("Datos filtrados con edades de 25 y 30 años:")
head(datos_filtrados)

# convertir Edad a numerico
datos_a <- datos_a %>%
  mutate(Edad = as.numeric(Edad))

str(datos_a$Edad)

# Visualización: histograma corregido
print("7. Creando un histograma de edades...")

ggplot(datos_a, aes(x = Edad)) +
  geom_histogram(fill = "lightpink", color = "blue", bins = 10) +
  labs(title = "Distribución de Edad", x = "Edad", y = "Frecuencia") +
  theme_minimal()

print("Grafico generado con exito.")

