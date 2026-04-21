pkgs <- c("AmesHousing", "mlbench", "data.table", "dplyr", "ggplot2", "skimr")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if(length(to_install)) install.packages(to_install)

library(AmesHousing)
library(mlbench)
library(data.table)
library(dplyr)
library(ggplot2)
library(skimr)
library(geosphere)
library (lmtest)
library (ltm)
library(corrplot)
library(stargazer)
library (carData)
library (car)
library (readxl)
library (zoo)
library (msm)
library (polycor)
library(sandwich) 
library(margins) 

# 1) Selección de dataset: Ames (preferido) o BostonHousing2 (alternativa)
# =====================================================================

## ---------------------------------------------------------------------
## 1.1 Opción A: Ames (más completa: precios, superficie, dormitorios, barrio, calidad)
## ---------------------------------------------------------------------
if(requireNamespace("AmesHousing", quietly = TRUE)){
  ## Cargar la base Ames (función make_ames crea un data.frame)
  ames_raw <- make_ames()
  data_raw <- as.data.table(ames_raw)
  dataset_used <- "Ames"
} else {
  ## Si Ames no está disponible, usar BostonHousing2
  data("BostonHousing2", package = "mlbench")
  data_raw <- as.data.table(BostonHousing2)
  dataset_used <- "BostonHousing2"
}

# =====================================================================
# 2) Selección y renombrado de variables útiles según dataset
# =====================================================================

if(dataset_used == "Ames"){
  data <- data_raw %>%
    rename(
      price = Sale_Price,
      size = Gr_Liv_Area,
      bedrooms = Bedroom_AbvGr,
      neighborhood = Neighborhood,
      quality = Overall_Qual,
      lot_area = Lot_Area,
      longitude = Longitude,
      latitude = Latitude
    )
}
# =====================================================================
# 3) Transformaciones: logs, cuadrados y variables auxiliares
# =====================================================================
## ---------------------------------------------------------------------
## 3.1 Nota: Ames Sale_Price está en USD, Boston medv en 1000s USD 
## ---------------------------------------------------------------------

data <- data %>%
  mutate(
    ## en niveles y en log (evitar log(0) añadiendo 1 si es necesario) Precio
    price_level = as.numeric (price),
    log_price = log(price_level + 1),
    
    ## Tamaño en niveles y log
    size_level = as.numeric(size),
    log_size = log(ifelse(is.na(size_level), 0, size_level) + 1),
    
    ## Cuadráticos 
    size_sq = (size_level)^2,
    log_size_sq = (log_size)^2,
    
    ## Bedrooms como factor si existe
    bedrooms_f = if("bedrooms" %in% names(data)) factor(bedrooms) else NA,
    
    ## Calidad como factor y numérica
    quality_num = if("quality" %in% names(data)) as.numeric(quality) else NA,
    quality_f = if("quality" %in% names(data)) factor(quality)
  )

# =====================================================================
# 4) Variable de distancia al centro 
# =====================================================================

if(all(c("longitude","latitude") %in% names(data))){
  
  ## Centro aproximado = media de long/lat
  center_lon <- mean(data$longitude, na.rm = TRUE)
  center_lat <- mean(data$latitude, na.rm = TRUE)
  
  # Creamos una matriz con el punto central
  # (distHaversine necesita que el centro sea una matriz o vector de 2 elementos)
  center_coords <- c(center_lon, center_lat)
  
  # Creamos una matriz con las coordenadas de todos los puntos
  all_coords <- data.frame(lon = data$longitude, lat = data$latitude)
  
  data <- data %>%
    mutate(
      # Calculamos la distancia Haversine (en metros) de cada punto al centro
      dist_center = distHaversine(all_coords, center_coords),
      
      # Convertimos a kilómetros
      dist_center_km = dist_center / 1000,
      
      log_dist_center_km = log(dist_center_km + 1),
      dist_center__km_sq = dist_center_km^2
    )
  
}
# =====================================================================
# 5) Filtrado y observaciones finales
# =====================================================================
## ---------------------------------------------------------------------
## 5.1. Quitar observaciones con price o size NA
## ---------------------------------------------------------------------
housing_data <- data %>% filter(!is.na(price_level) & !is.na(size_level))

# =====================================================================
# 6) Creamos la variable binaria 'remodelacion_binaria'
# =====================================================================
# Criterio:
# 0 si Year_Built = Year_Remod_Add (No hubo remodelación)
# 1 si Year_Built < Year_Remod_Add (Sí hubo remodelación)
# ----------------------------------------------------------------------

housing_data$remodelacion_binaria <- ifelse(
  housing_data$Year_Built < housing_data$Year_Remod_Add,
  1, 0)

housing_data$remodelacion_binaria <- as.factor(housing_data$remodelacion_binaria)

# Mostrar las primeras filas con las columnas relevantes para la verificación

print(head(housing_data[c("Year_Built", "Year_Remod_Add", "remodelacion_binaria")]))

# Contar cuántos 0s y 1s hay

print(table(housing_data$remodelacion_binaria))

# =====================================================================
# 7) Guardar la base limpia para adjuntar al trabajo
# =====================================================================
fwrite(housing_data, file = "housing_data.csv")
cat("Base limpia guardada como 'housing_data.csv' en el directorio de trabajo.\n")


# -------------------------------------------------------------------------------------------------------------------

# Iniciamos el analisis de los datos para la monografia

attach (housing_data)

# --- 1. Regresiones lineales simples ----------

regr1 <- lm (price_level ~ size_level)
coeftest(regr1, vcov = vcovHC(regr1, type = "HC1"))
## Positivo y significativo

regr2 <- lm (price_level ~ bedrooms_f)
coeftest(regr2, vcov = vcovHC(regr2, type = "HC1"))
## Positivo y significativo

regr3 <- lm (price_level ~ dist_center_km)
coeftest(regr3, vcov = vcovHC(regr3, type = "HC1"))
## Positivo y significativo

regr4 <- lm (price_level ~ Year_Built)
coeftest(regr4, vcov = vcovHC(regr4, type = "HC1"))
## Positivo y significativo

regr5 <- lm (price_level ~ quality_num)
coeftest(regr5, vcov = vcovHC(regr5, type = "HC1"))
## Positivo y significativo

# Creamos matriz de errores robustos
robustos_regr1 <- sqrt(diag(vcovHC(regr1, type = "HC1")))
robustos_regr2 <- sqrt(diag(vcovHC(regr2, type = "HC1")))
robustos_regr3 <- sqrt(diag(vcovHC(regr3, type = "HC1")))
robustos_regr4 <- sqrt(diag(vcovHC(regr4, type = "HC1")))
robustos_regr5 <- sqrt(diag(vcovHC(regr5, type = "HC1")))

# Exportamos con stargazer
stargazer(regr1, regr2, regr3, regr4, regr5, 
          se = list(robustos_regr1, robustos_regr2, robustos_regr3, robustos_regr4, robustos_regr5),
          type = "text")

# --- 2. Regresiones lineales multiples -------

regr6_m <- lm (price_level ~ size_level + bedrooms_f + dist_center_km + Year_Built + remodelacion_binaria)
coeftest(regr6_m, vcov = vcovHC(regr6_m, type = "HC1"))
## Todos significativos. Positivos el de size, year_built y remodelacion, negativos el de bedrooms y el de la distancia

regr7_m <- lm (price_level ~ size_level + bedrooms_f + dist_center_km + Year_Built + remodelacion_binaria + quality_num)
coeftest(regr7_m, vcov = vcovHC(regr7_m, type = "HC1"))
## Significativos todos salvo el de la distancia. Negativo el de bedrooms, el resto positivos.

regr8_m <- lm (quality_num ~ size_level + bedrooms_f + dist_center_km + Year_Built + remodelacion_binaria)
coeftest(regr8_m, vcov = vcovHC(regr8_m, type = "HC1"))
## Todos significativos, en magnitudes bajas. Negativos el de bedrooms y el de la distancia, positivo el resto

# Creamos matriz de errores robustos
robustos_regr6 <- sqrt(diag(vcovHC(regr6_m, type = "HC1")))
robustos_regr7 <- sqrt(diag(vcovHC(regr7_m, type = "HC1")))
robustos_regr8 <- sqrt(diag(vcovHC(regr8_m, type = "HC1")))

# Exportamos con stargazer
stargazer(regr6_m, regr7_m, regr8_m, 
          se = list(robustos_regr6, robustos_regr7, robustos_regr8),
          type = "text")

# --- 3. Regresiones logaritmicas --------

regr9_loglin <- lm (log_price ~ size_level)
coeftest(regr9_loglin, vcov = vcovHC(regr9_loglin, type = "HC1"))
## Positivo y significativo

regr10_loglog <- lm (log_price ~ log_size)
coeftest(regr10_loglog, vcov = vcovHC(regr10_loglog, type = "HC1"))
## Elasticidad precio-tamanio, es positivo y significativo

regr11_loglog_m <- lm (log_price ~ log_size + log_dist_center_km + quality_num + bedrooms_f + Year_Built + remodelacion_binaria)
coeftest(regr11_loglog_m, vcov = vcovHC(regr11_loglog_m, type = "HC1"))
## Todos los coeficientes significativos, salvo la elasticidad distancia. Negativos el de la cantidad de habitaciones y el de la distancia

## Creamos matriz de errores robustos
robustos_regr9 <- sqrt(diag(vcovHC(regr9_loglin, type = "HC1")))
robustos_regr10 <- sqrt(diag(vcovHC(regr10_loglog, type = "HC1")))
robustos_regr11 <- sqrt(diag(vcovHC(regr11_loglog_m, type = "HC1")))

## Exportamos con stargazer
stargazer(regr9_loglin, regr10_loglog, regr11_loglog_m, 
          se = list(robustos_regr9, robustos_regr10, robustos_regr11),
          type = "text")

# --- 4. Regresiones cuadraticas --------

regr12_cuad <- lm (price_level ~ size_level + size_sq)
coeftest(regr12_cuad, vcov = vcovHC(regr12_cuad, type = "HC1"))
## El primero positivo y significativo. El segundo negativo y no significativo

regr13_cuad <- lm (price_level ~ dist_center_km + dist_center_km_sq)
coeftest(regr13_cuad, vcov = vcovHC(regr13_cuad, type = "HC1"))
## Ambos significativos, el primero positivo y el segundo negativo

regr14_cuad_m <- lm (price_level ~ size_level + size_sq + dist_center_km + dist_center_km_sq + Year_Built + remodelacion_binaria)
coeftest(regr14_cuad_m, vcov = vcovHC(regr14_cuad_m, type = "HC1"))
## Todos significativos, salvo el cuadrado del tamanio. Ademas, los cuadraticos negativos y los lineales positivos

## Creamos matriz de errores robustos
robustos_regr12 <- sqrt(diag(vcovHC(regr12_cuad, type = "HC1")))
robustos_regr13 <- sqrt(diag(vcovHC(regr13_cuad, type = "HC1")))
robustos_regr14 <- sqrt(diag(vcovHC(regr14_cuad_m, type = "HC1")))

## Exportamos con stargazer
stargazer(regr12_cuad, regr13_cuad, regr14_cuad_m, 
          se = list(robustos_regr12, robustos_regr13, robustos_regr14),
          type = "text")

# --- 5. Modelos de probabilidades -------

## Convertimos la variable dependiente en binaria. Pasa a ser igual a 1 si el precio está en el cuartil superior, 0 en caso contrario

housing_data$high_price <- ifelse (housing_data$price_level > quantile(housing_data$price_level, 0.75), 1, 0)

## Modelo logit

regr15_logit <- glm (high_price ~ log_size + log_dist_center_km + quality_num + bedrooms_f + Year_Built + remodelacion_binaria,
                     data = housing_data,
                     family = binomial (link = "logit"))
coeftest(regr15_logit, vcov = vcovHC(regr15_logit, type = "HC1"))
### Todos significativos, salvo el del logaritmo de la distancia y la binaria de remodelacion. Positivos salvo el de bedrooms y el logaritmode la distancia

## Modelo probit

regr16_probit <- glm (high_price ~ log_size + log_dist_center_km + quality_num + bedrooms_f + Year_Built + remodelacion_binaria,  
                      data = housing_data,
                      family = binomial(link = "probit"))
coeftest(regr16_probit, vcov = vcovHC(regr16_probit, type = "HC1"))
### Todos significativos, salvo el del logaritmo de la distancia y la binaria de remodelacion (es al 10%). Positivos salvo el de bedrooms y el logaritmode la distancia

## Creamos matriz de errores robustos
robustos_regr15 <- sqrt(diag(vcovHC(regr15_logit, type = "HC1")))
robustos_regr16 <- sqrt(diag(vcovHC(regr16_probit, type = "HC1")))

## Exportamos con stargazer
stargazer(regr15_logit, regr16_probit,
          se = list(robustos_regr15, robustos_regr16),
          type = "text")

## Efectos marginales promedio de los modelos logit y probit

library(margins)

margins_logit <- margins (regr15_logit)
summary(margins_logit)

margins_probit <- margins (regr16_probit)
summary(margins_probit)


# --- 6. Graficos -----------------------

## Relación entre tamaño y precio

ggplot(housing_data, aes(x = size_level, y = price_level)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "Tamaño del inmueble", y = "Precio", title = "Relación entre tamaño y precio")

## Relación entre distancia al centro y precio
ggplot(housing_data, aes(x = dist_center_km, y = price_level)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "blue") +
  labs(x = "Distancia al centro (km)", y = "Precio", title = "Relación entre distancia al centro y precio")

## Relacion calidad y precio
ggplot(housing_data, aes(x = factor(quality_num), y = price_level)) +
  geom_boxplot(fill = "lightblue", outlier.colour = "red", outlier.alpha = 0.5) +
  labs(title = "Precio de la Vivienda según Calidad General",
       x = "Nivel de Calidad (Overall_Qual)",
       y = "Precio de Venta (USD)") +
  theme_minimal()

## Relacion tamanio y precio
ggplot(housing_data, aes(x = size_level, y = price_level)) +
  geom_point(alpha = 0.4) + # Puntos semitransparentes para ver densidad
  geom_smooth(method = "lm", col = "blue") + # Línea de regresión MCO
  labs(title = "Gráfico 1: Relación entre Precio y Tamaño de la Vivienda",
       x = "Superficie Habitable (pies cuadrados)",
       y = "Precio de Venta (USD)") +
  theme_minimal()

## Distribucion de los precios y del log_price
p1 <- ggplot(housing_data, aes(x = price_level)) + 
  geom_histogram(fill="red", alpha=0.7, bins=40) + labs(title="Distribución de Price (Niveles)")
p2 <- ggplot(housing_data, aes(x = log_price)) + 
  geom_histogram(fill="blue", alpha=0.7, bins=40) + labs(title="Distribución de log(Price)")

# install.packages("gridExtra")
gridExtra::grid.arrange(p1, p2, ncol = 2)





















