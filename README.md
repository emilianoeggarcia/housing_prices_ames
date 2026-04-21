# Determinantes del Precio de la Vivienda: Un análisis econométrico de Ames, Iowa.
Este proyecto realiza un análisis exhaustivo de los factores que influyen en el valor de los inmuebles en Ames, Iowa (EE. UU.), utilizando la base de datos AmesHousing. El estudio aplica diversos modelos econométricos para descomponer el valor de una vivienda en función de sus atributos físicos, espaciales y cualitativos.
**Autores:** Emiliano García & Mariano Dubarry  
**Institución:** Facultad de Ciencias Económicas - Universidad Nacional del Litoral (UNL)  
**Fecha:** Octubre 2025

## Objetivos del Proyecto
* **Explorar y describir** las características estructurales y la distribución de las variables clave del mercado inmobiliario local.
* **Estimar modelos de regresión** lineales, logarítmicos y cuadráticos, para evaluar la magnitud y significancia de los determinantes del precio.
* **Aplicar modelos de probabilidad** Logit y Probit, para identificar la propensión de una vivienda a ubicarse en el segmento de precios altos (cuartil superior).

## Metodología y Tecnologías
El análisis se desarrolló íntegramente en **R**, empleando un enfoque econométrico por etapas:
* **Limpieza de Datos:** Ingeniería de variables incluyendo logaritmos, términos cuadráticos y cálculo de distancias geográficas mediante la fórmula de Haversine.
* **Modelado Estadístico:** Uso de Mínimos Cuadrados Ordinarios (MCO) con errores estándar robustos y Máxima Verosimilitud para modelos binarios.
* **Librerías Clave:** `dplyr`, `data.table`, `ggplot2`, `stargazer`, `margins` y `AmesHousing`.

## Hallazgos Principales
* **Calidad y Tamaño:** Son los factores dominantes. En el modelo log-log, un aumento del 1% en el tamaño se asocia con un incremento del 0,545% en el precio, mientras que cada punto extra de calidad eleva el valor en un 12,2%.
* **Sesgo de Variable Omitida:** El análisis inicial mostraba una relación positiva contraintuitiva entre la distancia al centro y el precio; al aplicar controles multivariados, se corrigió el sesgo, alineándose con la teoría económica urbana.
* **Probabilidad de Precio Alto:** Según los Efectos Marginales Promedio (AMEs), un punto adicional en calidad aumenta en un **8,67%** la probabilidad de que una vivienda pertenezca al segmento de lujo.

## Contenido del Repositorio
* **' housing_data.xlsx '**: Base de datos utilizada para el desarrollo del analisi
* **`Analisis viviendas - Monografia.R`**: Script completo con el procesamiento y análisis de datos.
* **`Monografia Econometria - Garcia y Dubarry.docx`**: Documento detallado con el marco teórico, metodología y conclusiones.
* **`Determinantes-del-Precio-de-la-Vivienda.pptx`**: Presentación ejecutiva de los resultados.
