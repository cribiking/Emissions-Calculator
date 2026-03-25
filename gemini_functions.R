################################################################################
# INTEGRACIÓN CON GOOGLE GEMINI PARA ANÁLISIS DE GRÁFICOS
# Funciones para enviar imágenes a Gemini y obtener explicaciones
################################################################################

# Librerías necesarias
library(httr)           # Para llamadas HTTP a la API de Gemini
library(jsonlite)       # Para procesar respuestas JSON
library(base64enc)      # Para codificar imágenes en base64
library(markdown)       # Para renderizar markdown en las explicaciones

cat("✅ gemini_functions.R cargado - Librerías disponibles\n")
