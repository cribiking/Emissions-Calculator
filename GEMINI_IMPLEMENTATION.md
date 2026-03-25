# 🤖 Integración Gemini API - Análisis Automático de Gráficos

## ✅ Implementación Completada

Se ha completado la integración de **Google Gemini API** para analizar automáticamente los gráficos de la aplicación Shiny. Ahora cada panel tiene un botón **"Explicar amb IA"** que:

1. **Captura los gráficos** del panel
2. **Crea un collage** combinado
3. **Envía a Gemini** para análisis automático
4. **Muestra los resultados** formateados en markdown

---

## 🚀 Cómo Usar

### En la Aplicación Shiny
1. Carga tu archivo de datos
2. Navega a cualquier pestaña (Composición, Impactes, Origen, etc.)
3. Haz clic en el botón **"Explicar amb IA"** 🤖
4. Espera mientras Gemini analiza los gráficos
5. Lee el análisis generado automáticamente

---

## 🔑 Configuración de API Key

### ✅ Opción Recomendada: Archivo `.env`

1. **Archivo `.env` está creado** en la raíz del proyecto
2. **Contiene**:
   ```
   
   ```
3. **Proteción**: El archivo `.env` está en `.gitignore` y NO se versionará en Git

### 🆕 Como usuario nuevo:

1. Copia `.env.example` a `.env`
2. Reemplaza `tu_clave_api_aqui` con tu propia clave
3. El archivo se cargará automáticamente al iniciar la app

### Alternativa: Variable de Entorno del Sistema

Si prefieres no usar `.env`, configura la variable de entorno:

```bash
# En Windows (PowerShell)
$env:GEMINI_API_KEY = "tu_clave_api"

# En bash/Linux/Mac
export GEMINI_API_KEY="tu_clave_api"
```

### Cómo obtener tu API Key:

1. Ve a [Google Cloud Console](https://console.cloud.google.com/apis)
2. Crea un nuevo proyecto o selecciona uno existente
3. Habilita la API de Generative Language
4. Ve a "Credenciales" y crea una clave de API
5. Copia la clave y pégala en el archivo `.env`

---

## 📋 Cambios Realizados

### 1. **gemini_functions.R**
- ✅ Integración completa con Gemini API v1beta
- ✅ Función `preparar_grafics_per_gemini()` para crear collages y enviar a Gemini
- ✅ Manejo robusto de errores

### 2. **server.R**
- ✅ Agregada librería `ggpubr` para combinar gráficos
- ✅ Creados 9 `observeEvent` handlers para cada tipo de análisis:
  - Composición, Impactes, Origen, Top Ingredients, Mapas, Distribución, Diferencias, Desglossament, Contribución Total
- ✅ Corregido bug: Ahora usa lista de plots reactivos en lugar de intentar llamar a renderPlotly()

### 3. **test_gemini.R** (Nuevo)
- Script de prueba para verificar que todo funciona

---

## 📝 Ejemplo de Respuesta Esperada

Cuando hagas clic en "Explicar amb IA", la respuesta de Gemini podría ser similar a:

```
Este gráfico muestra la composición de dos dietas diferentes (Solución A y B).
Los resultados indican que:

1. **Diferencia Clave**: La Solución B reduce significativamente el consumo
   de carne roja (20% menos) en comparación con A.

2. **Impacto**: Esta reducción podría disminuir las emisiones de CO2 en ~15%.

3. **Alternativas**: Se recomienda aumentar el porcentaje de proteína vegetal
   para mejores resultados de sostenibilidad.
```

---

## 🛠️ Troubleshooting

### "Error: resposta no és character"
- Verifica que la API key sea válida
- Comprueba tu conexión a internet
- Revisa la consola para errores HTTP

### "No hi ha gràfics disponibles"
- Asegúrate de haber cargado datos primero
- Verifica que hayas seleccionado métricas de impacto
- Comprueba que los datos sean válidos

### "Error API Gemini: 401"
- La API key es inválida o ha expirado
- Genera una nueva clave en Google Cloud Console

---

## 📱 Paneles con Explicación IA

| Panel | Botón | Input |
|-------|-------|-------|
| 📊 Composición | `ai_explain_comp` | `llista_plots_composicio()` |
| 📈 Impactes | `ai_explain_impactes` | `llista_plots_comparatius()` |
| 🌍 Origen | `ai_explain_origen` | `llista_plots_origen()` |
| ⭐ Top Ingredients | `ai_explain_top` | `llista_plots_top()` |
| 🗺️ Mapas | `ai_explain_mapes` | `llista_plots_mapes()` |
| 📦 Distribució | `ai_explain_distribucio` | `llista_plots_distribucio()` |
| 📊 Diferencia A-B | `ai_explain_diff` | `llista_plots_diff()` |
| 🎯 Desglossament | `ai_explain_desglossament` | `llista_plots_desglossament()` |
| 🐄 Contribució Total | `ai_explain_contrib_total` | `llista_plots_contrib_total()` |

---

## 🔄 Próximos Pasos (Opcional)

Para mejorar la implementación en el futuro:

- [ ] Mover API key a archivo `.env` para mayor seguridad
- [ ] Implementar caché para evitar llamadas repetidas
- [ ] Agregar rate limiting para no exceder cuotas de API
- [ ] Permitir usuario configurar idioma de respuestas
- [ ] Agregar opción de descarga de análisis en PDF

---

**Estado**: ✅ Completado y listo para usar
**Última actualización**: 2026-03-25

