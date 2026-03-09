# Shiny Emissions v3

Dashboard en **RStudio (Shiny + shinydashboard)** per analitzar l'impacte ambiental de dietes, comparar dues solucions (A i B), desglossar emissions per ingredient i origen, i incorporar l'efecte del transport.

## Objectiu del projecte

L'aplicacio permet:

1. Carregar dades ambientals, de formulacio de dietes i de transport.
2. Comparar dos escenaris (Solucio A i Solucio B) per etapa productiva.
3. Analitzar impactes ambientals per dieta, per origen geografic i per ingredient.
4. Reassignar origens d'ingredients per a simulacio d'escenaris.
5. Exportar resums per al seu us extern.

## Estructura del dashboard

El dashboard s'organitza en:

1. **Bloc superior de configuracio**
2. **Bloc avancat col-lapsable**
3. **Tabset principal** amb les analisis

### 1. Bloc superior de configuracio

Inclou tres passos:

1. **Importar arxius** (`.xlsx`):
	- Ambiental
	- Dietes
	- Transport
2. **Seleccio d'etapa** per comparar:
	- `stepA` (Solucio A)
	- `stepB` (Solucio B)
3. **Seleccio d'impactes** a visualitzar:
	- `climate_change`
	- `land_use`
	- `water_use`
	- `eutrophication_marine`
	- `acidification`
	- `particulate_matter`

### 2. Bloc avancat (col-lapsable)

Inclou:

1. **Reassignacio d'origens** per ingredient (nomes ingredients amb mes d'un origen disponible).
2. **Aplicar canvi** i **restablir overrides**.
3. **Descarrega de resum** en CSV (`resum_impactes_solucions_<data>.csv`).

## Dades d'entrada

El projecte fa servir 3 fitxers Excel:

1. **Ambiental**
2. **Dietes**
3. **Transport**

### Format esperat (minim)

#### 1. Ambiental

Columnes obligatories:

- `ingredient`
- `group`
- `origen`
- `default_origen`

Columnes d'impacte reconegudes (si existeixen):

- `climate_change`
- `land_use`
- `water_use`
- `eutrophication_marine`
- `acidification`
- `particulate_matter`

#### 2. Dietes

Columnes obligatories:

- `step`
- `ingredient`
- `diet`
- `prop`

#### 3. Transport

Columna obligatoria:

- `origen`

A mes, pot incloure columnes d'impacte per integrar l'efecte del transport per origen.

## Unitats d'impacte

Les unitats definides a l'app son:

- `climate_change`: `kg CO2 eq`
- `land_use`: `dimensionless (pt)`
- `water_use`: `m3 world eq`
- `eutrophication_marine`: `kg N eq`
- `acidification`: `mol H+ eq`
- `particulate_matter`: `disease incidence`

## Utilitat de cada tabPanel

### Visio general

Que mostra:

1. KPIs de recompte (`ingredients`, `origins`, `diets`, `steps`).
2. Llistat de dietes per a Solucio A i Solucio B.

Per a que serveix:

1. Validar rapidament que la carrega de dades es correcta.
2. Tenir una foto inicial de la dimensio de l'analisi.

### Composicio per dieta

Que mostra:

1. Grafic interactiu de composicio per a Solucio A.
2. Grafic interactiu de composicio per a Solucio B.

Per a que serveix:

1. Entendre proporcions d'ingredients per dieta.
2. Detectar canvis de formulacio entre escenaris.

### Impactes per dieta

Que mostra:

1. Barres comparatives A vs B per dieta.
2. Un grafic per cada impacte seleccionat.

Per a que serveix:

1. Comparar rendiment ambiental de les dues solucions.
2. Detectar en quines dietes apareix la diferencia mes gran.

### Contribucio per origen

Que mostra:

1. Barres apilades per origen per a Solucio A.
2. Barres apilades per origen per a Solucio B.
3. Una analisi per cada impacte seleccionat.

Per a que serveix:

1. Identificar paisos/origens mes contributors.
2. Avaluar la sensibilitat geografica de l'impacte.

### Top ingredients

Que mostra:

1. Top 5 ingredients per dieta i per impacte seleccionat.
2. Comparacio en paral-lel de Solucio A i Solucio B.

Per a que serveix:

1. Prioritzar ingredients objectiu per a reduccio d'emissions.
2. Trobar palanques de millora d'alt impacte.

### Mapa d'origens

Que mostra:

1. Mapa mundial d'origens d'ingredients per a Solucio A.
2. Mapa mundial d'origens d'ingredients per a Solucio B.

Per a que serveix:

1. Visualitzar la distribucio geografica de la cadena de subministrament.
2. Detectar concentracions d'origen i possibles riscos logistic-ambientals.

### Distribucio

Que mostra:

1. Boxplots A vs B per impacte seleccionat.
2. Dispersio de valors entre dietes (amb punts individuals).

Per a que serveix:

1. Analitzar variabilitat, no nomes mitjanes.
2. Detectar outliers o dietes amb comportament extrem.

### Diferencia A - B

Que mostra:

1. Diferencia directa d'impacte per dieta (`A - B`) per a cada impacte seleccionat.
2. Missatge de control quan `stepA == stepB`.

Per a que serveix:

1. Veure rapidament qui "guanya" per impacte.
2. Quantificar magnitud i signe de la diferencia.

### Desglossament Impacte

Que mostra:

1. Descomposicio de l'impacte en:
	- contribucio d'ingredients
	- contribucio de transport
2. Vista comparativa per a Solucio A i Solucio B.

Per a que serveix:

1. Entendre d'on venen les emissions.
2. Decidir si convé actuar en formulacio, origen o logistica.

### Verificacio d'Ingredients

Que mostra:

1. Ingredients presents en dietes pero absents a la base ambiental.
2. Missatge d'estat correcte quan no hi ha faltants.

Per a que serveix:

1. Control de qualitat de dades abans d'interpretar resultats.
2. Evitar conclusions esbiaixades per dades incompletes.

### Contribucio Total

Que mostra:

1. Taula editable de `kg_consum` per dieta/etapa.
2. Comparacio d'impacte total acumulat A vs B (barres apilades per etapa).

Per a que serveix:

1. Escalar l'impacte a una realitat de consum.
2. Simular escenaris productius amb diferents kg consumits.

### Petjada Ambiental

Estat actual:

1. Pestanya creada a UI, sense visualitzacions implementades a `server.R`.

Per a que serveix (previst):

1. Espai reservat per a un resum addicional de petjada ambiental.

## Flux recomanat d'us

1. Carregar els tres fitxers (`Ambiental`, `Dietes`, `Transport`).
2. Revisar `Visio general` i `Verificacio d'Ingredients`.
3. Seleccionar `stepA` i `stepB`.
4. Triar impactes rellevants.
5. Analitzar:
	- `Impactes per dieta`
	- `Contribucio per origen`
	- `Top ingredients`
	- `Desglossament Impacte`
6. Si cal, aplicar overrides d'origen i repetir la comparacio.
7. Ajustar `kg_consum` a `Contribucio Total` per a analisi acumulada.
8. Exportar el resum final.

## Estructura del projecte

- `global.R`: llibreries, constants globals, funcions de carrega/calc/plot.
- `ui.R`: estructura visual completa del dashboard.
- `server.R`: logica reactiva, validacions, calculs i renderitzat.
- `www/style.css`: estils personalitzats.
- `Dades/`: dades auxiliars (incloent coordenades i taules de paisos).

## Execucio

Des de RStudio, obrir el projecte `shiny_emissions_v3.Rproj` i executar l'app amb `Run App`.

