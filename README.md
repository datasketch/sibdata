# sibdata

<!-- badges: start -->

<!-- badges: end -->

> **Paquete R para el procesamiento, análisis y visualización de datos de biodiversidad de Colombia**

`sibdata` es el motor de datos detrás de la plataforma [**Biodiversidad en Cifras**](https://cifras.biodiversidad.co) del Sistema de Información sobre Biodiversidad de Colombia (SIB Colombia) y el Instituto Alexander von Humboldt.

## 🎯 Propósito

Este paquete transforma datos científicos de biodiversidad en información accesible y visualizable para científicos, tomadores de decisiones, educadores y el público general. Facilita:

-   **Consultas analíticas** sobre 80,000+ especies colombianas
-   **Visualizaciones interactivas** (mapas, gráficos, tablas)
-   **Generación de datos estáticos** para sitios web
-   **Explorador Shiny** para análisis dinámico
-   **Acceso eficiente** a través de DuckDB

## 🏗️ Arquitectura del Sistema

El paquete es el componente central en un sistema de tres capas:

```         
┌─────────────────────────────────────────────────────────────┐
│  1. Scripts Python (GitLab)                                 │
│     Procesan datos de iNaturalist, GBIF, colecciones        │
│     Generan tablas TSV con cifras de biodiversidad          │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ↓
┌─────────────────────────────────────────────────────────────┐
│  2. Paquete R (sibdata) ← ESTE PAQUETE                      │
│     • Importa datos desde GitLab                            │
│     • Crea bases de datos (DuckDB, SQLite)                  │
│     • Genera archivos JSON para web                         │
│     • Proporciona API de consultas                          │
│     • Aplicación Shiny interactiva                          │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ↓
┌─────────────────────────────────────────────────────────────┐
│  3. Sitio Web Next.js                                       │
│     Lee JSON estáticos para máximo rendimiento              │
│     https://cifras.biodiversidad.co                         │
└─────────────────────────────────────────────────────────────┘
```

## 📦 Instalación

### Desde GitHub (recomendado)

``` r
# Instalar remotes si no lo tiene
install.packages("remotes")

# Instalar sibdata (instala automáticamente dependencias desde GitHub)
remotes::install_github("datasketch/sibdata")


install.packages("devtools")
# Para instalar con todas las dependencias opcionales (Suggests)
devtools::install_github("datasketch/sibdata", dependencies = TRUE)

# Docker
#docker build --build-arg GITHUB_PAT=tu_token_aqui -t sibdata .
```

> **Nota**: Algunos paquetes del ecosistema Datasketch ( `shinypanels`, `dsmods`, etc.) se instalan automáticamente desde GitHub.

### Dependencias principales

El paquete se instala con todas sus dependencias automáticamente:

-   `DBI`, `RSQLite`, `duckdb` - Gestión de bases de datos

-   `dplyr`, `tidyr` - Manipulación de datos

-   `jsonlite` - Generación de JSON

-   `leaflet` - Mapas interactivos

-   `highcharter` - Gráficos interactivos

-   `shiny`, `DT` - Aplicación web interactiva

### ⚠️ Requisito: Bases de Datos

**Importante:** Para que el paquete funcione correctamente, debes agregar manualmente los archivos de base de datos en el directorio `inst/db/`:

-   `sibdata.duckdb` (recomendado, formato DuckDB)
-   `sibdata.sqlite` (formato SQLite legacy)

Estos archivos **no están incluidos** en el repositorio de GitHub debido a su tamaño (\~200 MB). Debes obtenerlos por separado y colocarlos en:

```         
inst/db/
├── sibdata.duckdb
└── sibdata.sqlite
```

Sin estos archivos, las funciones del paquete que requieren conexión a la base de datos no funcionarán.

## 🚀 Inicio Rápido

### Cargar el paquete

``` r
library(sibdata)
```

### Consultas básicas

``` r
# Conectar a la base de datos
con <- get_app_connection(system.file("db/sibdata.duckdb", package = "sibdata"))

# Especies totales en Colombia
datos_colombia <- sibdata(
  region = "colombia",
  tipo = "especies",
  con = con
)

# Mamíferos en el Meta
mamiferos_meta <- sibdata(
  region = "meta",
  grupo = "mamiferos",
  tipo = "especies",
  con = con
)

# Especies amenazadas en Antioquia con subregiones
amenazadas_ant <- sibdata(
  region = "antioquia",
  tematica = "amenazadas",
  tipo = "especies",
  subregiones = TRUE,
  con = con
)

# Ver datos
head(amenazadas_ant)
```

### Visualizaciones

``` r
# Crear mapa coroplético
conmap <- gt_con()

mapa <- choropleth_map(
  data = amenazadas_ant,
  region = "antioquia",
  tipo = "especies",
  con = con,
  conmap = conmap
)

mapa  # Visualizar en RStudio Viewer

# Crear gráfico de barras
grafico <- create_bar_chart(
  data = datos_colombia,
  r = list(sel_tipo = "especies"),
  con = con
)

grafico
```

## 🔍 Explorador Interactivo (Shiny App)

El paquete incluye una aplicación Shiny completa para explorar los datos interactivamente:

``` r
# Ejecutar el explorador (versión 4 - más reciente)
shiny::runApp(
  system.file("org_sibhumboldt_sibdata_app4", package = "sibdata")
)
```

**Características del explorador:**

-   **Mapas coropléticos** de Colombia por departamento/municipio
-   **Gráficos dinámicos** (barras, tortas, treemaps)
-   **Tablas interactivas** con búsqueda y filtros
-   **Tarjetas informativas** con indicadores clave
-   **Descarga de datos** en Excel
-   **Filtros por**:
    -   Región (nacional, departamental, municipal)
    -   Grupo biológico (mamíferos, aves, plantas, etc.)
    -   Temática (amenazadas, CITES, endémicas, exóticas, migratorias)

## 📊 Funciones Principales

### Consulta de datos: `sibdata()`

La función principal para obtener datos de biodiversidad:

``` r
sibdata(
  region = "colombia",        # Región: colombia, antioquia, medellin, etc.
  tipo = "especies",          # "especies" o "registros"
  grupo = NULL,               # "mamiferos", "aves", "plantas", etc.
  tematica = NULL,            # "amenazadas", "cites", "endemicas", etc.
  indicador = NULL,           # Indicador específico
  cobertura = NULL,           # "marinas", "continentales"
  subregiones = FALSE,        # Incluir datos de subregiones
  with_parent = FALSE,        # Incluir región padre
  all_indicators = FALSE,     # Retornar todos los indicadores
  con = con                   # Conexión a base de datos
)
```

### Visualizaciones

``` r
# Mapas
choropleth_map(data, region, tipo, con, conmap)

# Gráficos
create_bar_chart(data, r, con)
create_pie_chart(data, r, con)
create_donut_chart(data, r, con)
create_treemap_chart(data, r, con)
```

### Utilidades

``` r
# Regiones disponibles
sib_available_regions(con)

# Grupos biológicos disponibles
get_grupos_biologicos(con)

# Información de una región
get_region_info("meta", con)

# Etiquetas amigables para indicadores
sib_merge_ind_label(c("especies_region_total", "registros_region_total"), con)
```

## 🗂️ Estructura del Paquete

```         
sibdata/
├── R/                          # Código R
│   ├── sibdata.R              # Función principal de consulta
│   ├── exp_*.R                # Módulos Shiny del explorador
│   ├── charts.R               # Funciones de visualización
│   ├── map.R                  # Mapas coropléticos
│   ├── queries.R              # Consultas a base de datos
│   └── utils.R                # Funciones auxiliares
│
├── inst/
│   ├── db/                    # Bases de datos
│   │   ├── sibdata.duckdb     # DuckDB (recomendado) ⚠️ Debe agregarse manualmente
│   │   └── sibdata.sqlite     # SQLite (legacy) ⚠️ Debe agregarse manualmente
│   │
│   ├── org_sibhumboldt_sibdata_app4/  # Explorador Shiny v4
│   │   ├── app4.R             # Aplicación principal
│   │   ├── db/                # DB local de la app
│   │   └── www/               # CSS e iconos
│   │
│   └── geo/                   # Datos geográficos
│
├── data-raw/                  # Scripts de procesamiento
│   ├── DATASET.R              # Sincronización desde GitLab
│   ├── get_data.R             # Datos de Google Sheets
│   └── validate_data.R        # Validaciones
│
├── scripts/                   # Generación de JSON
│   ├── 00_info_pages.R        # Páginas informativas
│   ├── 00_world.R             # Datos mundiales
│   ├── 01_colombia.R          # Datos de Colombia
│   ├── 02_departamentos_pais.R # Departamentos
│   ├── 03_municipios.R        # Municipios
│   └── 04_especial.R          # Regiones especiales
│
├── static/data/               # JSON generados
│   ├── home.json
│   ├── colombia/
│   ├── antioquia/
│   ├── meta/
│   └── ...
│
├── vignettes/                 # Documentación extendida
│   ├── guia-actualizacion-datos.Rmd
│   └── explorador-shiny-app.Rmd
│
└── man/                       # Documentación (generada)
```

## 📚 Documentación

### Vignettes (Guías Extensas)

``` r
# Ver vignettes disponibles
browseVignettes("sibdata")

# Guía de actualización de datos
vignette("guia-actualizacion-datos", package = "sibdata")

# Guía del explorador Shiny
vignette("explorador-shiny-app", package = "sibdata")
```

### Ayuda de funciones

``` r
# Ayuda de la función principal
?sibdata

# Ayuda de módulos Shiny
?exp_inputs_ui
?exp_viz_inputs_ui
?exp_species_table_ui

# Ayuda de visualizaciones
?choropleth_map
?create_bar_chart
```

## 🔧 Desarrollo y Trabajo Local

### Clonar el Repositorio

Para trabajar con el código fuente del paquete:

``` bash
# Clonar el repositorio
git clone https://github.com/datasketch/sibdata.git

# Navegar al directorio
cd sibdata
```

O desde R:

``` r
# Usar usethis para clonar
usethis::create_from_github("datasketch/sibdata")
```

### Configurar el Entorno de Desarrollo

1.  **Cargar el paquete en modo desarrollo:**

``` r
# Cargar el paquete desde el directorio local
devtools::load_all()

# O instalar en modo desarrollo
devtools::install()
```

2.  **Agregar las bases de datos:**

Los scripts requieren que los archivos de base de datos estén en `inst/db/`. Si aún no los tienes:

``` r
# Crear el directorio si no existe
dir.create("inst/db", recursive = TRUE)

# Luego, copia manualmente los archivos:
# - sibdata.duckdb
# - sibdata.sqlite
# 
# Asegúrate de que estén en:
# inst/db/sibdata.duckdb
# inst/db/sibdata.sqlite
```

**Estructura esperada:**

```         
sibdata/
├── inst/
│   └── db/
│       ├── sibdata.duckdb    # Base de datos principal (DuckDB)
│       └── sibdata.sqlite    # Base de datos alternativa (SQLite)
├── scripts/
│   ├── 00_info_pages.R
│   ├── 00_world.R
│   ├── 01_colombia.R
│   ├── 02_departamentos_pais.R
│   ├── 03_municipios.R
│   └── 04_especial.R
└── ...
```

### Ejecutar los Scripts de Generación de Datos

Una vez que tengas las bases de datos en `inst/db/`, puedes ejecutar los scripts desde cualquier directorio:

``` r
# Desde R, ejecuta los scripts en orden:
source("scripts/00_info_pages.R")
source("scripts/00_world.R")
source("scripts/01_colombia.R")
source("scripts/02_departamentos_pais.R")
source("scripts/03_municipios.R")
source("scripts/04_especial.R")
```

**Nota:** Los scripts están diseñados para funcionar desde cualquier ruta. Automáticamente detectan la raíz del proyecto buscando el archivo `DESCRIPTION`.

## 🔄 Actualización de Datos

Para actualizar los datos del paquete (administradores):

``` r
# 1. Sincronizar desde GitLab
source("data-raw/DATASET.R")

# 2. Obtener datos complementarios
source("data-raw/get_data.R")

# 3. Generar JSON para web
source("scripts/00_info_pages.R")
source("scripts/00_world.R")
source("scripts/01_colombia.R")
source("scripts/02_departamentos_pais.R")
source("scripts/03_municipios.R")
source("scripts/04_especial.R")
```

Ver la vignette "Guía de Actualización de Datos" para instrucciones detalladas.

## Datos Cubiertos

### Cobertura Geográfica

-   **Nacional**: Colombia
-   **Departamental**: 32 departamentos
-   **Municipal**: 1,122+ municipios
-   **Regiones especiales**:
    -   Región Amazonía
    -   Reserva Forestal La Planada
    -   Resguardo Indígena Pialapi Pueblo Viejo

### Grupos Biológicos (37 grupos)

Incluyendo: - Mamíferos, Aves, Reptiles, Anfibios, Peces - Plantas vasculares, Algas, Hongos, Líquenes - Invertebrados (insectos, arácnidos, moluscos, etc.) - Microorganismos (bacterias, archaea, virus, protozoos)

### Temáticas de Conservación

-   **Amenazadas** (CR, EN, VU, NT)
-   **CITES** (Apéndices I, II, III)
-   **Endémicas**
-   **Exóticas** (invasoras, trasplantadas)
-   **Migratorias**

### Estadísticas

-   **Especies**: \~80,000
-   **Registros biológicos**: \~20,000,000
-   **Última actualización**: Enero 2025

## 🧪 Testing

Este paquete aun no cuenta con tests consolidados, para referencia en el momento de incorporación se podrán verificar con:

``` r
# Ejecutar tests
devtools::test()

# Con coverage
covr::package_coverage()
```

## 🤝 Contribución

Este es un proyecto colaborativo entre:

-   [**Datasketch**](https://datasketch.co) - Desarrollo técnico
-   [**Instituto Humboldt**](https://humboldt.org.co) - Datos y contenido científico
-   [**SIB Colombia**](https://biodiversidad.co) - Plataforma y coordinación

### Reportar problemas

Si encuentra un bug o tiene una sugerencia:

1.  Verifique que no exista un [issue similar](https://github.com/datasketch/sibdata/issues)
2.  Cree un nuevo issue con:
    -   Descripción clara del problema
    -   Código reproducible
    -   Versión de R y del paquete
    -   Salida de `sessionInfo()`

## 📄 Licencia

Este proyecto está bajo licencia MIT. Ver archivo `LICENSE` para más detalles.

## 📞 Contacto

-   **Web**: <https://cifras.biodiversidad.co>
-   **GitHub**: <https://github.com/datasketch/sibdata>

## 🙏 Agradecimientos

Datos de biodiversidad proporcionados por: - Sistema de Información sobre Biodiversidad de Colombia (SIB Colombia) - Instituto de Investigación de Recursos Biológicos Alexander von Humboldt - Comunidad científica colombiana - Plataformas globales: GBIF, iNaturalist

------------------------------------------------------------------------

**Hecho con ❤️ en Colombia** 🇨🇴
