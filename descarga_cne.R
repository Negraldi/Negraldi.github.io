# Cargamos las librerías necesarias
library(sf)         # Para manejo de datos espaciales
library(mapview)    # Para visualización interactiva de mapas
library(pbapply)    # Para aplicar funciones con barra de progreso
library(dplyr)      # Para manipulación de datos
library(jsonify)    # Para leer respuestas JSON

# Aumentamos el tiempo de espera para las conexiones
options(timeout = 1000)

#########################
# 1. Juntas electorales #
#########################

# URL del servicio de juntas electorales (FeatureServer/0)
url_juntas <- 'https://services6.arcgis.com/SGiLSdsy6CJtpjEM/ArcGIS/rest/services/Organizacion_Territorial/FeatureServer/0/query?%s'

# Descargamos todos los registros en formato pgeojson
consulta_juntas <- sprintf(url_juntas, URLencode('where=1<>0&outFields=*&f=pgeojson'))
juntas_electorales <- read_sf(consulta_juntas)



# Agrupamos las juntas por códigos territoriales y unimos sus geometrías,
# de modo que se obtenga una geometría única para cada zona electoral
zonas_electorales_juntas <- juntas_electorales[, c("CODIGO_PRO", "CODIGO_CAN", "CODIGO_PAR", "CODIGO_ZON")] %>%
  group_by(CODIGO_PRO, CODIGO_CAN, CODIGO_PAR, CODIGO_ZON) %>%
  summarise(geometry = suppressMessages(st_union(geometry)))

zonas_electorales_juntas=
zonas_electorales_juntas %>%
  rename(CODPRO=CODIGO_PRO,CODCAN=CODIGO_CAN,CODPAR=CODIGO_PAR,CODZON=CODIGO_ZON) %>%
  ungroup() %>%
  arrange(CODPRO,CODCAN,CODPAR,CODZON)

#######################################
# 2. Áreas de zonas electorales (layer 4) #
#######################################

# URL del servicio de zonas electorales (FeatureServer/4)
url_zonas <- 'https://services6.arcgis.com/SGiLSdsy6CJtpjEM/ArcGIS/rest/services/Organizacion_Territorial/FeatureServer/4/query?%s'

# Obtenemos el conteo total de registros de zonas electorales
consulta_conteo_zonas <- sprintf(url_zonas, URLencode('where=1<>0&returnCountOnly=true&f=pgeojson'))
conteo_zonas <- from_json(consulta_conteo_zonas)
num_zonas <- conteo_zonas$properties$count

# Se realizan consultas paginadas (de 100 registros) para descargar todos los datos.
# Notar que se usa 2*num_zonas para definir el rango de OBJECTID, según la estructura del servicio.
consulta_urls_zonas <- lapply(1:ceiling((2 * num_zonas) / 100), function(i) {
  parametros_consulta <- sprintf('where=OBJECTID<=%s AND OBJECTID>=%s&outFields=*&f=pgeojson',
                                 pmin(100 * i, 2 * num_zonas),
                                 (100 * (i - 1) + 1))
  sprintf(url_zonas, URLencode(parametros_consulta))
})

# Descargamos y combinamos los datos de zonas electorales
datos_zonas <- pblapply(consulta_urls_zonas, function(url) read_sf(url))
datos_zonas <- plyr::rbind.fill(datos_zonas)
datos_zonas <- st_as_sf(datos_zonas)
# Aseguramos que el objeto de geometría tenga la misma longitud que el número de filas
datos_zonas$geometry <- datos_zonas$geometry[1:nrow(datos_zonas)]

# Corrección de códigos de cantón:
# Para registros con CODPRO == 23, si el código de cantón es "980", se reemplaza por "982"
datos_zonas$CODCAN[datos_zonas$CODPRO == 23 & datos_zonas$CODCAN %in% c("980")] <- "982"

#########################################
# 3. Datos de parroquias (layer 3)       #
#########################################

# URL del servicio de parroquias (FeatureServer/3)
url_parroquias <- 'https://services6.arcgis.com/SGiLSdsy6CJtpjEM/ArcGIS/rest/services/Organizacion_Territorial/FeatureServer/3/query?%s'

# Obtenemos el conteo total de registros de parroquias
consulta_conteo_parroquias <- sprintf(url_parroquias, URLencode('where=1<>0&returnCountOnly=true&f=pgeojson'))
conteo_parroquias <- from_json(consulta_conteo_parroquias)
num_parroquias <- conteo_parroquias$properties$count

# Generamos las URLs para descargar los datos de parroquias en bloques de 100 registros
consulta_urls_parroquias <- lapply(1:ceiling((2 * num_parroquias) / 100), function(i) {
  parametros_consulta <- sprintf('where=OBJECTID<=%s AND OBJECTID>=%s&outFields=*&f=pgeojson',
                                 pmin(100 * i, 2 * num_parroquias),
                                 (100 * (i - 1) + 1))
  sprintf(url_parroquias, URLencode(parametros_consulta))
})

# Descargamos y combinamos los datos de parroquias
datos_parroquias <- pblapply(consulta_urls_parroquias, function(url) read_sf(url))
datos_parroquias <- plyr::rbind.fill(datos_parroquias)
datos_parroquias <- st_as_sf(datos_parroquias)
datos_parroquias$geometry <- datos_parroquias$geometry[1:nrow(datos_parroquias)]
datos_parroquias=
datos_parroquias %>%
  mutate(CODCAN=ifelse((CODPRO==13) & (CODCAN==25),485,CODCAN))
datos_parroquias$geometry <- datos_parroquias$geometry[1:nrow(datos_parroquias)]
datos_parroquias=st_cast(st_as_sf(data.frame(datos_parroquias)),'MULTIPOLYGON')

#########################################################
# 4. Asignación de zona electoral a parroquias sin zona #
#########################################################

# Extraemos combinaciones únicas de códigos territoriales de las juntas electorales
codigos_juntas <- unique(st_drop_geometry(zonas_electorales_juntas)[, 
                                                                    c("CODIGO_PRO", "CODIGO_CAN", "CODIGO_PAR", "CODIGO_ZON")])
# Filtramos para quedarnos solo con aquellas combinaciones donde la zona es 0,
# lo que indica que la parroquia no se subdivide en zonas electorales
codigos_juntas <- codigos_juntas %>% filter(CODIGO_ZON == 0)

# Actualizamos los datos de parroquias:
# - Creamos columnas con los códigos territoriales como enteros (a partir de CODPRO, CODCAN y CODPAR)
# - Realizamos un left join con los códigos de juntas para asignar la zona (CODIGO_ZON)
# - Creamos la columna final CODZON y eliminamos las columnas intermedias
datos_parroquias <- datos_parroquias %>%
  mutate(CODIGO_PRO = as.integer(CODPRO),
         CODIGO_CAN = as.integer(CODCAN),
         CODIGO_PAR = as.integer(CODPAR)) %>%
  left_join(codigos_juntas, by = c("CODIGO_PRO", "CODIGO_CAN", "CODIGO_PAR")) %>%
  mutate(CODZON = CODIGO_ZON) %>%
  select(-CODIGO_PRO, -CODIGO_CAN, -CODIGO_PAR, -CODIGO_ZON)

# Nota:
# - El primer bloque descarga las ubicaciones de las juntas electorales y las agrupa por zona.
# - El segundo bloque descarga las áreas de las zonas electorales (algunas parroquias se dividen en zona).
# - El tercer bloque obtiene las parroquias, que son la división administrativa del país.

saveRDS(
list(
  datos_parroquias=datos_parroquias,
  datos_zonas=datos_zonas,
  zonas_electorales_juntas=zonas_electorales_juntas,
  juntas_electorales=juntas_electorales
)
,'geo_cne.rds')

st_write(juntas_electorales,'geo_cne.gpkg',layer = 'juntas_electorales')
st_write(zonas_electorales_juntas,'geo_cne.gpkg',layer = 'zonas_electorales_juntas')
st_write(datos_zonas,'geo_cne.gpkg',layer = 'datos_zonas')
st_write(datos_parroquias,'geo_cne.gpkg',layer = 'datos_parroquias')
