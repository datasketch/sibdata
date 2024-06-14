







- Mejorar selectize regions por lista país, departamento, terrotorios
- Paste_dash -> Hacer funcones que retorne los nombres de regions nombrados, y de grupos biológicos


# 2024-05

## DONE

- ¿Qué posibilidad habría de una mejor visualización de San Andrés en el topoJSON del mapa de Colombia, ya que se ve muy pequeño?
- Datos topoJSON para el mapa de Norte de Santander.
- Verificando los datos de los Municipios para la sección de Publicadores
- Actualizar los municipios de Santander.

## TODO

- Explorador




# 2024-04

## DONE

- Texto Migratorias
- Información de waffle para resguardo
- publicador para cada depto: poner también el número de observacines
- Mapa San Andrés
- Bogotá D.C. no hay datos de especies ni observaciones?
- Datos mapa bogota
- en los datos que actualizaste de (Boyacá, Nariño, Santander, Tolima) en la propiedad gallery solo hay 12 id cuando anteriormente habían 15

- Datos nuevos deptos: waffle y top municipios
- Mapa: Norte de Santander, San Andrés, Valle del Cauca, La Guajira
- Datos publicadores (n_tipos, cantidad por tipo publicador)
- No hay datos del departamento de Atlántico, arauca, risaralda y bogotá
- Waffle y Top Municipios para los nuevos Departamentos.
Unknown or uninitialised column: especies_exoticas_riesgo_invasion_estimadas. 
- Actualizar Pialapi 



## TODO

- publicador.json: region como arreglo

- correr world - con ranking



# Notes Data Validations


region table: make sure they are unique. Ej. risaralda was repeated
tabla: region_grupo no hay datos de Bogota... revisar que todos tengan datos
Que está "reserva-forestal-la-planada"

# 2023-05

Home: 
mapa ranking-biodiversidad-mundo. Referencias como tooltip
db-cifras-sib/ranking.tsv

Colombia:
- Colombia, Hongos, Especies amenazadas. Número de listado de especies menor al indicado en la cifra
- Ya estaba. Agregar Categoría cites_i_ii para Colombia




## TODO

Perfil Colombia:

- Especies exóticas_temática. No se está mostrando la lista de “Especies Exóticas con potencial de invasion observadas”

- Adicionalmente, se observan las especies repetidas. Debería mostrarse las especies de la tabla especie_tematica que apliquen en la región y en el caso de amenazadas nacional las que tengan en el campo slug_tematica los valores ‘amenazadas-nacional-vu’, ‘amenazadas-nacional-en’ o ‘amenazadas-nacional-cr’. Para las amenazadas global las especies que tengan en el campo slug_tematica los valores de ‘amenazadas-global-vu’, ‘amenazadas-global-cr’ o ‘amenazadas-global-en’


- Arreglar listado especies exóticas colombia.  En los grupos biológicos, en la categoría exóticas, el número de especies exóticas que se muestra no corresponde al total de la categoría, si no el valor de la subcategoría exóticas. jemplo: En Colombia se están mostrando 117 especies exóticas... deberían ser 180. Variable especies_exoticas_total. Animales Exóticas: El valor total de exóticas es de 76, se muestran 117 Se muestra: exoticas total2.718,   exoticas 1933, exoticas riesgo invasión 765, invasoras 20 el valor de exóticas total es de 1.869, en exóticas es 1.120, exóticas riesgo invasión es 732, invasoras es 17


- Amenenazas nacional: Agregar Estimadas con su referencia. X Especies CR observadas. YYY Especies estimadas (gris)





Perfil deptos:
- mostrar mapa por en deptos municipios
- En tolima: se siguen mostrando especies que no son cites
- revisar tematica_list_deptos para arreglar exóticas


Perfil municipios:
- Municipios, amenazadas: Se observan las mismas especies entre las categorías nacional y global. 
- Inconsistencia en la sección de grupos biológicos. No muestra especies de animales de tunja
- En la vista de los municipios hace falta incluir la comparación con el departamento.
- Municipios -> temática, compara el municipio con el departamento










# 2023-02

## DONE

- Actualizar datos 2023

- Halar datos para mapa en el home
- Regenerar glosario

- Referencia de estimadas para banners
icono foto y referencias por depto

Datos de publicadores para municipios
https://deploy-preview-1--cifras-biodiversidad.netlify.app/mas/publicadores?region=Carmen%20de%20Apical%C3%A1
publicadores municipios como arreglo en region

Estimadas vs observadas para deptos y municipios
Segunda vista amenazadas departamentos

- Revisar listas de especies (repetidas cites y exóticas, separar amenaza global y nacional, especies en cites que no son de la temática)

JSON
    - patrocinadores de cada perfil

## TODO

Repetidas exóticas
Título especies por departamento




- Revisar y ajustar json 
    - Datos municipios Santander y Boyacá

- Rehacer explorador
    - Revisar municipios, que todos tengan datos
    - Descargas de listas
    - Campos en español para la tabla
    - Orito en nariño




## Ajustes datos

- Falta información de créditos de fotos, no mostramos las que no tienen?
- Mostrar referencias con links a Zotero y otros lugares?






## Ajustes datos

inds_meta
marinas - marinos > marina?
continental > continentales?

especies_marinas -> especies_marinas_total?
especies_continentales -> especies_continentales_total 

tabla: grupo = grupo_bio + grupo_int

grupo -> rename to region_grupo_tematica?

Link a fotos









