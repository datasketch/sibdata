

amenazadas_nacional
parent_cr/vu





# OK



# TODO


referencia estimada




Datos mapa home

```
[
    {
      slug: '1',
      label: 'Orquídeas y mariposas',
      description: 'Orquídeas y mariposas. \n\n Nisi exercitation fugiat sint consectetur Lorem minim excepteur consequat. Veniam qui eu excepteur culpa Lorem do nisi est irure aute Lorem id. Pariatur eiusmod quis tempor anim labore aliqua sit duis aute voluptate non consectetur dolore sit. ', //texto a mostrar al lado del mapa 
      countries: ['Colombia', 'Argentina'] // ciudades que están en el ranking
    },
    {

      slug: '2',
      label: 'Aves, plantas, anfibios, peces dulceacuicolas',
      description: 'Aves, plantas, anfibios, peces dulceacuicolas \n\n Officia ex non consectetur tempor dolor laboris commodo enim ut non.',
      countries: ['Ecuador', 'Colombia']
    },
    {

      slug: '3',
      label: 'Palmas y reptiles',
      description: 'Palmas y reptiles \n\n Officia ex non consectetur tempor dolor laboris           commodo enim ut non.',
      countries: ['Argentina']
    }
  ]
```

## 

Separa tabla de dato relevante de destacadas



##


Árbol de territorios?
Revisar árbol para mostrar las navegaciones que no tienen datos si no solo vínculos a otras partes

narino
 |- municipios
 |- areas-protegidas
 |- areas-marinas
 |- regiones-naturales
 |- 

colombia
 |- departamentos
 |- municipios
 |- ...




## OLD NOTES





primero sale el mapa del departamento
ver por registros o por especies


Temáticas:

distribución con tilde en el slug
Exóticas e invasoras no está.... como sí en distribución

agregar tabla de especies a Número especies exóticas con potencial de invasión

arbol_tematica: dejar solo EN, CR, VU



Tidify

select one region:
- compare with parent
- subregions

select region -> 
    tipo (registros or species) -> 
        cobertura (continental, marino, both)

Si no selecciona temática... se miran siempre los totales de registros

compare_button:
- with parent
- continental vs marino
- Amenazadas global vs nacional




- Compare for a given region   
    - continental vs marino? Si
    - Especies vs registros? Maybe

- Compare regions
    - parent vs child
    - two regions at the same level

- Compare grupo
    - 



# Notas tolima

Quieren lanzar el 12 de septiembre






# Ajustes datos

inds_meta
marinas - marinos > marina?
continental > continentales?

especies_marinas -> especies_marinas_total?
especies_continentales -> especies_continentales_total 

tabla: grupo = grupo_bio + grupo_int

grupo -> rename to region_grupo_tematica?


Link a fotos


# DONE


tematica
amenazadas
 - Global
 - Nacional
cites
endemicas
migratorias
exoticas
invasoras
exoticas_riesgo_invasion


Nombres filtros:
tipo registro -> "Tipo"
modo?


Separar vista de tabla de visualizaciones

Temáticas:
Todas: solo para la tabla


amenazadas -> o global o nacional
-> subcategorias


# TODO

ocultar salobres
boyacá, santander, tolima y todos




Ver cuáles parejas de variales tienen sentido:
Ej:
- Categoría amenza vs Global o nacional



Amenzadas:
alcance geográfico

cites: apendice
I II y III
I-II solo para colombia


Qué hacer con la información faltante:
Si es 0 se muestra, si es NA no se muestra



Alerta: Cuando vemos cobertura:
"Texto aclaratorio"


V2:
Marinas y continentales













