# OK


# TODO


Nerith... dónde están las especies amenazadas estimadas? 

json vista temáticas

json grupos biológico

datos municipios

Agregar regiones a publicadores


datos para todos los gráficos
svgs de los mapas



```
 {
        "slug": "exoticas-invasoras",
        "activa": true,
        "descripcion": "Se incluyen especies catalogadas como exóticas, exóticas con alto riesgo de invasión, invasoras catalogadas por el Registro mundial de especies introducidas e invasoras para Colombia y el Ministerio de Ambiente y Desarrollo Sostenible.",
        "icon": false,
        "icon_black": "static/icons/exoticas-invasoras-black.svg",
        "icon_white": "static/icons/exoticas-invasoras-white.svg",
        "label": "Exóticas",
        "children": [
          {
            "slug": "exoticas",
            "activa": true,
            "descripcion": "Las especies exóticas son aquellas que no son nativas de un país o una región a la que llegaron de manera intencional o accidental, generalmente como resultado de actividades humanas.",
            "icon": true,
            "icon_black": "static/icons/exoticas-black.svg",
            "icon_white": "static/icons/exoticas-white.svg",
            "label": "Exóticas",
            "tooltip": "Exóticas GRIIS (Registro mundial de especies introducidas e invasoras)"
          },
          {
            "slug": "invasoras",
            "activa": true,
            "descripcion": "Organismos que se adaptan a regiones fuera de su distribución natural, causando daños al ecosistema, las especies nativas, salud o economía de la región.",
            "icon": false,
            "icon_black": "static/icons/invasoras-black.svg",
            "icon_white": "static/icons/invasoras-white.svg",
            "label": "Invasoras",
            "tooltip": "Invasoras (resolución 848 de 2008 del Ministerio de Ambiente y Desarrollo Sostenible)"
          },
          {
            "slug": "exotica-riesgo-invasion",
            "activa": true,
            "descripcion": "Una especie exótica con alto riesgo de invasión, es una especie con alta probabilidad de establecerse en ecosistemas o hábitats naturales o seminaturales, es un agente de cambio y amenaza la diversidad biológica nativa.",
            "icon": false,
            "icon_black": "static/icons/exotica-riesgo-invasion-black.svg",
            "icon_white": "static/icons/exotica-riesgo-invasion-white.svg",
            "label": "Especies exóticas con alto riesgo de invasión",
            "tooltip": "Invasoras GRIIS (Registro mundial de especies introducidas e invasoras)"
          }
        ]
      },

```




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

Dónde están los datos de municipios -> available regions

En destacados agregar el link a la imagen correspondiente

Separa tabla de dato relevante de destacadas

Agregar en table region_grupo, el tipo


## 

Revisar árbol para mostrar las navegaciones que no tienen datos si no solo vínculos a otras partes

Publicadores? Metodología?


Cómo resumir el listado de especies en grupos:

Podemos dejar solo el nombre científico (species) y el número de registros (registros). Pero si la idea es incluir un top 10, el resto de la información sería super útil para mostrar no solo la lista, sino un detalle del número de ordenes, familias o géneros que componen esa lista de especies y luego si el top con más registros.







##

¿Dónde están las especies estimadas por departamento?


Árbol de territorios?

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


