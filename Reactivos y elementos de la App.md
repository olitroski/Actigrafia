# Reactivos y elementos de la App

Separados por estructura



# Archivos













# Actograma











# Edición

## Sujetos y períodos





## Detalle del filtro



## Inicio de fin del registro



------

> **Tab - Filtrar períodos**
>
> Ya no se usa
>
> Seleccionar día, noche o ambos. A partir del input de tipo `per01 - martes 15/07/14` y el `acveditRDS()`.
>
> **Mostrar fechas del período**
>
> Cuando se selecciona un periodo `input$perChoose` se toma la fecha y calcula el mínimo y máximo del periodo y lo muestra.
>
> ```R
> verbatimTextOutput("selectedPer1")
> ```
>
> **Seleccionar día y noche**
>
> ```R
> checkboxGroupInput("dianoc")
>     input$dianoc
> ```
>
> **Botón para agregar el filtro**
>
> ```R
> actionButton("cambia_periodo")
> ```
>
> El botón usa como antecedente el reactivo `filterPeriod()` que es un largo `if` que toma el `input$perChoose` y dependiendo qué se seleccionó devuelve algo.

------



## Tab - Editar Actividad

**Los radio button con los episodios**

```R
uiOutput("editActUI")
    radioButtons("editAct.data")  => input$editAct.data
```

**Duración del episodio**

```R
# UI part
verbatimTextOutput("editActDur")
```

**Botón de aceptar**

```R
# UI part
actionButton("editAct.btn")

# Modal confirmar
input$editAct.mdl

```

**UI viejo**

```
p(strong("Se usa en 4 escenarios para rescatar datos:")),
p(code("AM-1"), "Se puso el actigrafo al despertar", code("[Inicio Vigilia]"), br(), 
code("AM-2"), "Se sacó el actígrafo al despertar", code("[Fin Sueño]")),
p(code("SetN"), "Mover la noche, existe un", code("[S]"), "largo inicio noche", code("[Inicio Sueño]")),
p(code("PM-1"), "Se sacó el actígrafo antes de dormir", code("[Fin Vigilia]"), br(),
code("PM-2"), "Se puso el actigrafo antes de dormir", code("[Inicio Sueño]"))
```



## Tab - Mover Período

Estos siempre se deben marcar

**Botones de selección**
Mover noche o día, 

```R
radioButtons("moveNightEscena") => "Mover Noche", "Mover Dia"
```

**IU output** disponibles
Para mostrar inicios de episodios disponibles. Lo dejo como mover noche no más.

```R
uiOutput("moveNightUI")
    radioButtons("moveNight.data")
```

**Duración del episodio**
Del seleccionado

```R
verbatimTextOutput("moveNight.Dur")
```

**Botón**

```R
actionButton("moveNight.btn")
```



## Tab - Excluir Segmento

**Finales de episodio disponibles**
UI output

```R
uiOutput("ExcludeUI")
    radioButtons("Exclude.data")
		input$Exclude.data
```

**Mostrar la información**

```R
# Tipo de episodio
verbatimTextOutput("Exclude.stInfo")
	output$Exclude.stInfo

# Duración del episodio
verbatimTextOutput("Exclude.durInfo")
	output$Exclude.durInfo
```

**Valores de filtro disponibles**
UI output. Valor de Id de filtro disponible + "Nuevo"

```R
uiOutput("ExcludeFiltros")
	radioButtons("Exclude.filterdata")
		input$Exclude.filterdata
```

**Valores de inicio y fin**

```R
verbatimTextOutput("Exclude.IniText")
actionButton("Exclude.IniSet")
	input$Exclude.IniSet
		warnModal.ExcludeIni    =>    actionButton("ExcludeIniOK")

verbatimTextOutput("Exclude.FinText")
actionButton("Exclude.FinSet")
```



# Episodios









# Informes





