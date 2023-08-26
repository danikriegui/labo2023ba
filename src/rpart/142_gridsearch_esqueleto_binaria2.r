# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$ksemillas <-c(106791, 300177, 654551, 989581, 892287)
semilla <- PARAM$ksemillas
#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
# que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_binaria, seed=semilla)
# crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_binaria", seed = semilla)

  # genero el modelo
  # quiero predecir clase_binaria a partir del resto
  modelo <- rpart("clase_binaria ~ .",
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con DOS columnas,
  #  llamadas "pos", y "neg"
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing  que es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "pos"] > 0.025,
      ifelse(clase_binaria == "pos", 117000, -3000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3

  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semilla, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  semilla
  ganancias <- mcmapply(ArbolEstimarGanancia,
    semilla, # paso el vector de semillas
    MoreArgs = list(param_basicos), # aqui paso el segundo parametro
    SIMPLIFY = FALSE,
    mc.cores = 5
  ) # se puede subir a 5 si posee Linux o Mac OS

  ganancia_promedio <- mean(unlist(ganancias))

  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
#setwd("~/buckets/b1/") # Establezco el Working Directory
setwd("~/grid_search/")
# cargo los datos

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]

# chequeo la cant de cada variable
dataset[clase_ternaria == "BAJA+1", .N]
dataset[clase_ternaria == "BAJA+2", .N]
dataset[clase_ternaria == "CONTINUA", .N]

# Modifico la columna clase_ternaria para que sea binaria y le cambio el nombre: 
setnames(dataset, old = "clase_ternaria", new = "clase_binaria")
dataset[clase_binaria == "BAJA+1", clase_binaria := "neg"]
dataset[clase_binaria == "CONTINUA", clase_binaria := "neg"]
dataset[clase_binaria == "BAJA+2", clase_binaria := "pos"]


# chequeo la cant de cada variable
dataset[clase_binaria == "neg", .N]
dataset[clase_binaria == "pos", .N]

setwd("~/buckets/b1")

# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = TRUE)
dir.create("./exp/HT2025/", showWarnings = TRUE)
archivo_salida <- "./exp/HT2025/gridsearch_bin_001.txt"

# Escribo los titulos al archivo donde van a quedar los resultados
# atencion que si ya existe el archivo, esta instruccion LO SOBREESCRIBE,
#  y lo que estaba antes se pierde
# la forma que no suceda lo anterior es con append=TRUE
cat(
  file = archivo_salida,
  sep = "",
  "cp", "\t",
  "max_depth", "\t",
  "min_split", "\t",
  "min_bucket", "\t",
  "ganancia_promedio", "\n"
)


# itero por los loops anidados para cada hiperparametro

for (vmax_depth in c(4, 6, 8, 10, 12, 14)) {
  for (vmin_split in c(1000, 800, 600, 400, 200, 100, 50, 20, 10)) {
    for (minbucket in c(500, 400, 300, 200, 50, 25, 10, 5)) {
      for (vcp in c(-1, -0.5, -0.3, -0.1)) {
        # aqui se debe poner el codigo que ejecuta el modelo
        #  y calcula la ganancia promedio
        #  para cada combinacion de hiperparametros
        #  y la escribe al archivo de salida
        #  el codigo debe ser similar al de 141_gridsearch_esqueleto.r
        #  pero con los cuatro hiperparametros
        #  y con los valores de los loops anidados
        #  y con el nombre del archivo de salida correcto
        #  y con el nombre de la carpeta correcto
        #  y con el nombre de la funcion correcto
        #  y con el nombre de la funcion correc
        # notar como se agrega
        
        # vminsplit  minima cantidad de registros en un nodo para hacer el split
        param_basicos <- list(
            "cp" = vcp, # complejidad minima
            "minsplit" = vmin_split,
            "minbucket" = minbucket, # minima cantidad de registros en una hoja
            "maxdepth" = vmax_depth
        ) 

        # Un solo llamado, con la semilla 17
        ganancia_promedio <- ArbolesMontecarlo(semilla, param_basicos)
        
        # escribo los resultados al archivo de salida
        cat(
            file = archivo_salida,
            append = TRUE,
            sep = "",
            vcp, "\t",
            vmax_depth, "\t",
            vmin_split, "\t",
            minbucket, "\t",
            ganancia_promedio, "\n"
        )
      }
    }
  }
}
