# ============================================================
#  Tarea 3 · CA-0204 Herramientas de Ciencia de Datos I
#  Alonso Alpízar Brenes · C20274
#  Funciones de los ejercicios 1, 2 y 3
# ============================================================

library(tidyverse)


# ------------------------------------------------------------
#  Ejercicio 1 · Alertas por ventana móvil
# ------------------------------------------------------------
#  alerta.movil(df, ventana, k.alerta)
#
#  Recibe:
#    df       data frame de transacciones (columnas id_tx y fecha)
#    ventana  número de días anteriores que definen lo "normal"
#    k.alerta cuántas desviaciones sobre la media activan la alerta
#
#  Devuelve:
#    data frame con fecha, n.tx, media, desv y alerta.
#    Los primeros `ventana` días quedan en NA (no se evalúan).
# ------------------------------------------------------------

alerta.movil <- function(df, ventana, k.alerta) {
  
  # 1. Transacciones por día
  diario <- df |>
    count(fecha, name = "n.tx") |>
    mutate(fecha = as.Date(fecha))
  
  # 2. Media y desviación de los `ventana` días anteriores
  n     <- nrow(diario)
  media <- rep(NA, n)
  desv  <- rep(NA, n)
  
  for (t in (ventana + 1):n) {
    previos  <- diario$n.tx[(t - ventana):(t - 1)]   # sin incluir el día t
    media[t] <- mean(previos)
    desv[t]  <- sd(previos)
  }
  
  diario$media <- media
  diario$desv  <- desv
  
  # 3. Alerta: el día supera media + k desviaciones
  diario$alerta <- diario$n.tx > diario$media + k.alerta * diario$desv
  
  diario
}