# =========================================================
# TAREA IV
# Creada: Juan Pablo Maldonado
# =========================================================

library(tidyverse)
library(fixest)
library(gmm)

# ---------------------------------------------------------
# Preparar base general construye el frame y filtra la base
# ---------------------------------------------------------
df0 <- df %>%
  mutate(
    Pole = as.factor(Pole),
    gender = as.factor(gender),
    religion = as.factor(religion),
    caste = as.factor(caste),
    birthplace = as.factor(birthplace)
  ) %>%
  filter(
    !is.na(total_expenditure),
    !is.na(treat),
    !is.na(distance),
    !is.na(gender),
    !is.na(age),
    !is.na(religion),
    !is.na(caste),
    !is.na(birthplace),
    !is.na(Pole)
  )

# =========================================================
# En esta parte genero la variable forcing con los mismos criterios para probar resultados
# pero esto no es necesario porque la variable ya se encuentra lista en la base de datos que les dimos
# Parte de la generación de las primeras etapas, reduce form y 2sls simple
# =========================================================
df_iv <- df0 %>%
  filter((distance >= 20 & distance <= 35) | (distance >= 45 & distance <= 60)) %>%
  mutate(forcing = ifelse(distance >= 20 & distance <= 35, 1, 0))

fs_forcing <- feols(
  treat ~ forcing + gender + age + religion + caste + birthplace | Pole,
  data = df_iv
)

fs_distance <- feols(
  treat ~ distance + gender + age + religion + caste + birthplace | Pole,
  data = df_iv
)

rf_forcing <- feols(
  total_expenditure ~ forcing + gender + age + religion + caste + birthplace | Pole,
  data = df_iv
)

iv_simple <- feols(
  total_expenditure ~ gender + age + religion + caste + birthplace | Pole |
    treat ~ forcing,
  data = df_iv
)

wald_iv <- coef(rf_forcing)["forcing"] / coef(fs_forcing)["forcing"]

# =========================================================
#  IV con múltiples variables y el  GMM
# =========================================================
df_7 <- df0 %>%
  mutate(
    dist_group = case_when(
      distance <= 25 ~ "Z1_<=25",
      distance <= 40 ~ "Z2_25_40",
      distance < 55  ~ "Z3_40_55",
      TRUE           ~ "Z4_>=55"
    ),
    dist_group = factor(dist_group, levels = c("Z1_<=25", "Z2_25_40", "Z3_40_55", "Z4_>=55"))
  ) %>%
  filter(!is.na(dist_group))

fs_multi <- feols(
  treat ~ i(dist_group, ref = "Z4_>=55") + gender + age + religion + caste + birthplace | Pole,
  data = df_7
)

iv_multi <- feols(
  total_expenditure ~ gender + age + religion + caste + birthplace | Pole |
    treat ~ i(dist_group, ref = "Z4_>=55"),
  data = df_7
)

X <- model.matrix(
  ~ treat + gender + age + religion + caste + birthplace + Pole,
  data = df_7
)

Z <- model.matrix(
  ~ relevel(dist_group, ref = "Z4_>=55") + gender + age + religion + caste + birthplace + Pole,
  data = df_7
)

y <- df_7$total_expenditure

g_iv <- function(theta, data){
  resid <- as.vector(data$y - data$X %*% theta)
  data$Z * resid
}

gmm_7 <- gmm(
  g = g_iv,
  x = list(y = y, X = X, Z = Z),
  t0 = coef(lm(y ~ X - 1)),
  type = "twoStep"
)

# Extraer coeficieente de  GMM
nombres_gmm <- names(coef(gmm_7))
nom_treat_gmm <- nombres_gmm[grep("treat", nombres_gmm)][1]

coef_gmm <- unname(coef(gmm_7)[nom_treat_gmm])
se_gmm <- unname(sqrt(diag(vcov(gmm_7)))[nom_treat_gmm])

# =========================================================
# En esta parte lo único que realize fue insertar 
# los valores de la tarea pasada para poder generar la comparación que les pedimos
# =========================================================
tabla_final <- tibble(
  estimador = c(
    "RCT (Neyman)",
    "RCT estratificado",
    "IV simple (Wald/2SLS)",
    "IV multiple (2SLS)",
    "GMM formal"
  ),
  coeficiente = c(
    2386.057,
    2370.165,
    unname(coef(iv_simple)["fit_treat"]),
    unname(coef(iv_multi)["fit_treat"]),
    coef_gmm
  ),
  error_estandar = c(
    356.360,
    350.918,
    unname(se(iv_simple)["fit_treat"]),
    unname(se(iv_multi)["fit_treat"]),
    se_gmm
  )
) %>%
  mutate(t = coeficiente / error_estandar)

print(tabla_final)

# =========================================================
# Generación de las tablas (imprime tambien al final el valor de las pruebas de wald)
# =========================================================
etable(
  fs_forcing, fs_distance, rf_forcing, iv_simple,
  se = "hetero",
  headers = c("Primera etapa: forcing", "Primera etapa: distance", "Forma reducida", "2SLS"),
  dict = c(
    forcing = "Forcing (20-35m)",
    distance = "Distance",
    treat = "Legal electrification",
    gender = "Gender",
    age = "Age",
    religion = "Religion",
    caste = "Caste",
    birthplace = "Birthplace"
  )
)

etable(
  fs_multi, iv_multi,
  se = "hetero",
  headers = c("Primera etapa multiple", "2SLS multiple"),
  dict = c(
    treat = "Legal electrification",
    gender = "Gender",
    age = "Age",
    religion = "Religion",
    caste = "Caste",
    birthplace = "Birthplace"
  )
)

wald(fs_forcing, keep = "forcing", vcov = "hetero")
wald(fs_distance, keep = "distance", vcov = "hetero")
wald(fs_multi, keep = "dist_group", vcov = "hetero")
specTest(gmm_7)