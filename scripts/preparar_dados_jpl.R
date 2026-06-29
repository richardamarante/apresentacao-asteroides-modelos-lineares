# Preparação dos dados de asteroides (base JPL/SBDB) para a apresentação.
# Lê o CSV bruto e o dicionário, monta a base de modelagem (casos completos),
# roda Box-Cox, seleção de variáveis e diagnósticos, e salva tudo em um .rds.

library(data.table)
library(dplyr)
library(forcats)
library(car)
library(glmnet)
library(leaps)
library(MASS)
library(tibble)

# Permite sobrescrever a origem dos CSVs (úti­l no WSL, onde ler direto de
# /mnt/c quebra o fread; o wrapper bash copia para /tmp e aponta para cá).
origem      <- Sys.getenv("ORIGEM_CSV", "trabalho/data/dataset_asteroides.csv")
dicionario  <- Sys.getenv("DIC_CSV",    "trabalho/data/dicionario_dataset_asteroides.csv")
destino     <- "trabalho/data/asteroides_modelo.rds"

# ---------------------------------------------------------------------------
# 1. Leitura
# ---------------------------------------------------------------------------
num_cols <- c(
  "diametro", "magnitude_absoluta", "albedo", "excentricidade_orbita",
  "tamanho_orbita", "distancia_perielio", "distancia_afelio",
  "inclinacao_orbita", "orientacao_orbita", "periodo_orbita",
  "vel_media_angular_orbita", "dist_minima_terra"
)

# fread via mmap quebra em drives montados do Windows (/mnt/c) no WSL.
# Copiamos para o filesystem nativo (/tmp, ext4) antes de ler.
ler_csv <- function(caminho, ...) {
  abs <- normalizePath(caminho, mustWork = TRUE)
  if (grepl("^/mnt/", abs)) {
    tmp <- tempfile(fileext = ".csv")
    file.copy(abs, tmp, overwrite = TRUE)
    on.exit(unlink(tmp), add = TRUE)
    fread(tmp, ...)
  } else {
    fread(abs, ...)
  }
}

dt <- ler_csv(origem, na.strings = c("", "NA", "null", "NaN"), showProgress = FALSE)

for (col in intersect(num_cols, names(dt))) {
  set(dt, j = col, value = suppressWarnings(as.numeric(dt[[col]])))
}

dados <- as_tibble(dt) |>
  mutate(
    neo           = dplyr::recode(neo, "sim" = "Sim", "nao" = "Não", .default = neo),
    pha           = dplyr::recode(pha, "sim" = "Sim", "nao" = "Não", .default = pha),
    classe_orbita = na_if(classe_orbita, "")
  )

# ---------------------------------------------------------------------------
# 2. Dicionário (tabela única: Variável | Unidade de medida | Descrição)
# ---------------------------------------------------------------------------
dic <- ler_csv(dicionario, na.strings = c("", "NA")) |>
  as_tibble()

estrutura <- dic |>
  transmute(
    `Variável`          = .data[[names(dic)[1]]],
    `Unidade de medida` = .data[[names(dic)[2]]],
    `Descrição`         = .data[[names(dic)[3]]]
  )

# Classificação de tipos (a partir da própria base)
tipos <- vapply(dados, function(x) if (is.numeric(x)) "Numérica" else "Categórica/texto",
                character(1))

# ---------------------------------------------------------------------------
# 3. Resumo bruto da base
# ---------------------------------------------------------------------------
n_total        <- nrow(dados)
n_com_diametro <- sum(!is.na(dados$diametro) & dados$diametro > 0)
n_sem_diametro <- n_total - n_com_diametro

na_estrutura <- tibble(
  `Variável` = names(dados),
  `NAs`      = vapply(dados, function(x) sum(is.na(x)), numeric(1)),
  `% NAs`    = 100 * vapply(dados, function(x) sum(is.na(x)), numeric(1)) / n_total
)

resumo_bruto <- list(
  n_total        = n_total,
  n_com_diametro = n_com_diametro,
  n_sem_diametro = n_sem_diametro,
  pct_sem_diametro = 100 * n_sem_diametro / n_total,
  n_variaveis    = ncol(dados),
  n_numericas    = sum(tipos == "Numérica"),
  n_categoricas  = sum(tipos != "Numérica"),
  n_neo          = sum(dados$neo == "Sim", na.rm = TRUE),
  n_pha          = sum(dados$pha == "Sim", na.rm = TRUE),
  classes        = dados |> count(classe_orbita, sort = TRUE),
  estrutura      = estrutura,
  nas            = na_estrutura
)

# ---------------------------------------------------------------------------
# 4. Base de modelagem (casos completos, escalas log para variáveis positivas)
# ---------------------------------------------------------------------------
dados_modelo <- dados |>
  filter(
    !is.na(diametro), diametro > 0,
    !is.na(albedo), albedo > 0,
    !is.na(magnitude_absoluta),
    !is.na(excentricidade_orbita),
    !is.na(tamanho_orbita), tamanho_orbita > 0,
    !is.na(distancia_perielio), distancia_perielio > 0,
    !is.na(distancia_afelio), distancia_afelio > 0,
    !is.na(inclinacao_orbita),
    !is.na(orientacao_orbita),
    !is.na(periodo_orbita), periodo_orbita > 0,
    !is.na(vel_media_angular_orbita),
    !is.na(dist_minima_terra),
    !is.na(classe_orbita)
  ) |>
  mutate(
    log_diametro          = log(diametro),
    log_albedo            = log(albedo),
    log_tamanho_orbita    = log(tamanho_orbita),
    log_periodo_orbita    = log(periodo_orbita),
    log_dist_minima_terra = log1p(dist_minima_terra),
    classe_orbita = fct_lump_n(factor(classe_orbita), n = 8, other_level = "Outras"),
    neo = factor(neo),
    pha = factor(pha)
  )

set.seed(20260629)
amostra_plot <- dados_modelo |>
  slice_sample(n = min(25000, nrow(dados_modelo)))

# ---------------------------------------------------------------------------
# 5. Box-Cox (justifica a transformação da resposta)
# ---------------------------------------------------------------------------
modelo_boxcox_base <- lm(
  diametro ~ magnitude_absoluta + albedo + excentricidade_orbita +
    tamanho_orbita + distancia_perielio + distancia_afelio +
    inclinacao_orbita + dist_minima_terra,
  data = dados_modelo
)

bc <- boxcox(modelo_boxcox_base, lambda = seq(-1, 1, by = 0.02), plotit = FALSE)
lambda_otimo <- bc$x[which.max(bc$y)]
boxcox_df <- tibble(lambda = bc$x, log_verossimilhanca = bc$y)

# ---------------------------------------------------------------------------
# 6. Modelos: cheio (seleção), final (parcimônia)
# ---------------------------------------------------------------------------
formula_selecao <- log_diametro ~ magnitude_absoluta + log_albedo +
  excentricidade_orbita + log_tamanho_orbita + distancia_perielio +
  distancia_afelio + inclinacao_orbita + orientacao_orbita +
  log_periodo_orbita + vel_media_angular_orbita + log_dist_minima_terra +
  classe_orbita + neo + pha

modelo_selecao <- lm(formula_selecao, data = dados_modelo)
modelo_final   <- lm(log_diametro ~ magnitude_absoluta + log_albedo, data = dados_modelo)
modelo_nulo    <- lm(log_diametro ~ 1, data = dados_modelo)

termos_selecao <- attr(terms(formula_selecao), "term.labels")

rotulos_termos <- c(
  magnitude_absoluta       = "Magnitude absoluta",
  log_albedo               = "log(albedo)",
  excentricidade_orbita    = "Excentricidade",
  log_tamanho_orbita       = "log(tamanho da órbita)",
  distancia_perielio       = "Distância do periélio",
  distancia_afelio         = "Distância do afélio",
  inclinacao_orbita        = "Inclinação",
  orientacao_orbita        = "Orientação",
  log_periodo_orbita       = "log(período orbital)",
  vel_media_angular_orbita = "Velocidade angular média",
  log_dist_minima_terra    = "log(distância mínima à Terra)",
  classe_orbita            = "Classe orbital",
  neo                      = "Objeto próximo da Terra",
  pha                      = "Potencialmente perigoso"
)

formatar_termos <- function(termos, limite = 4) {
  termos <- unique(termos)
  if (length(termos) == 0) return("Intercepto")
  nomes <- unname(rotulos_termos[termos])
  nomes[is.na(nomes)] <- termos[is.na(nomes)]
  if (length(nomes) <= limite) {
    paste(nomes, collapse = ", ")
  } else {
    paste0(paste(head(nomes, limite), collapse = ", "), " + ", length(nomes) - limite, " outros")
  }
}

termos_do_modelo <- function(modelo) attr(terms(modelo), "term.labels")

metricas_modelo <- function(metodo, modelo, limite = 3) {
  tibble(
    Método = metodo,
    `Termos` = length(termos_do_modelo(modelo)),
    `Preditores mantidos` = formatar_termos(termos_do_modelo(modelo), limite = limite),
    AIC = AIC(modelo),
    BIC = BIC(modelo),
    `R² ajustado` = summary(modelo)$adj.r.squared
  )
}

escopo_selecao <- list(lower = formula(modelo_nulo), upper = formula_selecao)

# Stepwise por AIC
modelo_forward_aic  <- stepAIC(modelo_nulo, scope = escopo_selecao, direction = "forward", trace = FALSE)
modelo_backward_aic <- stepAIC(modelo_selecao, direction = "backward", trace = FALSE)
modelo_stepwise_aic <- stepAIC(modelo_nulo, scope = escopo_selecao, direction = "both", trace = FALSE)

# Stepwise por BIC
k_bic <- log(nrow(dados_modelo))
modelo_forward_bic  <- step(modelo_nulo, scope = escopo_selecao, direction = "forward", trace = FALSE, k = k_bic)
modelo_backward_bic <- step(modelo_selecao, direction = "backward", trace = FALSE, k = k_bic)
modelo_stepwise_bic <- step(modelo_nulo, scope = escopo_selecao, direction = "both", trace = FALSE, k = k_bic)

# Backward por p-valor
modelo_pvalor <- modelo_selecao
historico_pvalor <- list()
repeat {
  testes <- drop1(modelo_pvalor, test = "F")
  testes <- testes[rownames(testes) != "<none>", , drop = FALSE]
  pvals <- testes$`Pr(>F)`
  if (!length(pvals) || all(is.na(pvals)) || max(pvals, na.rm = TRUE) <= 0.05) break
  termo_removido <- rownames(testes)[which.max(pvals)]
  historico_pvalor[[length(historico_pvalor) + 1]] <- tibble(
    Etapa = length(historico_pvalor) + 1,
    `Termo removido` = formatar_termos(termo_removido),
    `p-valor` = max(pvals, na.rm = TRUE)
  )
  modelo_pvalor <- update(modelo_pvalor, paste(". ~ . -", termo_removido))
}
tabela_pvalor <- bind_rows(historico_pvalor)
if (nrow(tabela_pvalor) == 0) {
  tabela_pvalor <- tibble(Etapa = 0, `Termo removido` = "Nenhum", `p-valor` = NA_real_)
}

# LASSO
set.seed(20260629)
dados_lasso <- dados_modelo |> slice_sample(n = min(50000, nrow(dados_modelo)))
x_lasso <- model.matrix(formula_selecao, data = dados_lasso)[, -1, drop = FALSE]
y_lasso <- dados_lasso$log_diametro
cv_lasso <- cv.glmnet(x_lasso, y_lasso, alpha = 1, nfolds = 5, standardize = TRUE)
coef_lasso <- as.matrix(coef(cv_lasso, s = "lambda.1se"))

mapear_termo_lasso <- function(x) {
  ifelse(startsWith(x, "classe_orbita"), "classe_orbita",
    ifelse(startsWith(x, "neo"), "neo",
      ifelse(startsWith(x, "pha"), "pha", x)))
}

tabela_lasso_coef <- tibble(
  termo_matriz = rownames(coef_lasso),
  coeficiente = as.numeric(coef_lasso[, 1])
) |>
  filter(termo_matriz != "(Intercept)", abs(coeficiente) > 0) |>
  mutate(
    termo = mapear_termo_lasso(termo_matriz),
    Variável = unname(rotulos_termos[termo]),
    Variável = if_else(is.na(Variável), termo, Variável)
  ) |>
  group_by(Variável) |>
  summarise(
    `Coeficiente máximo em módulo` = max(abs(coeficiente)),
    `Termos não nulos` = n(),
    .groups = "drop"
  ) |>
  arrange(desc(`Coeficiente máximo em módulo`))

termos_lasso <- tabela_lasso_coef |> pull(Variável)
termos_lasso_modelo <- names(rotulos_termos)[rotulos_termos %in% termos_lasso]
if (length(termos_lasso_modelo) == 0) {
  modelo_lasso_pos <- modelo_nulo
} else {
  modelo_lasso_pos <- lm(
    as.formula(paste("log_diametro ~", paste(termos_lasso_modelo, collapse = " + "))),
    data = dados_modelo
  )
}

# FIV (bloco numérico)
modelo_fiv <- lm(
  log_diametro ~ magnitude_absoluta + log_albedo + excentricidade_orbita +
    log_tamanho_orbita + distancia_perielio + distancia_afelio +
    inclinacao_orbita + orientacao_orbita + log_periodo_orbita +
    vel_media_angular_orbita + log_dist_minima_terra,
  data = dados_modelo
)
vif_cheio <- car::vif(modelo_fiv)
if (is.matrix(vif_cheio)) {
  tabela_fiv <- tibble(termo = rownames(vif_cheio), `FIV` = vif_cheio[, 1])
} else {
  tabela_fiv <- tibble(termo = names(vif_cheio), `FIV` = as.numeric(vif_cheio))
}
tabela_fiv <- tabela_fiv |>
  mutate(
    Variável = unname(rotulos_termos[termo]),
    Variável = if_else(is.na(Variável), termo, Variável)
  ) |>
  dplyr::select(Variável, `FIV`) |>
  arrange(desc(`FIV`))

tabela_metodos <- bind_rows(
  metricas_modelo("AIC forward", modelo_forward_aic),
  metricas_modelo("AIC backward", modelo_backward_aic),
  metricas_modelo("AIC stepwise", modelo_stepwise_aic),
  metricas_modelo("BIC forward", modelo_forward_bic),
  metricas_modelo("BIC backward", modelo_backward_bic),
  metricas_modelo("BIC stepwise", modelo_stepwise_bic),
  metricas_modelo("p-valor backward", modelo_pvalor)
)

# Melhores subconjuntos (apenas preditores numéricos)
set.seed(20260629)
preditores_num <- setdiff(termos_selecao, c("classe_orbita", "neo", "pha"))
dados_selecao <- dados_modelo |>
  dplyr::select(log_diametro, dplyr::all_of(preditores_num)) |>
  slice_sample(n = min(50000, nrow(dados_modelo)))

subconjuntos <- regsubsets(log_diametro ~ ., data = dados_selecao, nvmax = 6, method = "exhaustive")
sub_resumo <- summary(subconjuntos)
selecionadas_brutas <- apply(sub_resumo$outmat, 1, function(x) paste(names(x)[x == "*"], collapse = ", "))
selecionadas <- vapply(strsplit(selecionadas_brutas, ", "), formatar_termos, character(1), limite = 4)

tabela_subconjuntos <- tibble(
  `Número de preditores` = seq_along(selecionadas),
  `Preditores selecionados` = selecionadas,
  BIC = sub_resumo$bic,
  `R² ajustado` = sub_resumo$adjr2
)

# ---------------------------------------------------------------------------
# 7. Comparação final e empacotamento
# ---------------------------------------------------------------------------
aic_comparacao <- tibble(
  Modelo = c("Cheio", "Parcimonioso"),
  Preditores = c(
    "Todas as candidatas (físicas, orbitais e categóricas)",
    "Magnitude absoluta e log(albedo)"
  ),
  AIC = c(AIC(modelo_selecao), AIC(modelo_final)),
  BIC = c(BIC(modelo_selecao), BIC(modelo_final)),
  `R² ajustado` = c(summary(modelo_selecao)$adj.r.squared, summary(modelo_final)$adj.r.squared)
)

selecoes <- list(
  subconjuntos = tabela_subconjuntos,
  metodos      = tabela_metodos,
  pvalor       = tabela_pvalor,
  lasso        = tabela_lasso_coef,
  fiv          = tabela_fiv
)

resumo_modelo <- list(
  n_modelo      = nrow(dados_modelo),
  lambda_boxcox = lambda_otimo,
  aic_comparacao = aic_comparacao
)

obj <- list(
  resumo_bruto  = resumo_bruto,
  dados_modelo  = dados_modelo,
  amostra_plot  = amostra_plot,
  boxcox        = boxcox_df,
  resumo_modelo = resumo_modelo,
  selecoes      = selecoes
)

saveRDS(obj, destino)

cat("OK -> ", destino, "\n")
cat("n_total:", n_total, "| com diametro:", n_com_diametro,
    "| sem diametro:", n_sem_diametro,
    sprintf("(%.1f%%)", 100 * n_sem_diametro / n_total), "\n")
cat("n_modelo (casos completos):", nrow(dados_modelo), "\n")
cat("lambda Box-Cox:", round(lambda_otimo, 3), "\n")
cat("R2 aj. final:", round(summary(modelo_final)$adj.r.squared, 4), "\n")
cat("R2 aj. cheio:", round(summary(modelo_selecao)$adj.r.squared, 4), "\n")
