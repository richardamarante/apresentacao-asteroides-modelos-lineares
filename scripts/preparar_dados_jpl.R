library(data.table)
library(dplyr)
library(forcats)
library(car)
library(glmnet)
library(leaps)
library(MASS)
library(tibble)

origem <- "/mnt/c/Users/Richard Amarante/Desktop/ModelosLineares/Trabalho Asteroides/dados/dataset_jpl.csv"
destino <- "trabalho/data/asteroides_modelo.rds"

colunas <- tibble(
  original = c(
    "spkid", "full_name", "pdes", "name", "prefix", "neo", "pha", "H",
    "diameter", "albedo", "diameter_sigma", "orbit_id", "epoch",
    "epoch_mjd", "epoch_cal", "equinox", "e", "a", "q", "i", "om", "w",
    "ma", "ad", "n", "tp", "tp_cal", "per", "per_y", "moid", "moid_ld",
    "sigma_e", "sigma_a", "sigma_q", "sigma_i", "sigma_om", "sigma_w",
    "sigma_ma", "sigma_ad", "sigma_n", "sigma_tp", "sigma_per", "class",
    "rms"
  ),
  nome = c(
    "codigo_jpl", "nome_completo", "designacao_primaria", "nome", "prefixo",
    "objeto_proximo_terra", "potencialmente_perigoso", "magnitude_absoluta",
    "diametro_km", "albedo", "incerteza_diametro_km", "id_orbita",
    "epoca_juliana", "epoca_mjd", "epoca_calendario", "equinocio",
    "excentricidade", "semi_eixo_maior_ua", "distancia_perielio_ua",
    "inclinacao_graus", "longitude_no_ascendente_graus",
    "argumento_perielio_graus", "anomalia_media_graus",
    "distancia_afelio_ua", "movimento_medio_graus_dia",
    "tempo_perielio_juliano", "tempo_perielio_calendario",
    "periodo_orbital_dias", "periodo_orbital_anos",
    "distancia_minima_orbita_terra_ua",
    "distancia_minima_orbita_terra_ld",
    "incerteza_excentricidade", "incerteza_semi_eixo_maior",
    "incerteza_distancia_perielio", "incerteza_inclinacao",
    "incerteza_longitude_no_ascendente", "incerteza_argumento_perielio",
    "incerteza_anomalia_media", "incerteza_distancia_afelio",
    "incerteza_movimento_medio", "incerteza_tempo_perielio",
    "incerteza_periodo_orbital", "classe_orbital", "erro_medio_observacao"
  ),
  descricao = c(
    "Código interno do JPL",
    "Nome completo do objeto",
    "Designação primária",
    "Nome próprio, quando existe",
    "Prefixo de designação",
    "Indica se é objeto próximo da Terra",
    "Indica se é potencialmente perigoso",
    "Magnitude absoluta",
    "Diâmetro estimado em quilômetros",
    "Fração de luz refletida",
    "Incerteza do diâmetro em quilômetros",
    "Identificador da solução orbital",
    "Época orbital em data juliana",
    "Época orbital em MJD",
    "Época orbital em calendário",
    "Referencial/equinócio orbital",
    "Excentricidade orbital",
    "Semi-eixo maior em unidades astronômicas",
    "Distância do periélio em unidades astronômicas",
    "Inclinação orbital em graus",
    "Longitude do nó ascendente em graus",
    "Argumento do periélio em graus",
    "Anomalia média em graus",
    "Distância do afélio em unidades astronômicas",
    "Movimento médio em graus por dia",
    "Tempo do periélio em data juliana",
    "Tempo do periélio em calendário",
    "Período orbital em dias",
    "Período orbital em anos",
    "Distância mínima entre órbitas, em UA",
    "Distância mínima entre órbitas, em distâncias lunares",
    "Incerteza da excentricidade",
    "Incerteza do semi-eixo maior",
    "Incerteza da distância do periélio",
    "Incerteza da inclinação",
    "Incerteza da longitude do nó ascendente",
    "Incerteza do argumento do periélio",
    "Incerteza da anomalia média",
    "Incerteza da distância do afélio",
    "Incerteza do movimento médio",
    "Incerteza do tempo do periélio",
    "Incerteza do período orbital",
    "Classe orbital do objeto",
    "Erro médio quadrático da solução orbital"
  )
)

num_cols <- c(
  "H", "diameter", "albedo", "diameter_sigma", "epoch", "epoch_mjd",
  "e", "a", "q", "i", "om", "w", "ma", "ad", "n", "tp", "per", "per_y",
  "moid", "moid_ld", "sigma_e", "sigma_a", "sigma_q", "sigma_i",
  "sigma_om", "sigma_w", "sigma_ma", "sigma_ad", "sigma_n", "sigma_tp",
  "sigma_per", "rms"
)

dt <- fread(origem, na.strings = c("", "NA", "null"), showProgress = FALSE)

for (col in intersect(num_cols, names(dt))) {
  set(dt, j = col, value = suppressWarnings(as.numeric(dt[[col]])))
}

setnames(dt, colunas$original, colunas$nome)

dados <- as_tibble(dt) |>
  mutate(
    objeto_proximo_terra = dplyr::recode(objeto_proximo_terra, "Y" = "Sim", "N" = "Não", .default = objeto_proximo_terra),
    potencialmente_perigoso = dplyr::recode(potencialmente_perigoso, "Y" = "Sim", "N" = "Não", .default = potencialmente_perigoso),
    classe_orbital = na_if(classe_orbital, "")
  )

tipos <- vapply(dados[names(dados) %in% colunas$nome], function(x) {
  if (is.numeric(x)) "Numérica" else "Categórica/texto"
}, character(1))

categorias_variaveis <- c(
  objeto_proximo_terra = "Sim = NEO, Near-Earth Object, objeto próximo da Terra; Não = demais objetos.",
  potencialmente_perigoso = "Sim = PHA, Potentially Hazardous Asteroid, asteroide potencialmente perigoso; Não = demais objetos.",
  classe_orbital = "Classe dinâmica: cinturão principal, Apollo, Amor, Aten, cruzador de Marte, transnetuniano e outras.",
  codigo_jpl = "Identificador interno.",
  nome_completo = "Texto livre.",
  designacao_primaria = "Texto livre.",
  nome = "Texto livre, quando existe.",
  prefixo = "Texto livre, quando existe.",
  id_orbita = "Identificador da solução orbital.",
  epoca_calendario = "Data em calendário.",
  equinocio = "Referencial orbital.",
  tempo_perielio_calendario = "Data em calendário."
)

ordem_estrutura <- c(
  "diametro_km", "magnitude_absoluta", "albedo", "incerteza_diametro_km",
  "objeto_proximo_terra", "potencialmente_perigoso", "classe_orbital",
  "excentricidade", "semi_eixo_maior_ua", "distancia_perielio_ua",
  "distancia_afelio_ua", "inclinacao_graus",
  "longitude_no_ascendente_graus", "argumento_perielio_graus",
  "anomalia_media_graus", "movimento_medio_graus_dia",
  "periodo_orbital_dias", "periodo_orbital_anos",
  "distancia_minima_orbita_terra_ua",
  "distancia_minima_orbita_terra_ld",
  "erro_medio_observacao", "epoca_juliana", "epoca_mjd",
  "epoca_calendario", "tempo_perielio_juliano",
  "tempo_perielio_calendario", "equinocio", "id_orbita",
  "codigo_jpl", "nome_completo", "designacao_primaria", "nome", "prefixo",
  "incerteza_excentricidade", "incerteza_semi_eixo_maior",
  "incerteza_distancia_perielio", "incerteza_inclinacao",
  "incerteza_longitude_no_ascendente", "incerteza_argumento_perielio",
  "incerteza_anomalia_media", "incerteza_distancia_afelio",
  "incerteza_movimento_medio", "incerteza_tempo_perielio",
  "incerteza_periodo_orbital"
)

estrutura <- colunas |>
  transmute(
    Variável = nome,
    Descrição = descricao,
    Tipo = unname(tipos[nome]),
    Categorias = unname(categorias_variaveis[nome])
  ) |>
  mutate(
    Categorias = if_else(is.na(Categorias), "", Categorias),
    ordem = match(Variável, ordem_estrutura)
  ) |>
  arrange(ordem) |>
  dplyr::select(-ordem)

na_estrutura <- colunas |>
  transmute(
    Variável = nome,
    `NAs` = vapply(nome, function(x) sum(is.na(dados[[x]])), numeric(1)),
    `% NAs` = 100 * `NAs` / nrow(dados)
  )

resumo_bruto <- list(
  n_total = nrow(dados),
  n_variaveis = length(colunas$nome),
  n_numericas = sum(tipos == "Numérica"),
  n_categoricas = sum(tipos != "Numérica"),
  n_neo = sum(dados$objeto_proximo_terra == "Sim", na.rm = TRUE),
  n_pha = sum(dados$potencialmente_perigoso == "Sim", na.rm = TRUE),
  classes = dados |> count(classe_orbital, sort = TRUE),
  estrutura = estrutura,
  nas = na_estrutura
)

candidatas <- c(
  "magnitude_absoluta", "albedo", "log_albedo",
  "excentricidade", "semi_eixo_maior_ua", "log_semi_eixo_maior",
  "distancia_perielio_ua", "distancia_afelio_ua",
  "inclinacao_graus", "longitude_no_ascendente_graus",
  "argumento_perielio_graus", "anomalia_media_graus",
  "movimento_medio_graus_dia", "periodo_orbital_dias",
  "log_periodo_orbital", "distancia_minima_orbita_terra_ua",
  "log_distancia_minima_orbita_terra", "erro_medio_observacao",
  "classe_orbital", "objeto_proximo_terra", "potencialmente_perigoso"
)

dados_modelo <- dados |>
  dplyr::select(
    diametro_km,
    dplyr::all_of(setdiff(candidatas, c(
      "log_albedo",
      "log_semi_eixo_maior",
      "log_periodo_orbital",
      "log_distancia_minima_orbita_terra"
    )))
  ) |>
  filter(
    diametro_km > 0,
    albedo > 0,
    semi_eixo_maior_ua > 0,
    periodo_orbital_dias > 0,
    distancia_minima_orbita_terra_ua > 0
  ) |>
  mutate(
    log_diametro = log(diametro_km),
    log_albedo = log(albedo),
    log_semi_eixo_maior = log(semi_eixo_maior_ua),
    log_periodo_orbital = log(periodo_orbital_dias),
    log_distancia_minima_orbita_terra = log(distancia_minima_orbita_terra_ua)
  ) |>
  dplyr::select(diametro_km, log_diametro, dplyr::all_of(candidatas)) |>
  filter(if_all(everything(), ~ !is.na(.x))) |>
  mutate(
    classe_orbital = fct_lump_n(factor(classe_orbital), n = 8, other_level = "Outras"),
    objeto_proximo_terra = factor(objeto_proximo_terra),
    potencialmente_perigoso = factor(potencialmente_perigoso)
  )

set.seed(20260626)
amostra_plot <- dados_modelo |>
  slice_sample(n = min(25000, nrow(dados_modelo)))

modelo_boxcox_base <- lm(
  diametro_km ~ magnitude_absoluta + albedo +
    excentricidade + semi_eixo_maior_ua + distancia_perielio_ua +
    distancia_afelio_ua + inclinacao_graus + distancia_minima_orbita_terra_ua,
  data = dados_modelo
)

bc <- boxcox(modelo_boxcox_base, lambda = seq(-1, 1, by = 0.02), plotit = FALSE)
lambda_otimo <- bc$x[which.max(bc$y)]
boxcox_df <- tibble(lambda = bc$x, log_verossimilhanca = bc$y)

modelo_selecao <- lm(
  log_diametro ~ magnitude_absoluta + log_albedo +
    excentricidade + log_semi_eixo_maior + distancia_perielio_ua +
    distancia_afelio_ua + inclinacao_graus +
    log_distancia_minima_orbita_terra + erro_medio_observacao +
    classe_orbital + objeto_proximo_terra + potencialmente_perigoso,
  data = dados_modelo
)

modelo_final <- lm(
  log_diametro ~ magnitude_absoluta + log_albedo,
  data = dados_modelo
)

formula_selecao <- log_diametro ~ magnitude_absoluta + log_albedo +
  excentricidade + log_semi_eixo_maior + distancia_perielio_ua +
  distancia_afelio_ua + inclinacao_graus +
  log_distancia_minima_orbita_terra + erro_medio_observacao +
  classe_orbital + objeto_proximo_terra + potencialmente_perigoso

termos_selecao <- attr(terms(formula_selecao), "term.labels")

rotulos_termos <- c(
  magnitude_absoluta = "Magnitude absoluta",
  log_albedo = "log(albedo)",
  excentricidade = "Excentricidade",
  log_semi_eixo_maior = "log(semi-eixo maior)",
  distancia_perielio_ua = "Distância do periélio",
  distancia_afelio_ua = "Distância do afélio",
  inclinacao_graus = "Inclinação",
  log_distancia_minima_orbita_terra = "log(distância mínima à órbita da Terra)",
  erro_medio_observacao = "Erro médio da órbita",
  classe_orbital = "Classe orbital",
  objeto_proximo_terra = "Objeto próximo da Terra",
  potencialmente_perigoso = "Potencialmente perigoso"
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

modelo_nulo <- lm(log_diametro ~ 1, data = dados_modelo)
escopo_selecao <- list(lower = formula(modelo_nulo), upper = formula_selecao)

modelo_forward_aic <- stepAIC(
  modelo_nulo,
  scope = escopo_selecao,
  direction = "forward",
  trace = FALSE
)

modelo_backward_aic <- stepAIC(
  modelo_selecao,
  direction = "backward",
  trace = FALSE
)

modelo_stepwise_aic <- stepAIC(
  modelo_nulo,
  scope = escopo_selecao,
  direction = "both",
  trace = FALSE
)

k_bic <- log(nrow(dados_modelo))

modelo_forward_bic <- step(
  modelo_nulo,
  scope = escopo_selecao,
  direction = "forward",
  trace = FALSE,
  k = k_bic
)

modelo_backward_bic <- step(
  modelo_selecao,
  direction = "backward",
  trace = FALSE,
  k = k_bic
)

modelo_stepwise_bic <- step(
  modelo_nulo,
  scope = escopo_selecao,
  direction = "both",
  trace = FALSE,
  k = k_bic
)

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
  tabela_pvalor <- tibble(
    Etapa = 0,
    `Termo removido` = "Nenhum",
    `p-valor` = NA_real_
  )
}

set.seed(20260626)
dados_lasso <- dados_modelo |>
  slice_sample(n = min(50000, nrow(dados_modelo)))

x_lasso <- model.matrix(formula_selecao, data = dados_lasso)[, -1, drop = FALSE]
y_lasso <- dados_lasso$log_diametro

cv_lasso <- cv.glmnet(
  x_lasso,
  y_lasso,
  alpha = 1,
  nfolds = 5,
  standardize = TRUE
)

coef_lasso <- as.matrix(coef(cv_lasso, s = "lambda.1se"))

mapear_termo_lasso <- function(x) {
  ifelse(
    startsWith(x, "classe_orbital"), "classe_orbital",
    ifelse(
      startsWith(x, "objeto_proximo_terra"), "objeto_proximo_terra",
      ifelse(startsWith(x, "potencialmente_perigoso"), "potencialmente_perigoso", x)
    )
  )
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

termos_lasso <- tabela_lasso_coef |>
  pull(Variável)

termos_lasso_modelo <- names(rotulos_termos)[rotulos_termos %in% termos_lasso]
if (length(termos_lasso_modelo) == 0) {
  modelo_lasso_pos <- modelo_nulo
} else {
  modelo_lasso_pos <- lm(
    as.formula(paste("log_diametro ~", paste(termos_lasso_modelo, collapse = " + "))),
    data = dados_modelo
  )
}

modelo_fiv <- lm(
  log_diametro ~ magnitude_absoluta + log_albedo +
    excentricidade + log_semi_eixo_maior + distancia_perielio_ua +
    distancia_afelio_ua + inclinacao_graus +
    log_distancia_minima_orbita_terra + erro_medio_observacao,
  data = dados_modelo
)

vif_cheio <- car::vif(modelo_fiv)
if (is.matrix(vif_cheio)) {
  tabela_fiv <- tibble(
    termo = rownames(vif_cheio),
    `FIV ajustado` = vif_cheio[, "GVIF^(1/(2*Df))"]
  )
} else {
  tabela_fiv <- tibble(
    termo = names(vif_cheio),
    `FIV ajustado` = as.numeric(vif_cheio)
  )
}

tabela_fiv <- tabela_fiv |>
  mutate(
    Variável = unname(rotulos_termos[termo]),
    Variável = if_else(is.na(Variável), termo, Variável)
  ) |>
  dplyr::select(Variável, `FIV ajustado`) |>
  arrange(desc(`FIV ajustado`))

tabela_metodos <- bind_rows(
  metricas_modelo("AIC forward", modelo_forward_aic),
  metricas_modelo("AIC backward", modelo_backward_aic),
  metricas_modelo("AIC stepwise", modelo_stepwise_aic),
  metricas_modelo("BIC forward", modelo_forward_bic),
  metricas_modelo("BIC backward", modelo_backward_bic),
  metricas_modelo("BIC stepwise", modelo_stepwise_bic),
  metricas_modelo("p-valor backward", modelo_pvalor),
  metricas_modelo("LASSO 1-SE", modelo_lasso_pos)
)

set.seed(20260626)
dados_selecao <- dados_modelo |>
  dplyr::select(log_diametro, dplyr::all_of(setdiff(termos_selecao, c(
    "classe_orbital", "objeto_proximo_terra", "potencialmente_perigoso"
  )))) |>
  slice_sample(n = min(50000, nrow(dados_modelo)))

subconjuntos <- regsubsets(
  log_diametro ~ .,
  data = dados_selecao,
  nvmax = 6,
  method = "exhaustive"
)

sub_resumo <- summary(subconjuntos)
selecionadas_brutas <- apply(sub_resumo$outmat, 1, function(x) {
  paste(names(x)[x == "*"], collapse = ", ")
})

selecionadas <- vapply(
  strsplit(selecionadas_brutas, ", "),
  formatar_termos,
  character(1),
  limite = 4
)

tabela_subconjuntos <- tibble(
  `Número de preditores` = seq_along(selecionadas),
  `Preditores selecionados` = selecionadas,
  BIC = sub_resumo$bic,
  `R² ajustado` = sub_resumo$adjr2
)

selecoes <- list(
  modelo_cheio = modelo_selecao,
  modelo_final = modelo_final,
  subconjuntos = tabela_subconjuntos,
  metodos = tabela_metodos,
  pvalor = tabela_pvalor,
  lasso = tabela_lasso_coef,
  fiv = tabela_fiv
)

aic_comparacao <- tibble(
  Modelo = c("Cheio", "Físico simples"),
  Preditores = c(
    "Magnitude, albedo, órbita, classe, proximidade e periculosidade",
    "Magnitude absoluta e log(albedo)"
  ),
  AIC = c(AIC(modelo_selecao), AIC(modelo_final)),
  BIC = c(BIC(modelo_selecao), BIC(modelo_final)),
  `R² ajustado` = c(
    summary(modelo_selecao)$adj.r.squared,
    summary(modelo_final)$adj.r.squared
  )
)

resumo_modelo <- list(
  n_modelo = nrow(dados_modelo),
  lambda_boxcox = lambda_otimo,
  aic_comparacao = aic_comparacao
)

obj <- list(
  resumo_bruto = resumo_bruto,
  dados_modelo = dados_modelo,
  amostra_plot = amostra_plot,
  boxcox = boxcox_df,
  resumo_modelo = resumo_modelo,
  selecoes = selecoes
)

saveRDS(obj, destino)
