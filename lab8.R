
# Pacotes ----
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, caret)

# Dados ----
dados <- readr::read_table("Lab08q01.txt")

dados <- dados|>
  janitor::clean_names()

# Modelo Linear Simples ----

# Descritiva

dados|>
  dplyr::rename("Renda" = renda, "Tempo" = tempo)|>
  summarytools::descr(
    stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv", "Skewness", "Kurtosis"),
    justify = "c",
    style = "rmarkdown",
    transpose = T
  )|>
  kableExtra::kbl(
    caption = "Medidas Resumo dos dados",
    digits = 2,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = T, 
    booktabs = T
  )|>
  kableExtra::column_spec(1, bold = T)|>
  kableExtra::kable_styling(
    full_width = F,
    position = 'center', 
    latex_options = c("striped", "HOLD_position", "scale_down")
  )|>
  kableExtra::kable_material()

dados|>
  dplyr::rename("Renda" = renda, "Tempo" = tempo)|>
  psych::pairs.panels(smooth = F, digits = 3, ellipses = F, 
                      hist.col = "skyblue", rug = F, stars = T, show.points = T,
                      main = "Figura 1: Relação entre as variáveis")


# Modelo completo
mod = stats::lm(renda ~ tempo, data = dados)

# Modelo de treino
mod_tr = stats::lm(formula = renda ~ tempo, data = dados_tr)
summary(mod_tr)

tr = base::round(0.8*base::nrow(dados))

base::set.seed(2023)
treino = base::sample(x = base::nrow(dados), size = tr, replace = F)

# Dados de treino
dados_tr <- dados[treino, ]

# Gráfico
plot(renda~tempo, dados_tr, pch = 20, cex = 1.5)
abline(mod_tr, col = "seagreen")
graphics::grid(lwd = 2)

dados|>
  ggplot2::ggplot(mapping = aes(x = tempo, y = renda))+
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = "lm", formula = y ~ stats::poly(x, degree = 3, raw = T))+
  ggplot2::geom_smooth(method = "lm", formula = "y ~ x", color = "seagreen")+
  ggplot2::labs(title = "Renda vs Tempo")+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  ggplot2::theme_bw()



dados_tr|>
  ggplot2::ggplot(mapping = aes(x = tempo, y = renda))+
  ggplot2::geom_point()+
  # ggplot2::geom_smooth(method = "lm", formula = "y~poly(x,3")+
  ggplot2::geom_smooth(method = "lm", formula = "y ~ x", color = "seagreen")+
  ggplot2::labs(title = "Renda vs Tempo")+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  ggplot2::theme_bw()





## Teste ----

# Dados de teste
dados_test <- dados[-treino, ]

# Previsão com dados de teste
res_test <- stats::predict(
  mod_tr, newdata = base::data.frame(tempo = dados_test[,2]))

dados_test[,1]|>t()
res_test

# Gráfico
plot(renda~tempo, dados_tr, pch = 20, cex = 1.5)
abline(mod_tr, col = "seagreen")
graphics::grid(lwd = 2)

ggplot2::ggplot()+
  ggplot2::geom_point(mapping = aes(x = dados_test$tempo, y = dados_test$renda))+
  # ggplot2::geom_smooth(method = "lm", formula = "y~poly(x,3")+
  ggplot2::geom_smooth(mapping = aes(x = dados_tr$tempo, y = dados_tr$renda), method = "lm", formula = "y ~ x", color = "seagreen")+
  ggplot2::labs(title = "Renda vs Tempo")+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  ggplot2::theme_bw()


# Performace do modelo

caret::defaultSummary(data = dados)


# Análise de gráfica
graphics::par(mfrow = c(2,2))
graphics::plot(mod)
graphics::par(mfrow = c(1,1))

# Teste de homocedasticidade
olsrr::ols_test_breusch_pagan(model = mod)


## Modelo polinomial ----
# 2 formas de fazer:
# Forma 1
mod2 = stats::lm(renda ~ tempo + base::I(tempo^2), data = dados)
mod3 = stats::lm(renda ~ tempo + base::I(tempo^2) + base::I(tempo^3), data = dados)

# Forma 2
mod3 = stats::lm(renda ~ stats::poly(x = tempo, degree = 3, raw = T), data = dados)


mod3$coefficients[[1]]
mod3$coefficients[[2]]
mod3$coefficients[[3]]
mod3$coefficients[[4]]


anova(mod)
anova(mod2)
anova(mod3)

summary(mod)
summary(mod2)
summary(mod3)

broom::tidy(mod)
broom::tidy(mod2)
broom::tidy(mod3)


# Modelo polinomial ajustado
dados|>
  ggplot2::ggplot(mapping = aes(x = tempo, y = renda))+
  ggplot2::geom_point()+
  # ggplot2::geom_smooth(method = "lm", formula = "y~poly(x,3")+
  ggplot2::geom_smooth(method = "lm", formula = y ~ stats::poly(x, degree = 3, raw = T), color = "seagreen")+
  ggplot2::labs(title = "Renda vs Tempo")+
  scale_y_continuous(labels = scales::number_format(big.mark = ".",decimal.mark = ","))+
  ggplot2::theme_bw()




anova(mod3, mod2)|>broom::tidy()
anova(mod2, mod)|>broom::tidy()


graphics::par(mfrow = c(2,2))
graphics::plot(mod3)
graphics::par(mfrow = c(1,1))


stats::shapiro.test(residuals(mod3))

dwtest(mod3)

# Teste de homocedasticidade
olsrr::ols_test_breusch_pagan(model = mod3)


# ----
# MAE = ERRO ABSOLUTO MÉDIO

# RMSE = RAIZ DO ERRO QUADRÁTICO MÉDIO

























# FIM ----
