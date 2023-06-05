


dados <- readr::read_table("Lab08q01.txt")

dados <- dados|>
  janitor::clean_names()
# dados2 <- dados2 %>% mutate(svi = svi*100)

mod3 = stats::lm(renda ~ tempo + tempo^2 + tempo^3, data = dados)
mod2 = stats::lm(renda ~ tempo + tempo^2, data = dados)
mod = stats::lm(renda ~ tempo, data = dados)

summary(mod3)
summary(mod2)
summary(mod)

broom::tidy(mod3)
broom::tidy(mod2)
broom::tidy(mod)

anova(mod3, mod2)|>broom::tidy()
anova(mod2, mod)|>broom::tidy()





