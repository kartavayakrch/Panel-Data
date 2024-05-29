#загрузим пакеты
library(plm)
library(dplyr)
library(stargazer)
library(erer)
library(DescTools)
View(wage_data)

data <- pdata.frame(wage_data, index = c("nr", "year"))

#построим модель пула
mod_pooled <- plm(wage ~ school + exper + union + married + health,
                  data = wage_data, model = "pooling")
summary(mod_pooled)

#случайные эффекты
mod_RE <- plm(wage ~ school + exper + union + married + health,
              data = wage_data, model = "random")
summary(mod_RE)

#фиксированные эффекты
mod_FE <- plm(wage ~ school + exper + union + married + health,
              data = wage_data, model = "within")
summary(mod_FE)

#сравним модели по тестам
#pooling против FE (короткая против длинной)
pFtest(mod_FE, mod_pooled)
#p-value < 0.05 - FE лучше

#RE против FE (тест Хаусмана)
phtest(mod_FE, mod_RE)
#p-value < 0.05 - FE лучше

#pooling против RE (тест множителей Лагранжа Бреуша-Пагана)
plmtest(mod_RE)
#p-value < 0.05 - pooling лучше RE
#  RE< pooling < FE
#таким образом, делаем выбор в пользу FE

#выведем все модели
stargazer(mod_pooled, mod_FE, mod_RE,
          column.labels = c("Pooled", "FE", "RE"),
          type = "html", out = "3_models.html")

#Оставьте в выборке только наблюдения 1980 года. Введите переменную ww, 
# которая принимает значение равное 1, если переменная wage больше 1,5 
# («высокая» заработная плата) и 0 в противном случае («низкая» заработная плата). 
# По полученным данным постройте логистическую регрессию с зависимой 
# переменной ww и регрессорами school, exper (2 балла)
data2<- filter(data,year == 1980)
data3 <- mutate(data2, ww = ifelse(wage >= 1.5, 1, 0))
mod_logit <- glm(ww ~ school + exper, family = binomial, x = TRUE, data = data3)
summary(mod_logit)
#выгрузка
stargazer(mod_logit,
          type = "html", out = "logit_mod_prak.html")
#предельные эффекты 
maBina(mod_logit)

