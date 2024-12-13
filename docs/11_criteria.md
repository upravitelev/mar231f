# Стат.критерии

## Запись занятия

<iframe width="560" height="315" src="https://www.youtube.com/embed/uD1DNim8Is4?si=VQnoN1TL7PvwBdub" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

## Код занятия на Python

https://colab.research.google.com/drive/19wQXtrX6in8Sj305043R1hnMCF_F9tno


## Разбор домашнего задания

### level 2 (HNTR)

Прочитайте [пример расчета](https://medstatistic.ru/methods/methods4.html) критерия $\chi^2$-Пирсона. Повторите его вручную (повторите действия, которые описаны на странице).
Проверьте значимость различия групп с помощью R / Python (необходимо выбрать соответствующую функцию и/или пакет).

Для ситуаций, когда у нас есть одна категориальная группирующая переменная и бинарная зависимая переменная (вернулся/не вернулся, купил/не купил), мы используем тест пропорций ($\chi^2$-Пирсона для таблиц 2*2 как раз сводится к нему):


``` r
prop.test(c(40, 30), c(72, 78), correct = FALSE, alternative = "two.sided")
```

```
## 
## 	2-sample test for equality of proportions without continuity correction
## 
## data:  c(40, 30) out of c(72, 78)
## X-squared = 4.3956, df = 1, p-value = 0.03603
## alternative hypothesis: two.sided
## 95 percent confidence interval:
##  0.0133635 0.3285168
## sample estimates:
##    prop 1    prop 2 
## 0.5555556 0.3846154
```


### level 3 (HMP)

Сгенерируйте две выборки из нормального распределения (N = 100 в каждой, средние различаются на 5). Сравните, значимо ли различие выборок с помощью R / Python с помощью $t$-критерия Стьюдента (необходимо выбрать соответствующую функцию и/или пакет).

Для семплирования вам нужны функция `rnorm()` в R и пакет `numpy` в Python (там либо `numpy.random.Generator.normal`, либо `numpy.random.normal`, но это устаревшая функция).

В случаях, когда у нас есть две группы (то есть одна из переменных -- категориальная) и вторая -- интервальная (деньги, время, количество пользователей и т. д.) мы используем разные тесты, в зависимости от количества наблюдений и формы распределения. Один из самых распространенных тестов -- $t$-критерия Стьюдента:


``` r
s1 <- rnorm(100, mean = 0, sd = 1)
s2 <- rnorm(100, mean = 5, sd = 1)

t.test(s1, s2, alternative = "two.sided", var.equal = TRUE)
```

```
## 
## 	Two Sample t-test
## 
## data:  s1 and s2
## t = -37.208, df = 198, p-value < 2.2e-16
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -5.373909 -4.832949
## sample estimates:
## mean of x mean of y 
## 0.0219104 5.1253395
```

Здесь я использую метку, что дисперсии выборок одинаковые. Но это потому что я сам при генерации сэмплов указал, что `sd = 1`.


### level 4 (UV)
Задание из тестового задания на продуктового аналитика в Альфа-банк.

Был проведен эксперимент: изменение заголовка на кнопке на главном экране подписной страницы. Сделан акцент на выгоде пользователя.

<img src="./pics/alfa_ab_test_pic.jpeg" width="80%">

Описание полей:

- date – Дата
- deviceCategory – Тип устройства
- sourceMedium – Источник и канал привлечения
- experimentVariant – Группа (варианта) эксперимента: 0 - контроль, 1 - тест
- clickButtonOnMain – Кликнул/не кликнул по кнопке на главной странице в рамках сеанса (1 – кликнул, или 0 – не кликнул)
- sessionDuration – Время проведенное на сайте в рамках сеанса


Данные: [AB_ab_2_1](https://raw.githubusercontent.com/upravitelev/mar231f/refs/heads/main/data/AB_ab_2_1.csv)

Проверьте гипотезы:

- Есть ли значимое изменение в большую или меньшую сторону у клика на целевую кнопку?
- Изменилось ли время проведенное на сайте в рамках сеанса?
- Напишите, какими тестами пользовались и почему выбрали их?
- Напишите выводы, которые можно сделать на основе анализа.


Импортируем датасет и приводим к нормальному виду:

``` r
library(data.table)
dataset <- fread('https://raw.githubusercontent.com/upravitelev/mar231f/refs/heads/main/data/AB_ab_2_1.csv')
dataset[, sessionDuration := gsub(',', '.', sessionDuration, fixed = TRUE)]
dataset[, sessionDuration := as.numeric(sessionDuration)]
```

Считаем количество нажавших на кнопку в каждой группе и количество всего пользователей:

``` r
dataset_stat <- dataset[, list(n_users = uniqueN(userId)), keyby = list(experimentVariant, clickButtonOnMain)]
dataset_stat[, total_users := sum(n_users), keyby = experimentVariant]
dataset_stat
```

```
## Key: <experimentVariant>
##    experimentVariant clickButtonOnMain n_users total_users
##                <int>             <int>   <int>       <int>
## 1:                 0                 0    1293        1485
## 2:                 0                 1     192        1485
## 3:                 1                 0    1325        1458
## 4:                 1                 1     133        1458
```


Применяем тест пропорций, видим значимые различия (т.е. кнопка повлияло на количество кликов):

``` r
prop.test(c(192, 133), c(1485, 1458))
```

```
## 
## 	2-sample test for equality of proportions with continuity correction
## 
## data:  c(192, 133) out of c(1485, 1458)
## X-squared = 10.471, df = 1, p-value = 0.001213
## alternative hypothesis: two.sided
## 95 percent confidence interval:
##  0.01481732 0.06132684
## sample estimates:
##     prop 1     prop 2 
## 0.12929293 0.09122085
```


Считаем значимость различий между группами по длительности сессий:

``` r
t.test(sessionDuration ~ experimentVariant, data = dataset)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  sessionDuration by experimentVariant
## t = -45.299, df = 2217.4, p-value < 2.2e-16
## alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
## 95 percent confidence interval:
##  -13.58488 -12.45749
## sample estimates:
## mean in group 0 mean in group 1 
##        120.0380        133.0592
```

``` r
# альтернативна запись 1
# dataset[, t.test(sessionDuration ~ experimentVariant)]

# альтернативная запись 2, python-like
# t.test(
#   dataset[experimentVariant == 0, sessionDuration],
#   dataset[experimentVariant == 1, sessionDuration]
# )
```





