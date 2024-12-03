# Метрики монетизации pt.4 {#c7_monetization}

## Запись занятия {-}

<iframe width="560" height="315" src="https://www.youtube.com/embed/3JY43wF9mVM?si=9DNqdaVefwRm6TmP" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

## Код занятия на Python

https://colab.research.google.com/drive/1V4x-LdknR1vVhgrwCIvzsQjHpN1xmOrj


## Разбор домашнего задания

Датасеты:

``` r
library(data.table)
library(plotly)

Sys.setlocale('LC_ALL', 'en_US.UTF-8')
```

```
## [1] "LC_CTYPE=en_US.UTF-8;LC_NUMERIC=C;LC_TIME=en_US.UTF-8;LC_COLLATE=en_US.UTF-8;LC_MONETARY=en_US.UTF-8;LC_MESSAGES=en_US.UTF-8;LC_PAPER=ru_RU.UTF-8;LC_NAME=C;LC_ADDRESS=C;LC_TELEPHONE=C;LC_MEASUREMENT=ru_RU.UTF-8;LC_IDENTIFICATION=C"
```

``` r
# импортируем данные
# installs <- fread('https://gitlab.com/hse_mar/mar211f/-/raw/main/data/installs.csv')
installs <- fread('./data/installs.csv')

# payments <- fread('https://gitlab.com/hse_mar/mar211f/-/raw/main/data/payments_custom.csv')
payments <- fread('./data/payments.csv')


# делаем рыбу, чтобы учесть потерянные дни, в которых не было платежей
arpu_fish <- data.table(
  dt = seq(as.Date('2022-06-01'), as.Date('2022-07-31'), by = 1)
)
arpu_fish <- arpu_fish[, list(lifetime = 0:30), by = dt]


# корректируем медиасорсы
installs[media_source %in% c('organic', 'other', '') | is.na(media_source), media_source := 'organic']
```

### level 2 (HNTR)

Рассчитайте табличку с метриками монетизации для июньской когорты. Сделайте разбивку по платформам. Попробуйте проинтерпретировать результаты.


``` r
# выделяем инсталлы в июне
installs_june <- installs[dt >= '2022-06-01' & dt < '2022-07-01']

installs_june_stat <- installs_june[, list(total_users = uniqueN(user_pseudo_id)), by = list(media_source)]

installs_june_stat
```

```
##         media_source total_users
##               <char>       <int>
## 1:      applovin_int       36714
## 2:           organic       43070
## 3:      unityads_int       21932
## 4: googleadwords_int        7767
## 5:      Facebook Ads        1297
```


``` r
# к платежам присоединяем дату инсталла и источник трафика
payments_june <- merge(
  installs_june[, list(user_pseudo_id, media_source, dt)],
  payments,  
  by = 'user_pseudo_id', all = FALSE
)

# берем июньскую когорту и ограничиваем даты лайфтайма
payments_june[, lifetime := pay_dt - dt]
payments_june <- payments_june[lifetime >= 0 & lifetime <= 30]

payments_june_stat <- payments_june[, list(
  payers_30 = uniqueN(user_pseudo_id), 
  gross_30 = sum(gross),
  n_transactions_30 = length(ts)
), by = list(media_source)]

payments_june_stat
```

```
##         media_source payers_30 gross_30 n_transactions_30
##               <char>     <int>    <num>             <int>
## 1:           organic      1000 39958.64              4444
## 2:      unityads_int       262  6061.02               974
## 3:      applovin_int      1120 38554.59              4583
## 4:      Facebook Ads        53   914.41               103
## 5: googleadwords_int       161  4392.92               518
```


``` r
users_june_stat = merge(
    installs_june_stat,
    payments_june_stat,
    by = 'media_source', all.x = TRUE
)

users_june_stat[, gross_30 := round(gross_30)]
users_june_stat[, Conversion_30 := paste0(round(payers_30 * 100 / total_users, 1), '%')]
users_june_stat[, ARPU_30 := round(gross_30 / total_users, 3)]
users_june_stat[, ARPPU_30 := round(gross_30 / payers_30, 3)]
users_june_stat[, Av.Check_30 := round(gross_30 / n_transactions_30, 1)]
users_june_stat[, Av.Purchases_30 := round(n_transactions_30 / payers_30, 1)]

users_june_stat[, CPI := c(0.5, 2, .9, NA, 0.3)]
users_june_stat[, RoAS := round(ARPU_30 / CPI, 3)]

kableExtra::kable(users_june_stat)
```



|media_source      | total_users| payers_30| gross_30| n_transactions_30|Conversion_30 | ARPU_30| ARPPU_30| Av.Check_30| Av.Purchases_30| CPI|  RoAS|
|:-----------------|-----------:|---------:|--------:|-----------------:|:-------------|-------:|--------:|-----------:|---------------:|---:|-----:|
|Facebook Ads      |        1297|        53|      914|               103|4.1%          |   0.705|   17.245|         8.9|             1.9| 0.5| 1.410|
|applovin_int      |       36714|      1120|    38555|              4583|3.1%          |   1.050|   34.424|         8.4|             4.1| 2.0| 0.525|
|googleadwords_int |        7767|       161|     4393|               518|2.1%          |   0.566|   27.286|         8.5|             3.2| 0.9| 0.629|
|organic           |       43070|      1000|    39959|              4444|2.3%          |   0.928|   39.959|         9.0|             4.4|  NA|    NA|
|unityads_int      |       21932|       262|     6061|               974|1.2%          |   0.276|   23.134|         6.2|             3.7| 0.3| 0.920|




### level 3 (HMP)

Рассчитайте табличку с метриками монетизации для июньской и июльской когорт (должно быть две строки в табличке, отдельно на каждую когорту). Выберите правильный период лайфтайма. Попробуйте проинтерпретировать результаты.


``` r
installs_jj <- installs[dt < '2022-07-25']
installs_jj[, month := strftime(dt, '%B')]

installs_jj_stat <- installs_jj[, list(total_users = uniqueN(user_pseudo_id)), by = list(month)]

# к платежам присоединяем дату инсталла и источник трафика
payments_jj <- merge(
  installs_jj[, list(user_pseudo_id, month, dt)],
  payments,  
  by = 'user_pseudo_id', all = FALSE
)

# берем июньскую когорту и ограничиваем даты лайфтайма
payments_jj[, lifetime := pay_dt - dt]
payments_jj <- payments_jj[lifetime >= 0 & lifetime <= 7]

payments_jj_stat <- payments_jj[, list(
  payers_7 = uniqueN(user_pseudo_id), 
  gross_7 = sum(gross),
  n_transactions_7 = length(ts)
), by = list(month)]

users_jj_stat = merge(
    installs_jj_stat,
    payments_jj_stat,
    by = 'month', all.x = TRUE
)

users_jj_stat[, gross_7 := round(gross_7)]
users_jj_stat[, Conversion_7 := paste0(round(payers_7 * 100 / total_users, 1), '%')]
users_jj_stat[, ARPU_7 := round(gross_7 / total_users, 3)]
users_jj_stat[, ARPPU_7 := round(gross_7 / payers_7, 3)]
users_jj_stat[, Av.Check_7 := round(gross_7 / n_transactions_7, 1)]
users_jj_stat[, Av.Purchases_7 := round(n_transactions_7 / payers_7, 1)]

kableExtra::kable(users_jj_stat)
```



|month | total_users| payers_7| gross_7| n_transactions_7|Conversion_7 | ARPU_7| ARPPU_7| Av.Check_7| Av.Purchases_7|
|:-----|-----------:|--------:|-------:|----------------:|:------------|------:|-------:|----------:|--------------:|
|July  |       11843|      217|    4582|              757|1.8%         |  0.387|  21.115|        6.1|            3.5|
|June  |      110780|     2059|   42518|             5217|1.9%         |  0.384|  20.650|        8.1|            2.5|





### level 4 (UV)

Постройте воронку платежей для июньской когорты. Сделайте разбивку по платформам. Попробуйте проинтерпретировать результаты.

``` r
# выше мы уже делали вычисление дней от инсталла и фильтрацию на 30 дней
payments_june <- payments_june[order(user_pseudo_id, ts)]
payments_june[, purchase_number := seq_len(.N), by = user_pseudo_id]

# считаем, сколько пользователей сделало платеж с этим номером
payments_funnel <- payments_june[, list(n_users = uniqueN(user_pseudo_id)), keyby = purchase_number]

# считаем посчитать долю от всего пользователей, сделавших платеж (purchase_number == 1)
payments_funnel[, total_payers := n_users[purchase_number == 1]]

# если у нас есть группировка, то надо отдельно считать и мерджить по ключу

# рисуем
payments_funnel[, share := n_users / total_payers]

plot_ly(payments_funnel[purchase_number <= 10], 
        x = ~purchase_number, y = ~share, type = 'bar') %>%
  layout(
    title = 'Воронка платежей'
  ) %>%
  config(displayModeBar = FALSE)  
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-6a7cd9ee4ac5deef7a07" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-6a7cd9ee4ac5deef7a07">{"x":{"visdat":{"ccd4693fa7fc":["function () ","plotlyVisDat"]},"cur_data":"ccd4693fa7fc","attrs":{"ccd4693fa7fc":{"x":{},"y":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Воронка платежей","xaxis":{"domain":[0,1],"automargin":true,"title":"purchase_number"},"yaxis":{"domain":[0,1],"automargin":true,"title":"share"},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"data":[{"x":[1,2,3,4,5,6,7,8,9,10],"y":[1,0.53620955315870567,0.35978428351309705,0.26771956856702617,0.2083975346687211,0.17488443759630201,0.14291217257318953,0.12326656394453005,0.1059322033898305,0.095916795069337438],"type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

Воронки можно считать не от первого шага, а от предыдущего. В некоторых случаях это удобнее и информативнее.

``` r
# если хотим считать от предыдущего шага
payments_funnel[, prev_users := shift(n_users, n = 1)]
payments_funnel[, prev_share := n_users / prev_users]

plot_ly(payments_funnel[purchase_number <= 10], 
        x = ~purchase_number, y = ~prev_share, type = 'bar') %>%
  layout(
    title = 'Воронка платежей, доля от предыдущего'
  ) %>%
  config(displayModeBar = FALSE)
```

```
## Warning: Ignoring 1 observations
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-22f0ee53f08c8d7a54f2" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-22f0ee53f08c8d7a54f2">{"x":{"visdat":{"ccd458c7eb92":["function () ","plotlyVisDat"]},"cur_data":"ccd458c7eb92","attrs":{"ccd458c7eb92":{"x":{},"y":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Воронка платежей, доля от предыдущего","xaxis":{"domain":[0,1],"automargin":true,"title":"purchase_number"},"yaxis":{"domain":[0,1],"automargin":true,"title":"prev_share"},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"data":[{"x":[2,3,4,5,6,7,8,9,10],"y":[0.53620955315870567,0.67097701149425293,0.74411134903640253,0.77841726618705032,0.83918669131238444,0.81718061674008813,0.86253369272237201,0.859375,0.9054545454545454],"type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```


Обе воронки сразу

``` r
plot_ly(payments_funnel[purchase_number <= 10], 
        x = ~purchase_number, y = ~share, type = 'bar', name = '% from payers') %>%
  add_trace(y = ~prev_share, name = '% from prev') %>%
  layout(
    title = 'Воронка платежей'
  ) %>%
  config(displayModeBar = FALSE)  
```

```
## Warning: Ignoring 1 observations
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-f7040989518789fcf96b" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-f7040989518789fcf96b">{"x":{"visdat":{"ccd47ed43fb3":["function () ","plotlyVisDat"]},"cur_data":"ccd47ed43fb3","attrs":{"ccd47ed43fb3":{"x":{},"y":{},"name":"% from payers","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"},"ccd47ed43fb3.1":{"x":{},"y":{},"name":"% from prev","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Воронка платежей","xaxis":{"domain":[0,1],"automargin":true,"title":"purchase_number"},"yaxis":{"domain":[0,1],"automargin":true,"title":"share"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"data":[{"x":[1,2,3,4,5,6,7,8,9,10],"y":[1,0.53620955315870567,0.35978428351309705,0.26771956856702617,0.2083975346687211,0.17488443759630201,0.14291217257318953,0.12326656394453005,0.1059322033898305,0.095916795069337438],"name":"% from payers","type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[2,3,4,5,6,7,8,9,10],"y":[0.53620955315870567,0.67097701149425293,0.74411134903640253,0.77841726618705032,0.83918669131238444,0.81718061674008813,0.86253369272237201,0.859375,0.9054545454545454],"name":"% from prev","type":"bar","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```








