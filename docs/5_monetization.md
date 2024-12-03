# Метрики монетизации pt.2 {#c5_monetization}

## Запись занятия {-}

<iframe width="560" height="315" src="https://www.youtube.com/embed/16-9hKo7ulw?si=nzfR9xjtS8FC6iw2" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

## Код занятия на Python
https://colab.research.google.com/drive/1tAAkLUsyck3OJE-994owPh3TIGJI_qDX?usp=sharing


## Разбор домашнего задания

### level 4 (UV)

Постройте график накопительной конверсии с разбивкой по источнику пользователей.



``` r
library(data.table)
library(plotly)

# импортируем данные
# installs <- fread('https://gitlab.com/hse_mar/mar211f/-/raw/main/data/installs.csv')
installs <- fread('./data/installs.csv')

# payments <- fread('https://gitlab.com/hse_mar/mar211f/-/raw/main/data/payments_custom.csv')
payments <- fread('./data/payments.csv')



# к платежам присоединяем дату инсталла и источник трафика
conversion <- merge(
  payments,
  installs[, list(user_pseudo_id, media_source, dt)],
  by = 'user_pseudo_id', all.x = TRUE
)

# берем июньскую когорту и ограничиваем даты лайфтайма
conversion[, lifetime := pay_dt - dt]
conversion <- conversion[dt < '2022-07-01']
conversion <- conversion[lifetime >= 0 & lifetime <= 30]

# беру минимальный день от инсталла
# это будет день первого платежа
conversion_stat <- conversion[, list(lifetime = min(lifetime)), by = list(user_pseudo_id, media_source)]

# считаю количество пользователей, которые сделали платеж на этот день
conversion_stat <- conversion_stat[, list(new_payers = uniqueN(user_pseudo_id)), by = list(lifetime)]

# считаю, сколько всего было пользователей в когорте
conversion_stat[, total_users := installs[dt < '2022-07-01', uniqueN(user_pseudo_id)]]

# сортирую и считаю накопительное количество пользователей, которые сделали 
# первый платеж в этот день от инсталла
conversion_stat <- conversion_stat[order(lifetime)]
conversion_stat[, new_payers_cum := cumsum(new_payers)]

# считаю накопительную конверсию в платящих
conversion_stat[, cum_conversion := new_payers_cum / total_users]

# рисую
plot_ly(
  conversion_stat, x = ~lifetime, y = ~cum_conversion, type = 'scatter', mode='lines'
) %>%
  layout(
    title = 'Накопительная конверсия',
    yaxis = list(rangemode = 'tozero')
  ) %>%
  config(displayModeBar = FALSE)    
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-fe08298fb3b2856b0526" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-fe08298fb3b2856b0526">{"x":{"visdat":{"cc6f1b4445db":["function () ","plotlyVisDat"]},"cur_data":"cc6f1b4445db","attrs":{"cc6f1b4445db":{"x":{},"y":{},"mode":"lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Накопительная конверсия","yaxis":{"domain":[0,1],"automargin":true,"rangemode":"tozero","title":"cum_conversion"},"xaxis":{"domain":[0,1],"automargin":true,"title":"lifetime"},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"data":[{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],"y":[0.0073027622314497205,0.01172594331106698,0.013901426250225673,0.015390864777035567,0.016419931395558765,0.017385809712944574,0.017981585123668532,0.018586387434554975,0.019064813143166638,0.019570319552265753,0.020021664560389962,0.020337606066076909,0.020590359270626468,0.020852139375338509,0.021104892579888065,0.021285430583137751,0.02152012998736234,0.021691641090449541,0.021845098393211772,0.02203466329662394,0.022188120599386171,0.022341577902148402,0.022495035204910634,0.022630438707347898,0.022774869109947644,0.022946380213034846,0.023036649214659685,0.02315399891677198,0.023253294818559308,0.023343563820184147,0.023433832821808991],"mode":"lines","type":"scatter","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```


## ARPU / ARPPU

Averange revenue per user - сумма платежей за определенный период, деленная на общее количество пользователей когорты. Средний чек, наверное, одна из самых важных метрик для оперирования продуктом, так как изучение структуры ARPU позволяет понять, за что платят пользователи и как можно улучшить эту метрику и так далее.

Average revenue per paying user - сумма платежей за определенный период, деленная на количество платящих пользователей когорты.

Обе метрики считаются в определенном окне (количестве дней от инсталла) - обычно 7, 28 или 30 дней. Это необходимо для того, чтобы учесть ситуацию, когда пользователи одной когорты (месячной, например) могли прожить разное количество дней в приложении. Или когда необходимо сравнить разные каналы привлечения, рекламные кампании или группы аб-тестов.

Для оценки динамики метрики и приянтия продуктовых решений (на какой день от инсталла что-то сломалось) часто рисуют кривую кумулятивного ARPU.


``` r
# расчет во многом похож на накопительную конверсию, так как тоже считается накопительно
# но не количество пользователей, сделавших первый платеж, а просто выручка

# повторяющийся блок
# к платежам присоединяем дату инсталла и источник трафика
conversion <- merge(
  payments,
  installs[, list(user_pseudo_id, media_source, dt)],
  by = 'user_pseudo_id', all.x = TRUE
)

# берем июньскую когорту и ограничиваем даты лайфтайма
conversion[, lifetime := pay_dt - dt]
conversion <- conversion[dt < '2022-07-01']
conversion <- conversion[lifetime >= 0 & lifetime <= 30]

# считаем выручку по дням от инсталла
arpu_stat <- conversion[, list(gross = sum(gross)), by = list(lifetime)]

# считаем количество уникальных пользователей в июньской когорте
arpu_stat[, total_users := installs[dt < '2022-07-01', uniqueN(user_pseudo_id)]]

# сортируем и считаем кумулятивную выручку
arpu_stat <- arpu_stat[order(lifetime)]
arpu_stat[, gross_cum := cumsum(gross)]

# считаем кумулятивное ARPU
arpu_stat[, cARPU := gross_cum / total_users]

# рисуем
plot_ly(
  arpu_stat, x = ~lifetime, y = ~cARPU, type = 'scatter', mode='lines'
) %>%
  layout(
    title = 'Cumulative ARPU, installs in June',
    yaxis = list(rangemode = 'tozero')
  ) %>%
  config(displayModeBar = FALSE)   
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-8e026d5d80304385b5bb" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-8e026d5d80304385b5bb">{"x":{"visdat":{"cc6f63b7db55":["function () ","plotlyVisDat"]},"cur_data":"cc6f63b7db55","attrs":{"cc6f63b7db55":{"x":{},"y":{},"mode":"lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Cumulative ARPU, installs in June","yaxis":{"domain":[0,1],"automargin":true,"rangemode":"tozero","title":"cARPU"},"xaxis":{"domain":[0,1],"automargin":true,"title":"lifetime"},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"data":[{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],"y":[0.097209063007760971,0.1706717819100883,0.22451408196424869,0.26283164831196432,0.29589402419208666,0.32977586206895942,0.35704251669975889,0.3838098934825715,0.4105041523740679,0.43589014262501563,0.45799241740385643,0.47731187940060682,0.4972793825600218,0.51515851236684618,0.53536441595955242,0.55533905037009579,0.57461617620508409,0.5913503339952989,0.6087176385629105,0.62550171511102381,0.64529716555334193,0.66324092796532963,0.68160101101281123,0.69951769272431141,0.71527179996388546,0.73064831196966262,0.74867503159414361,0.76522386712402268,0.77883074562194643,0.79829409640728677,0.81135204910632996],"mode":"lines","type":"scatter","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

## Полезные материалы {-}

[Основные метрики мобильных приложений](https://apptractor.ru/measure/user-analytics/osnovnyie-metriki-mobilnyih-prilozheniy.html) Очень обзорный материал от devtodev. Есть неплохой блок по метрикам монетизации.

## Домашнее задание

### level 1 (IATYTD)

Внимательно разберите материалы конспекта.


### level 2 (HNTR)

Постройте график накопительной конверсии в когорте июньских пользователей с разбивкой по источнику пользователей.

### level 3 (N)

Посчитайте по каждой платформе динамику метрики конверсию в платящих в день инсталла.
На графике на оси OX должна быть дата инсталла, на оси OY -- значение конверсии в день инсталла.
Делать аналогично динамике ретеншена первого дня.


### level 4 (UV)

Постройте и нарисуйте структуру накопительного ARPU для июньских пользователей в зависимости оттого, какие offer_type покупали пользователи. Таким образом мы можем понять, какая товарная категория делает наибольший вклад в кумулятивное ARPU.

ИЛИ

Постройте график накопительного ARPU в когорте июньских пользователей с разбивкой по источнику пользователей.


### level 5 (N)

Посчитайте по каждой платформе динамику ARPU 0, 1, 7 и 30 дней (сколько в среднем заплатили пользователи когорты в день инсталла, за 0 и 1 дни жизни в приложении, за первые 7 дней жизни, за первые 30 дней жизни в приложении). 
На графике на оси OX должна быть дата инсталла, на оси OY -- значение ARPU, с разбивкой, по какому количеству дней от инсталла мы это считаем

Делать аналогично динамике ретеншена, я показывал на занятии про ретеншен как раз близкое решение.



