
# level 2 (HNTR) ----------------------------------------------------------

# Постройте график накопительной конверсии в когорте июньских пользователей с разбивкой по источнику пользователей.

library(data.table)
library(plotly)

# импортируем данные
installs <- fread('https://gitlab.com/hse_mar/mar211f/-/raw/main/data/installs.csv')
payments <- fread('https://gitlab.com/hse_mar/mar211f/-/raw/main/data/payments_custom.csv')
# installs_june[media_source %in% c('organic', 'other') | is.na(media_source), media_source := 'organic']
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
conversion_stat <- conversion_stat[, list(new_payers = uniqueN(user_pseudo_id)), by = list(media_source, lifetime)]

# считаю, сколько всего было пользователей в когорте
# conversion_stat[, total_users := installs[dt < '2022-07-01', uniqueN(user_pseudo_id)]]

conversion_stat <- merge(
  installs[dt < '2022-07-01', list(total_users = uniqueN(user_pseudo_id)), by = media_source],
  conversion_stat,
  by = 'media_source', all.x = TRUE
)

installs[dt < '2022-07-01', list(total_users = uniqueN(user_pseudo_id)), by = media_source]

installs[installs['dt'] < pd.to_datetime('2022-07-01')].groupby('media_source', dropna=False).agg(
  total_users = ('user_pseudo_id', 'nunique')
).reset_index()

installs[installs['dt'] < pd.to_datetime('2022-07-01')] / 
  .groupby('media_source', dropna=False)['user_pseudo_id'] /
  .nunique() / 
  .reset_index() / 
  .rename(columns={'user_pseudo_id': 'total_users'})




# сортирую и считаю накопительное количество пользователей, которые сделали 
# первый платеж в этот день от инсталла
conversion_stat <- conversion_stat[order(media_source, lifetime)]
conversion_stat[, new_payers_cum := cumsum(new_payers), by = media_source]

# считаю накопительную конверсию в платящих
conversion_stat[, cum_conversion := new_payers_cum / total_users]

# рисую
plot_ly(
  conversion_stat, x = ~lifetime, y = ~cum_conversion, type = 'scatter', mode='lines', color = ~media_source
) %>%
  layout(
    title = 'Накопительная конверсия',
    yaxis = list(rangemode = 'tozero')
  ) %>%
  config(displayModeBar = FALSE)    




# level 3 (N) -------------------------------------------------------------

# Посчитайте по каждой платформе динамику метрики конверсии в платящих в день инсталла. 0, 3, 7, 30
# На графике на оси OX должна быть дата инсталла, на оси OY – значение конверсии в день инсталла. 
# Делать аналогично динамике ретеншена первого дня.


conversion_dyn <- merge(
  payments,
  installs[, list(user_pseudo_id, media_source, dt)],
  by = 'user_pseudo_id', all.x = TRUE
)
conversion_dyn <- conversion_dyn[!is.na(dt)]

# берем июньскую когорту и ограничиваем даты лайфтайма
conversion_dyn[, lifetime := pay_dt - dt]

# это будет день первого платежа
conversion_dyn_stat <- conversion_dyn[, list(lifetime = min(lifetime)), by = list(user_pseudo_id, dt)]

# считаю количество пользователей, которые сделали платеж на этот день
conversion_dyn_stat <- conversion_dyn_stat[, list(new_payers = uniqueN(user_pseudo_id)), 
                                           by = list(dt, lifetime)]

# считаю, сколько всего было пользователей в когорте
# conversion_stat[, total_users := installs[dt < '2022-07-01', uniqueN(user_pseudo_id)]]

conversion_dyn_stat <- merge(
  installs[, list(total_users = uniqueN(user_pseudo_id)), by = dt],
  conversion_dyn_stat,
  by = 'dt', all.x = TRUE
)


# сортирую и считаю накопительное количество пользователей, которые сделали 
# первый платеж в этот день от инсталла
conversion_dyn_stat <- conversion_dyn_stat[order(dt, lifetime)]
conversion_dyn_stat[, new_payers_cum := cumsum(new_payers), by = dt]

# считаю накопительную конверсию в платящих
conversion_dyn_stat[, cum_conversion := new_payers_cum / total_users]

# рисую
plot_ly(
  conversion_dyn_stat[lifetime %in% c(0, 3, 7, 30)], 
  x = ~dt, y = ~cum_conversion, 
  type = 'scatter', mode='lines', color = ~as.character(lifetime)
) %>%
  layout(
    title = 'Накопительная конверсия в дневных когортах',
    yaxis = list(rangemode = 'tozero')
  ) %>%
  config(displayModeBar = FALSE)    



# это будет день первого платежа
conversion_dyn_stat <- conversion_dyn[, list(lifetime = min(lifetime)), by = list(user_pseudo_id, dt, platform)]

# считаю количество пользователей, которые сделали платеж на этот день
conversion_dyn_stat <- conversion_dyn_stat[, list(new_payers = uniqueN(user_pseudo_id)), 
                                           by = list(platform, dt, lifetime)]

# считаю, сколько всего было пользователей в когорте
# conversion_stat[, total_users := installs[dt < '2022-07-01', uniqueN(user_pseudo_id)]]

conversion_dyn_stat <- merge(
  installs[, list(total_users = uniqueN(user_pseudo_id)), by = list(platform, dt)],
  conversion_dyn_stat,
  by = c('dt', 'platform'), all.x = TRUE
)


# сортирую и считаю накопительное количество пользователей, которые сделали 
# первый платеж в этот день от инсталла
conversion_dyn_stat <- conversion_dyn_stat[order(platform, dt, lifetime)]
conversion_dyn_stat[, new_payers_cum := cumsum(new_payers), by = list(platform, dt)]

# считаю накопительную конверсию в платящих
conversion_dyn_stat[, cum_conversion := new_payers_cum / total_users]

# рисую
plot_ly(
  conversion_dyn_stat[lifetime == 0], 
  x = ~dt, y = ~cum_conversion, 
  type = 'scatter', mode='lines', color = ~platform
) %>%
  layout(
    title = 'Динамика конверсии в платящих в дневных когортах в день инсталла',
    yaxis = list(rangemode = 'tozero')
  ) %>%
  config(displayModeBar = FALSE)    



arpu_fish <- data.table(
  dt = seq(as.Date('2022-06-01'), as.Date('2022-07-31'), by = 1)
)

arpu_fish <- arpu_fish[, list(lifetime = 1:30), by = dt]



