library(data.table)

task_1_events <- fread('./data/task_1_events.csv')
task_1_events <- task_1_events[order(user_id, ts)]
task_1_events[, ts_prev := shift(ts, type = 'lag'), by = user_id]
task_1_events[, ts_diff := c(NA, diff(ts, lag = 1)), by = user_id]
task_1_events[, ts_diff_rnd := round(ts_diff, 1)]


task_1_events[, dt_diff := c(NA, diff(pdate, lag = 1)), by = user_id]

task_1_events[, session_type := ifelse(ts_diff > 30*60 | dt_diff > 0 | is.na(ts_diff), 'new_session', 'old_session')]
task_1_events[ts_diff > 30*60 | dt_diff > 0 | is.na(ts_diff), session_type2 := 'new_session']

task_1_events[ts_diff > 30*60 | dt_diff > 0 | is.na(ts_diff), session_id := seq_len(.N), by = user_id]

task_1_events[session_type2 == 'new_session', session_number := seq_len(.N), by = user_id]
task_1_events[, session_number := nafill(session_number, type = 'locf'), by = user_id]

# task_1_events[session_number <= 2, .N, by = list(ab_group, session_number)]


# user_id  - id юзера
# ab_group  - группа A/B-теста,
# start_ts  - время старта сессии,
# end_ts  - время окончания сессии,
# pdate  - дата сессии.

task_1_events <- task_1_events[, list(start_ts = min(ts), end_ts = max(ts)), by = list(user_id, ab_group, pdate, session_number)]

fwrite(task_1_events[, list(user_id, ab_group, pdate, session_number, start_ts, end_ts)], './data/task_1_events_sessions.csv')

task_1_events[session_number <= 2, uniqueN(user_id), by = list(ab_group, session_number)]

prop.test(c(2652, 2913), c(18414, 17812))
prop.test(c(308, 335), c(2500, 2500))

