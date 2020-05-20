library(lubridate)
library(stringr)
library(memoise)

# Рассматриваемые параметры линий
# + Вероятность поступления тикета на линию
# + Вероятность того, что поступивший тикет является спамом
# + Вероятность эскалации внутри линии
# + Вероятность эскалации во внешние линии
# + Время обработки тикетов (сек)
# + Время обработки тикетов спама (сек)

# Параметры линий (old)
lines_old <- data.frame(
  id = 1:7,
  name = c("skill_1", "skill_2", "skill_3", "skill_6", "skill_4", "skill_5", "skill_7"),

  probability = c(0.4297, 0.1751, 0.1409, 0.0967, 0.0818, 0.0691, 0.0068),
  spam_probabiliy = c(0.1196, 0.1501, 0.1295, 0.1163, 0.1275, 0.1759, 0.1353),
  inline_escalation_probability = c(0.0833, 0.1366, 0.055, 0.0238, 0.0608, 0.0096, 0.2695),
  outline_escalation_probability = c(0.081, 0.1365, 0.2235, 0.2453, 0.4317, 0.2434, 0.1035),
  spam_processing_time = c(48.94, 63.97, 89.53, 82.30, 38.77, 49.43, 68.35) # seconds
)

# Параметры линий (new)
lines_new <- data.frame(
  id = 1:9,
  name = c("skill_9", "skill_11", "skill_10", "skill_13", "skill_12", "skill_14", "skill_8", "skill_15", "skill_16"),

  probability = c(0.0511, 0.1841, 0.1195, 0.1909, 0.1615, 0.0971, 0.1095, 0.0798, 0.0065),
  spam_probabiliy = c(0.1798, 0.1804, 0.3725, 0.0583, 0.2662, 0.0725, 0.1166, 0.1306, 0.0713),
  inline_escalation_probability = c(0.2396, 0.0609, 0.0826, 0.0408, 0.0402, 0.0351, 0.1098, 0.0104, 0.0662),
  outline_escalation_probability = c(0.2513, 0.3115, 0.1837, 0.0691, 0.3457, 0.1893, 0.3159, 0.0492, 0.5827),
  spam_processing_time = c(81.35, 52.77, 73.4, 47.87, 49.58, 47.22, 78.16, 29, 19.27) # seconds
)



# Среднее время обработки тикетов в другой линии
outline_avg_processing_time <- 514.49

# Максимально разрешенное количество эскалаций - 6
max_escalations <- 6
escalation_number_probability_multiplier <- c(1, 0.8508, 0.3779, 0.1097, 0.1097, 0.1114)

# Смены в службе поддержки
hour <- 60 # Итераций в часу
day <- 1440 # Итераций в сутках
workshift <- data.frame(
  start = c(0 * hour, 7 * hour, 8 * hour, 10 * hour, 12 * hour, 14 * hour, 22 * hour),
  end = c(7 * hour, 16 * hour, 17 * hour, 19 * hour, 21 * hour, 22 * hour, 24 * hour),
  count = c(10, 13, 6, 22, 5, 17, 10)
)

# Входящий поток тикетов
get_real_tickets_flow <- function() {
  data <- read.csv("data/tickets-first-line2.csv")
  data$create_date <- str_replace_all(data$create_date, "T", " ")
  data$create_date <- sort(as_datetime(data$create_date))

  start <- data$create_date[1]
  second(start) <- 0
  minute(start) <- 0
  hour(start) <- 0

  arrival_time <- as.duration(start %--% data$create_date) / dminutes(1)

  return(as.numeric(arrival_time))
}

# Удаление выбросов
remove_outliers <- function(data) {
  qnt <- quantile(data, probs = c(1 / 4, 3 / 4))
  iqr_dist <- 1.5 * IQR(data)
  left <- qnt[1] - iqr_dist
  right <- qnt[2] + iqr_dist

  return(data[data >= left & data <= right])
}

# Загрузка весов времени обработки
load_line_weighted_processing_times <- memoise(function(line_name, type = "old") {
  line_name_modified <- line_name %>%
    str_to_lower() %>%
    str_replace_all(" ", "_")
  data <- read.csv(str_glue("data/{type}/{line_name_modified}_processing_time.csv"))
  data_refined <- remove_outliers(data$processing_time)
  data_weighted <- table(data_refined) / length(data_refined)

  return(list(
    original = data$processing_time,
    data = data_refined,
    weights = data_weighted,
    processing_times = as.numeric(names(data_weighted))
  ))
})
