library(stats)
library(dplyr)
library(simmer)
library(simmer.plot)
library(progress)

source("./data.R")

real_tickets_flow <- get_real_tickets_flow()
lines <- lines_new
lines_type <- 'new'

set.seed(1234)

#
#
# SIMULATION
#
#
run_simulation = function() {
  env <- simmer()
  
  # Определить линию тикета по вероятности
  get_random_line_id <- function() {
    line <- sample_n(lines, size = 1, weight = lines$probability)
    return(line$id)
  }
  
  # Получить объект линии по id
  get_line_by_id <- function() {
    line_id <- get_attribute(env, "line_id")
    return(lines[lines$id == line_id, ])
  }
  
  # Определить, является ли тикет спамом в соответствии с вероятностью по линиям
  get_random_is_spam <- function() {
    line <- get_line_by_id()
    spam_probabiliy <- line$spam_probabiliy
    is_spam <- base::sample(c(0, 1), size = 1, prob = c(1 - spam_probabiliy, spam_probabiliy))
    return(is_spam)
  }
  
  # Определить, нужно ли эскалировать тикет
  get_random_escalation_type <- function() {
    line <- get_line_by_id()
    is_spam <- get_attribute(env, "is_spam") == 1
    escalation_times <- get_attribute(env, "escalation_times")
    
    if (escalation_times >= 6 || is_spam) {
      return(0)
    }
    
    multiplier <- escalation_number_probability_multiplier[escalation_times + 1]
    inline_escalation_prob <- multiplier * line$inline_escalation_probability
    outline_escalation_prob <- multiplier * line$outline_escalation_probability
    no_escalation_prob <- 1 - inline_escalation_prob - outline_escalation_prob
    
    escalation_type <- base::sample(
      c(0, 1, 2),
      size = 1,
      prob = c(no_escalation_prob, inline_escalation_prob, outline_escalation_prob)
    )
    
    return(escalation_type)
  }
  
  # Изменить линию при эскалации
  get_line_by_escalation <- function() {
    line_id <- get_attribute(env, "line_id")
    escalation_type <- get_attribute(env, "escalation_type")
    
    if (escalation_type == 1) {
      return(get_random_line_id())
    } else {
      return(line_id)
    }
  }
  
  # Увеличить счетчик эскалаций
  get_escalation_times_increment <- function() {
    escalation_type <- get_attribute(env, "escalation_type")
    escalation_times <- get_attribute(env, "escalation_times")
    
    if (escalation_type > 0) {
      return(escalation_times + 1)
    }
    
    return(escalation_times)
  }
  
  # Нужно ли по новой встать в очередь для обработки
  get_should_rollback <- function() {
    escalation_type <- get_attribute(env, "escalation_type")
    return(escalation_type > 0)
  }
  
  # Время обработки в зависимости от того, спам или не спам
  get_processing_time <- function() {
    line <- get_line_by_id()
    is_spam <- get_attribute(env, "is_spam") == 1
    escalation_type <- get_attribute(env, "escalation_type")
    
    if (is_spam) {
      return(line$spam_processing_time / 60)
    }
    
    if (escalation_type == 2) {
      return(outline_avg_processing_time / 60)
    }
    
    line_processing_time_data <- load_line_weighted_processing_times(line$name, lines_type)
    processing_time <- base::sample(
      x = line_processing_time_data$processing_times,
      size = 1,
      prob = line_processing_time_data$weights
    )
    
    return(processing_time / 60)
  }
  
  # Определить количество агентов в зависимости от смены
  get_currernt_agents_count <- function() {
    simulation_day_time <- simmer::now(env) %% day
    available_workshifts <- workshift %>%
      filter(start <= simulation_day_time & end >= simulation_day_time)
    
    return(sum(available_workshifts$count))
  }
  
  
  
  ticket_trajectory <- trajectory("Ticket processing") %>%
    # Инициализация тикета (выполняется один раз для тикета)
    set_attribute("line_id", get_random_line_id) %>%
    set_attribute("first_line_id", function() get_attribute(env, "line_id")) %>%
    set_attribute("is_spam", get_random_is_spam) %>%
    set_attribute("escalation_times", 0) %>%
    set_attribute("escalation_type", 1) %>%
    
    # Повторный проход по тикету при эскалации
    set_capacity("agent_inline", get_currernt_agents_count) %>%
    set_attribute("processing_time", get_processing_time) %>%
    branch(
      option = function() get_attribute(env, "escalation_type"),
      continue = TRUE,

      trajectory("Inline processing") %>%
        seize("agent_inline", 1) %>%
        timeout_from_attribute("processing_time") %>%
        simmer::timeout(function() rnorm(1, 0.15, 0.05)) %>% 
        simmer::release("agent_inline", 1),

      trajectory("Outline processing") %>%
        seize("agent_outline", 1) %>%
        timeout_from_attribute("processing_time") %>%
        simmer::release("agent_outline", 1)
    ) %>%
    set_attribute("escalation_type", get_random_escalation_type) %>%
    set_attribute("escalation_times", get_escalation_times_increment) %>%
    set_attribute("line_id", get_line_by_escalation) %>%
    simmer::rollback(6, check = get_should_rollback)
  
  plot(ticket_trajectory, verbose = FALSE)
  
  # Generate tickets
  env %>%
    add_generator("ticket", ticket_trajectory, at(real_tickets_flow)) %>%
    add_resource("agent_inline", 0) %>%
    add_resource("agent_outline", 200)
  
  # Run simulation
  env %>% run(5 * day, progress = progress::progress_bar$new()$update, steps = 20)
  
  return(env)
}

# Суммарная нагрузка агентов
get_resources_usage = function(env) {
  data <- env %>%
    dplyr::group_by(.data$resource, .data$replication) %>%
    dplyr::mutate(dt = .data$time - dplyr::lag(.data$time)) %>%
    dplyr::mutate(in_use = .data$dt * dplyr::lag(.data$server / .data$capacity)) %>%
    dplyr::summarise(utilization = sum(.data$in_use, na.rm = TRUE) / sum(.data$dt, na.rm=TRUE)) %>%
    dplyr::summarise(Q25 = stats::quantile(.data$utilization, .25),
                     Q50 = stats::quantile(.data$utilization, .5),
                     Q75 = stats::quantile(.data$utilization, .75))
  
  return(data)
}


env = run_simulation()


# Statistics
arrivals <- get_mon_arrivals(env)
resources <- get_mon_resources(env)

usage = get_resources_usage(resources)

# Время обработки тикета
plot(arrivals, metric = "activity_time")
# Время ожидания в очереди
plot(arrivals, metric = "waiting_time")
# Общее время нахождения тикета в системе
plot(arrivals, metric = "flow_time")


# Количество агентов, обрабатывающих тикеты
plot(resources, metric = "usage", items = "server", names = c('agent_inline'))
# Количество тикетов в очереди
plot(resources, metric = "usage", items = "queue", names = c('agent_inline'))
# Нагрузка на агентов
plot(resources, metric = "utilization", names = c('agent_inline'))

plot(resources, names = c('agent_inline'), steps = TRUE)
summary(resources)


# Reset simulation
# env %>% reset()
