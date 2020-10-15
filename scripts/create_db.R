## Обрабатываем опросы и создаем массив, содержащий только те вопросы,
## которые содержатся в каждом сборнике (для пояснения см. inspect_polls.R)

source("scripts/functions.R") # Загружаем функции

# Загружаем вопросы
load("input/questions.RData")

# Номер волны
w <- 45

# Загружаем названия сборников 
polls_files <- "data" %>%
  dir_ls()

# Загружаем все требуемые сборники (начиная с волны w)
dat <- polls_files[w:length(polls_files)] %>% 
  load_data()
# map(~ .x %>% gather(question, answer, colnames(.x))) %>% 
# map2(c(names(.)), ~.x %>% mutate(date = .y))

# Массив, с которым будем работать далее
d <- dat

# Цикл для отбора необходимых вопросов
for (i in 1:length(d)) {
  # Удаляем дубликаты строк
  d[[i]] <- d[[i]] %>% 
    subset(select = which(!duplicated(names(.))))
  # Берем опрос i и вычленяем из него вопросы
  q_i <- d[[i]] %>%
    var_label(unlist = T) %>%
    as_tibble() %>% 
    mutate(value = str_squish(value))
  # Меняем названия столбцов в опросе i
  colnames(d[[i]]) <- t(q_i[,1])
  # Удаляем дубликаты строк
  d[[i]] <- d[[i]] %>% 
    subset(select = which(!duplicated(names(.))))
  # Отбираем в опросе i только столбцы с требуемыми вопросами
  d[[i]] <- d[[i]] %>% 
    select_if(colnames(d[[i]]) %in% t(questions[,1]))
  print(i) # Выводим на экран номер обработанного вопроса
}

# Добавляем в таблицы номер волны и дату проведения опроса 
d <- d %>% 
  map2(c(names(.)), ~.x %>% mutate(poll = .y))

# Переходим от листа к tibble
d <- d %>% 
  map_dfr(select_all) %>% 
  mutate(wave = as.numeric(str_extract(poll, "\\d{2}"))) %>% 
  mutate(date = str_extract(poll, "\\d{6}")) %>% 
  mutate(date = paste0("01", date)) %>% 
  mutate(date = as.Date(date, "%d%m%Y")) %>% 
  select(-poll)

# Кодируем вопросы
q_all <- colnames(d)[(-length(d)+1):(-length(d))]
questions[,1] <- q_all
questions[,2] <- as.character(questions[,2])
for (i in 1:length(q_all)) {
  questions[i,2] <- paste0("q", i)
}
questions <- questions %>% 
  rename(question = value,
         question_code = s)
colnames(d)[(-length(d)+1):(-length(d))] <- questions$question_code

# Кладём итоговые файлы в папку output
write.csv2(d, "output/polls.csv", sep = ";", dec = ".")
write.csv2(questions, "output/questions_codes.csv", sep = ";", dec = ".")
