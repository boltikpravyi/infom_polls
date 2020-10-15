## Определяем, сколько опросов нужно парсить, чтобы получить
## максимальное количество вопросов, которые встречаются в каждом сборнике

# Загружаем названия сборников 
polls_files <- "data" %>%
  dir_ls()

# Создаем пустой tibble, в который будем класть результаты анализа
results <- as_tibble()

# Запускаем цикл на поиск
for (i in 1:(length(polls_files)-1)) { # внешний цикл на количество анализируемых сборников
  q_all <- as_tibble() # tibble для хранения всех вопросов
  for (j in i:length(polls_files)) { # внутренний цикл на загрузку сборника
    # Извлекаем вопросы из сборника j
    q_j <- polls_files[j] %>% 
      read_sav() %>% 
      var_label(unlist = T) %>% 
      as_tibble() %>% 
      add_column(poll = polls_files[j])
    # Добавляем результаты в итоговый массив
    q_all <- bind_rows(q_all, q_j)
    # Выводим на экран название обработанного файла
    print(polls_files[j])
  }
  # Находим количество вопросов, которые встречаются в максимальном количестве сборников
  q_k <- q_all %>%
    mutate(value = str_squish(value),
           poll = as_factor(poll)) %>% 
    distinct() %>% 
    group_by(value) %>% 
    summarise(s = n()) %>% 
    dplyr::filter(s == max(s)) %>% 
    summarize(n = n(), s = max(s))
  # Сопоставляем найденные значения с количеством анализируемых сборников
  temp <- tibble(n_q = q_k[,1],
                 max_polls = q_k[,2],
                 n_files = 92 - i,
                 file_from = polls_files[i])
  # Если количество анализируемых сборников равно максимальному количеству сборников - останавливаем цикл
  if (temp$max_polls == temp$n_files) {
    print(temp)
    stop()
  }
  # ... Иначе добавляем в итоговую таблицу и продолжаем поиск
  results <- bind_rows(results, temp)
  print(i)
}

# Находим вопросы, которые встречаются в максимальном количестве сборников (с 45 волны)
questions <- q_all %>%
  mutate(value = str_squish(value),
         poll = as_factor(poll)) %>% 
  distinct() %>% 
  group_by(value) %>% 
  summarise(s = n()) %>% 
  dplyr::filter(s == max(s))

# Убираем ненужные вопросы
questions <- questions %>% 
  dplyr::filter(value != "Номер населённого пункта.") %>% 
  dplyr::filter(value != "Пол") %>% 
  dplyr::filter(value != "Почему Вы так думаете?") %>% 
  dplyr::filter(value != "Возраст") %>% 
  dplyr::filter(value != "Тип населённого пункта, где живёт респондент.") %>% 
  dplyr::filter(value != "Семейный доход по квантилям") %>% 
  dplyr::filter(value != "Доход на одного члена семьи по квантилям") %>% 
  dplyr::filter(value != "Размер домохозяйства")

# Сохраняем вопросы в формате .Rdata
save(questions, file = "input/questions.RData")
