library (tidyverse)
library(janitor)
theme_set(theme_light())

create_cols <-  function(df){
  df %>% 
    mutate(
      crn = CRN,
      professor = Professor,
      course = Course,
      avg_gpa = `CRS AVG`,
      n_students_enrolled = Total.x,
      instruction_method = `Instruction Method.x`,
      As = `A  (90-100)`,
      Bs = `B  (80-89)`,
      Cs = `C  (70-79)`,
      Ds = `D  (60-69)`,
      Fs = `F <60`,
      withdrawal_while_failing = WF.x,
      As_through_WFs = `A- WF`, # N grades given?
      DWFs = DWF.x, # N drops, withdrawls, and fails
      other = Other.x,
      A_plus = `A+`,
      A_regular = A,
      A_minus = `A-`,
      B_plus = `B+`,
      B_regular = B,
      B_minus = `B-`,
      C_plus = `C+`,
      C_regular = C,
      C_minus = `C-`,
      D_regular = D,
      F_regular = `F`
    ) %>% 
    filter(CRN != "Report Total:",
           avg_gpa > 0) %>% 
    select(crn:F_regular)
} 

fall_2016_raw <- read_csv("2016-fall-grades.csv")
fall_2016_detail <- read_csv("2016-fall-grades-detail.csv")
fall_2016 <- 
  fall_2016_raw %>% 
  left_join(fall_2016_detail, "CRN") %>% 
  create_cols() %>% 
  mutate(year = 2016, term = "Fall", .before = 1)
  
spring_2016_raw <- read_csv("2016-spring-grades.csv")
spring_2016_detail <- read_csv("2016-spring-grades-detail.csv")
spring_2016 <- 
  spring_2016_raw %>% 
  left_join(spring_2016_detail, "CRN") %>% 
  create_cols() %>% 
  mutate(year = 2016, term = "Spring", .before = 1)

fall_2017_raw <- read_csv("2017-fall-grades.csv")
fall_2017_detail <- read_csv("2017-fall-grades-detail.csv")
fall_2017 <- 
  fall_2017_raw %>% 
  left_join(fall_2017_detail, "CRN") %>% 
  create_cols() %>% 
  mutate(year = 2017, term = "Fall", .before = 1)

spring_2018_raw <- read_csv("2018-spring-grades.csv")
spring_2018_detail <- read_csv("2018-spring-grades-detail.csv")
spring_2018 <- 
  spring_2018_raw %>% 
  left_join(spring_2018_detail, "CRN") %>% 
  create_cols() %>% 
  mutate(year = 2018, term = "Spring", .before = 1)
  

fall_2018_raw <- read_csv("2018-fall-grades.csv")
fall_2018_detail <- read_csv("2018-fall-grades-detail.csv")
fall_2018 <- 
  fall_2018_raw %>% 
  left_join(fall_2018_detail, "CRN") %>% 
  create_cols() %>% 
  mutate(year = 2018, term = "Fall", .before = 1)

spring_2019_raw <- read_csv("2019-spring_grades.csv")
spring_2019_detail <- read_csv("2019-spring-grades-detail.csv")
spring_2019 <- 
  spring_2019_raw %>% 
  left_join(spring_2019_detail, "CRN") %>% 
  create_cols() %>% 
  mutate(year = 2019, term = "Spring", .before = 1)

fall_2019_raw <- read_csv("2019-fall-grades.csv")
fall_2019_detail <- read_csv("2019-fall-grades-detail.csv")
fall_2019 <- 
  fall_2019_raw %>% 
  left_join(fall_2019_detail, "CRN") %>% 
  create_cols() %>% 
  mutate(year = 2019, term = "Fall", .before = 1)

spring_2020_raw <- read_csv("2020-spring-grades.csv")
spring_2020_detail <- read_csv("2020-spring-grades-detail.csv")
spring_2020 <- 
  spring_2020_raw %>% 
  left_join(spring_2020_detail, "CRN") %>% 
  create_cols() %>% 
  mutate(year = 2020, term ="Spring", .before = 1) 

fall_2020_raw <- read_csv("2020-fall-grades.csv")
fall_2020_detail <- read_csv("2020-fall-grades-detail.csv")
fall_2020 <- 
  fall_2020_raw %>% 
  left_join(fall_2019_detail, "CRN") %>% 
  create_cols() %>% 
  mutate(year = 2020, term = "Fall", .before = 1) 

spring_2021_raw <- read_csv("2021-spring-grades.csv")
spring_2021_detail <- read_csv("2021-spring-grades-detail.csv")
spring_2021 <- 
  spring_2021_raw %>% 
  left_join(spring_2020_detail, "CRN") %>% 
  create_cols() %>% 
  mutate(year = 2021, term = "Spring", .before = 1)

fall_2021_raw <- read_csv("2021-fall-grades.csv")
fall_2021_detail <- read_csv("2021-fall-grades-detail.csv")
fall_2021 <- 
  fall_2021_raw %>% 
  left_join(fall_2021_detail, "CRN") %>% 
  create_cols() %>% 
  mutate(year = 2021, term = "Fall", .before = 1)

spring_2022_raw <- read_csv("2022-spring-grades.csv")
spring_2022_detail <- read_csv("2022-spring-grades-detail.csv")
spring_2022 <- 
  spring_2022_raw %>% 
  left_join(spring_2022_detail, "CRN") %>% 
  create_cols() %>% 
  mutate(year = 2022, term = "Spring", .before = 1)

labels <-
  c("Fall 2017", "Spring 2018", "Fall 2018","Spring 2019", "Fall 2019",
    "Spring 2020", "Fall 2020", "Spring 2021", "Fall 2021", "Spring 2022")

df <-
  bind_rows(
    fall_2017,
    spring_2018,
    fall_2018,
    spring_2019,
    fall_2019,
    spring_2020,
    fall_2020,
    spring_2021,
    fall_2021,
    spring_2022
  ) %>%
  mutate(
    term_year = paste(term, year)) %>% 
  mutate(
    term_year_ordered = case_when(
      term_year == "Fall 2017" ~ 1,
      term_year == "Spring 2018" ~ 2,
      term_year == "Fall 2018" ~ 3,
      term_year == "Spring 2019" ~ 4,
      term_year == "Fall 2019" ~ 5,
      term_year == "Spring 2020" ~ 6,
      term_year == "Fall 2020" ~ 7,
      term_year == "Spring 2021" ~ 8,
      term_year == "Fall 2021" ~ 9,
      term_year == "Spring 2022" ~ 10
    )) %>% 
  mutate(
    course_modality = case_when(
      instruction_method == "T" ~ "Face-to-Face",
      instruction_method == "E" ~ "Online",
      instruction_method == "H" ~ "Hybrid",
      instruction_method == "P" ~ "Partially Distance + 50% Tech",
      instruction_method == "F" ~ "Fully Distance 95% Tech",
      TRUE ~ as.character("?")),
    .after = instruction_method) %>%
  mutate(
    course_prefix = str_replace_all(course, "[:digit:]", ""),
    course_pnbr = (str_remove_all(course, "\\D+")),
    course_level = case_when(
      as.numeric(course_pnbr) >= 7000 ~ "Graduate",
      as.numeric(course_pnbr) < 7000 ~ "Undergraduate"
    )
  ) %>%
  mutate(
    term_year = ordered(term_year_ordered, levels = 1:10, labels = labels)
    )

df %>%
  ggplot(aes(term_year, avg_gpa,
             color = as.character(term_year))) +
  geom_vline(xintercept = 5.75,
             size = 2,
             color = "red") +
  geom_jitter(width = .1, alpha = .25) +
  stat_summary(fun = mean, geom = "crossbar") +
  scale_x_discrete(labels = label_wrap(9)) +
  theme(legend.position = "none") +
  labs(
    x = "Term",
    y = "Average avg_gpa",
    title  = "Was there a change in average avg_gpa after the pandemic?",
    caption = "Red line indicates Pandemic"
  )

fall <-
  df %>%
  filter(term == "Fall", year == 2019)

spring <-
  df %>%
  filter(term == "Spring", year == 2020)

fall_vs_spring <-
  fall %>%
  full_join(spring, by = c("professor", "course")) %>%
  mutate(diff = avg_gpa.y - avg_gpa.x,
         academic_year = paste(year.x, sep = "-", year.y),
         course_modality = course_modality.x,
         course_level = course_level.x) %>%
  filter(!is.na(diff)) %>% 
  rename_with(~ gsub(".x", "_fall", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub(".y", "_spring", .x, fixed = TRUE))

write_csv(fall_vs_spring, "fall_vs_spring.csv")


spring_vs_spring <- 
  spring_2019 %>% 
  full_join(spring_2020, by = c("professor", "course")) %>% 
  mutate(diff = avg_gpa.y - avg_gpa.x) %>% 
  filter(!is.na(diff)) %>% 
  rename_with(~ gsub(".x", "_fall", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub(".y", "_spring", .x, fixed = TRUE))

write_csv(spring_vs_spring, "spring_vs_spring.csv")
