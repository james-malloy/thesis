library (tidyverse)
library(janitor)
theme_set(theme_light())

create_cols <-  function(df){
  df %>% 
    mutate(
      crn = CRN,
      year = year,
      term = term,
      professor = Professor,
      course = Course,
      As = `A  (90-100)`,
      Bs = `B  (80-89)`,
      Cs = `C  (70-79)`,
      Ds = `D  (60-69)`,
      total_Fs = `F <60`,
      withdraw_fail = WF.x,
      As_through_WFs = `A- WF`, # N grades given?
      dwf = DWF.x, # N drops, withdrawls, and fails
      avg_gpa = `CRS AVG`,
      other = Other.x,
      n_students_enrolled = Total.x,
      instruction_method_raw = `Instruction Method.x`,
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
    select(35:62)
} 

fall_2016_raw <- read_csv("2016-fall-grades.csv")
fall_2016_detail <- read_csv("2016-fall-grades-detail.csv")
fall_2016 <- 
  fall_2016_raw %>% 
  left_join(fall_2016_detail, "CRN") %>% 
  mutate(year = 2016, term = "Fall") %>% 
  create_cols()
fall_2016_detail %>% count(`Instruction Method`)
fall_2016_raw %>% count(`Instruction Method`)
fall_2016 %>% count(instruction_method_raw)

spring_2016_raw <- read_csv("2016-spring-grades.csv")
spring_2016_detail <- read_csv("2016-spring-grades-detail.csv")
spring_2016 <- 
  spring_2016_raw %>% 
  left_join(spring_2016_detail, "CRN") %>% 
  mutate(year = 2016, term = "Spring") %>% 
  create_cols()

fall_2017_raw <- read_csv("2017-fall-grades.csv")
fall_2017_detail <- read_csv("2017-fall-grades-detail.csv")
fall_2017 <- 
  fall_2017_raw %>% 
  left_join(fall_2017_detail, "CRN") %>% 
  mutate(year = 2017, term = "Fall") %>% 
  create_cols()

spring_2018_raw <- read_csv("2018-spring-grades.csv")
spring_2018_detail <- read_csv("2018-spring-grades-detail.csv")
spring_2018 <- 
  spring_2018_raw %>% 
  left_join(spring_2018_detail, "CRN") %>% 
  mutate(year = 2018, term = "Spring") %>% 
  create_cols()

fall_2018_raw <- read_csv("2018-fall-grades.csv")
fall_2018_detail <- read_csv("2018-fall-grades-detail.csv")
fall_2018 <- 
  fall_2018_raw %>% 
  left_join(fall_2018_detail, "CRN") %>% 
  mutate(year = 2018, term = "Fall") %>% 
  create_cols()

spring_2019_raw <- read_csv("2019-spring_grades.csv")
spring_2019_detail <- read_csv("2019-spring-grades-detail.csv")
spring_2019 <- 
  spring_2019_raw %>% 
  left_join(spring_2019_detail, "CRN") %>% 
  mutate(year = 2019, term = "Spring") %>% 
  create_cols()

fall_2019_raw <- read_csv("2019-fall-grades.csv")
fall_2019_detail <- read_csv("2019-fall-grades-detail.csv")
fall_2019 <- 
  fall_2019_raw %>% 
  left_join(fall_2019_detail, "CRN") %>% 
  mutate(year = 2019, term = "Fall") %>% 
  create_cols()

spring_2020_raw <- read_csv("2020-spring-grades.csv")
spring_2020_detail <- read_csv("2020-spring-grades-detail.csv")
spring_2020 <- 
  spring_2020_raw %>% 
  left_join(spring_2020_detail, "CRN") %>% 
  mutate(year = 2020, term ="Spring") %>% 
  create_cols()

fall_2020_raw <- read_csv("2020-fall-grades.csv")
fall_2020_detail <- read_csv("2020-fall-grades-detail.csv")
fall_2020 <- 
  fall_2020_raw %>% 
  left_join(fall_2019_detail, "CRN") %>% 
  mutate(year = 2020, term = "Fall") %>% 
  create_cols()

spring_2021_raw <- read_csv("2021-spring-grades.csv")
spring_2021_detail <- read_csv("2021-spring-grades-detail.csv")
spring_2021 <- 
  spring_2021_raw %>% 
  left_join(spring_2020_detail, "CRN") %>% 
  mutate(year = 2021, term = "Spring") %>% 
  create_cols()


fall_2021_raw <- read_csv("2021-fall-grades.csv")
fall_2021_detail <- read_csv("2021-fall-grades-detail.csv")
fall_2021 <- 
  fall_2021_raw %>% 
  left_join(fall_2021_detail, "CRN") %>% 
  mutate(year = 2021, term = "Fall") %>% 
  create_cols()

spring_2022_raw <- read_csv("2022-spring-grades.csv")
spring_2022_detail <- read_csv("2022-spring-grades-detail.csv")
spring_2022 <- 
  spring_2022_raw %>% 
  left_join(spring_2022_detail, "CRN") %>% 
  mutate(year = 2022, term = "Spring") %>% 
  create_cols()

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
  filter(crn != "Report Total:") %>% 
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
    instruction_method_recode = case_when(
      instruction_method_raw == "T" ~ "Face-to-Face",
      instruction_method_raw == "E" ~ "Online",
      instruction_method_raw == "H" ~ "Hybrid",
      instruction_method_raw == "P" ~ "Partially Distance + 50% Tech",
      instruction_method_raw == "F" ~ "Fully Distance 95% Tech",
      TRUE ~ as.character("?")),
    .after = instruction_method_raw) %>% 
  mutate(
    course_prefix = str_replace_all(course, "[:digit:]", ""),
    course_pnbr = (str_remove_all(course, "\\D+")),
    undergrad_or_grad = case_when(as.numeric(course_pnbr) >= 7000 ~ "Graduate",
                                  as.numeric(course_pnbr) < 7000 ~ "Undergraduate")
  ) %>% 
  mutate(
    online_or_f2f = case_when(instruction_method_recode == "Face-to-Face" ~ "Face-to-Face",
                              instruction_method_recode == "Online" ~ "Online")
  ) %>% 
  mutate(term_year = ordered(term_year_ordered, levels = 1:10, labels = labels))


labels <- c("Fall 2017", "Spring 2018", "Fall 2018", "Spring 2019", "Fall 2019", 
            "Spring 2020", "Fall 2020", "Spring 2021", "Fall 2021", "Spring 2022")

df %>% 
  ggplot(aes(term_year, avg_gpa, 
             color = as.character(term_year))) + 
  geom_vline(xintercept = 5.75, size = 2, color = "red") +
  geom_jitter(width = .1) +
  stat_summary(fun = mean, geom = "crossbar") +
  scale_x_discrete(labels = label_wrap(9)) +
  theme(legend.position = "none") +
  labs(x = "Term",
       y = "Average GPA",
       title  = "Was there a change in average GPA after the pandemic?",
       caption = "Red line indicates Pandemic")

df_pandemic <-
  df %>%
  select(1:5,14:18,30:35) %>% 
  filter(term_year_ordered == 5 |
           term_year_ordered == 6) %>%
  mutate(professor_2 = professor) %>% 
  pivot_wider(names_from = term_year,
              values_from = c(course, term_year, avg_gpa),
              names_glue = "{term_year}_{.value}") %>% 
  clean_names()
  group_by(professor, term_year)
  summarize(
    n = n()
  )
  
df_temp <- 
  df %>% 
  select(term_year, professor, crn, course, n_students_enrolled, 
         avg_gpa, undergrad_or_grad, online_or_f2f) %>% 
  filter(str_detect(course, c("EPRS"))) %>%
  group_by(professor) %>% 
  mutate(professor_alias = randomNames(), .after = professor) %>% 
  ungroup %>% 
  mutate(course_number_alias = runif(n = 202, min = 10000, max = 99999) %>% round(0),
         .after = crn) %>% 
  arrange(professor, term_year) %>% 
  select(-professor, -crn) %>% 
  slice_sample(n = 100)

df_temp <- 
  df %>%
  select(1:5,14:18,30:35) %>% 
  filter(term_year_ordered == 5 |
           term_year_ordered == 6) %>%
  mutate(professor_2 = professor) %>%
  group_by(professor) %>% 
  pivot_wider(
    names_from = term_year,
    values_from = c(course, avg_gpa, professor_2),
    names_glue = "{term_year}_{.value}"
  ) %>%
  clean_names()

fall <- 
  df %>% 
  group_by(professor) %>% 
  filter(term == "Fall",
         year == 2019)

spring <- 
  df %>% 
  group_by(professor) %>% 
  filter(term == "Spring",
         year == 2020) %>% 
  select(professor, course, avg_gpa, online_or_f2f)

df_fall_spring <- 
  fall %>% 
  full_join(spring, by = c("professor", "course")) %>% 
  mutate(diff = avg_gpa.y - avg_gpa.x) %>% 
  filter(!is.na(diff))

df_summary <- 
  df_fall_spring %>% 
  group_by(professor) %>% 
  summarize(
    n = n()
  )

ggplot(df_fall_spring, aes(diff)) + 
  geom_histogram(color = "white",
                 binwidth = .1)

df_fall_spring %>% group_by(undergrad_or_grad) %>% summarize(n = n())
df_fall_spring %>% group_by(online_or_f2f.x) %>% summarize(n = n())
df_fall_spring %>% group_by(online_or_f2f.y) %>% summarize(n = n())

ggplot(df_fall_spring, aes(instruction_method_recode, diff,
               color = instruction_method_recode)) + 
  geom_jitter(width = .1)

ggplot(df_fall_spring, aes(undergrad_or_grad, diff,
                           color = undergrad_or_grad)) + 
  geom_jitter(width =.2) +
  theme(legend.position = "none")
