###########################################
######## SUPPLEMENTARY MATERIALS #########
##########################################
### MENTAL HEALTH OF SEXUAL MINORITIES ###
##########################################

### loading necessary packages ###
library(tidyverse)  # data manipulation
library(readxl) # Excel data
library(purrr) # functional programming
library(ggplot2) # visualization
library(ggpubr) # combined plots
library(DescTools) # binomial Clopper-Pearson confidence intervals
library(lmtest) # robust (sandwich) standard errors
library(sandwich) # robust (sandwich) standard errors
library(extrafont) # text font
library(Cairo) # saving plots


###########################
##### DATA IMPORTING #####
##########################
data_2022_CATI <- read_excel("C:/Users/hp/Desktop/NUDZ/Epi_korona/Data/2022/01_DATA_CATI.xlsx", skip = 1)
data_2022_CAPI <- read_excel("C:/Users/hp/Desktop/NUDZ/Epi_korona/Data/2022/03_DATA_CAPI.xlsx", skip = 1)
data_2022_CAWI <- read_excel("C:/Users/hp/Desktop/NUDZ/Epi_korona/Data/2022/02_DATA_CAWI.xlsx", skip = 1)
data_2022w <- bind_rows(data_2022_CATI, data_2022_CAWI, data_2022_CAPI) %>% 
  rename_with(.fn = ~ str_replace(.x, "Q27DU", "PHQ9"),
              .cols = starts_with("Q27Du")) %>%
  rename_with(.fn = ~ str_replace(.x, "Q28UZ", "GAD7"),
              .cols = starts_with("Q28Uz")) %>%
  rename_with(.fn = ~ str_replace(.x, "Q67B", "DAST10"),
              .cols = starts_with("Q67B")) %>%
  rename_with(.fn = ~ str_replace(.x, "Q31BU", "SCOFF5"),
              .cols = starts_with("Q31BU")) %>%
  mutate(id = ID,
         collect_method = case_when(SOURCE == 1 ~ "panel sampling and telephone interviewing",
                                    SOURCE == 2 ~ "panel sampling and online interviewing",
                                    TRUE ~ "household sampling and personal interviewing"),
         sample_type = case_when(collect_method == "household sampling and personal interviewing" ~ "household",
                                 TRUE ~ "panel"),
         weight = VAHA,
         sex = factor(as.character(Q69),
                      levels = c("1","2"), 
                      labels = c("male", "female")),
         gender = factor(case_when(Q70 == 1 ~ "men",
                                   Q70 == 2 ~ "women",
                                   Q70 == 3 ~ "transgender",
                                   Q70 == 4 ~ "non_binary")),
         sexual_orientation = factor(case_when(Q76 == "1" ~ "heterosexual",
                                               Q76 %in% c("2","3") ~ "gay_or_lesbian",
                                               Q76 == "4" ~ "bisexual",
                                               Q76 == "5" ~ "more_diverse"),
                                     ordered = TRUE),
         region_residence = as.factor(Q72),
         size_place_residence = factor(as.character(Q73),
                                       levels = c("1","2","3","4"),
                                       labels = c(">5000", "5000-19999", "20000-99999","100000+")),
         income = factor(as.character(Q80),
                         levels = c("1","2","3","4","5","6","7"),
                         labels = c("0-9k","10-19k","20-29k","30-39k","40-49k","50+k","no reply")),
         age = Q71,
         education = factor(as.character(Q81),
                            levels = c("1","2","3","4"),
                            labels = c("primary", "lwr_secondary", "upr_secondary", "universitary")),
         work_status_orig = factor(case_when(Q77 == 1 ~ "employed",
                                             Q77 == 2 ~ "self-employed",
                                             Q77 == 3 ~ "in household",
                                             Q77 == 5 ~ "disability pension",
                                             Q77 == 6 ~ "sickness leave",
                                             Q77 == 7 ~ "student",
                                             Q77 == 9 ~ "voluntarily unemployed",  
                                             Q77 == 10 ~ "involuntarily unemployed", 
                                             Q77 == 8 ~ "retired", 
                                             Q77 == 4 ~ "parental_leave")), 
         work_status = factor(case_when(Q77 == 1 ~ "employed",
                                        Q77 == 2 ~ "self-employed",
                                        Q77 == 7 ~ "student",
                                        Q77 == 9 | Q77 == 10 ~ "unemployed",  # both intentional and unwanted
                                        Q77 == 8 ~ "retired", # "not-working retiree"
                                        Q77 == 4 ~ "parental_leave",
                                        TRUE ~ "other")), # sickness leave, disability pension, in household
         relationship_status = factor(case_when(Q74 == 3 | Q74 == 6 ~ "married/in rel, apart", # in relationship, living apart
                                                Q74 == 1 | Q74 == 2  ~ "married/in rel, together", # also married but living apart
                                                Q74 == 4 ~ "widowed",
                                                Q74 == 7 ~ "single",
                                                Q74 == 5 ~ "divorced")),
         marital_status_orig = factor(case_when(Q74 == 3 ~ "in relationship, living apart", # in relationship, living apart
                                                Q74 == 1 ~ "married", 
                                                Q74 == 2 ~ "living with a partner", # 1 a 2, 3 a 6
                                                Q74 == 6 ~ "married, living apart",
                                                Q74 == 4 ~ "widowed",
                                                Q74 == 7 ~ "single",
                                                Q74 == 5 ~ "divorced")),
         across(.cols = starts_with(c("PHQ9", "GAD7")), # depression- and anxiety-related symptoms
                ~ .x - 1),
         PHQ9_total = as.numeric(rowSums(select(., PHQ9_1:PHQ9_9))) - 9,
         GAD7_total = as.numeric(rowSums(select(., GAD7_1:GAD7_7))) - 7,
         DAST10_total = as.numeric(rowSums(select(., DAST10_1:DAST10_10) == 1)),
         E4_10 = E4_j,
         E4_11 = E4_k,
         E4_12 = E4_l,
         E4_13 = E4_m,
         across(.cols = starts_with("Q10_"), # help-seeking behavior
                ~ ifelse(.x %in% c(1, 2, 3, 4, 5, 6), 1, .x)),
         psychiatrist = Q10_1,
         psychologist = Q10_2,
         general_practinioner = Q10_3,
         crisis_intervention = Q10_4,
         online_therapy = Q10_5,
         help_seeking_12m = ifelse(Q9 == 1, 1, 0))

# M.I.N.I.-based clinical diagnoses
names(data_2022w)[grepl("^A3|^E1|^E4|^O1|^O3|^I4|^I5", names(data_2022w))] <-  names(data_2022w)[grepl("^A3|^E1|^E4|^O1|^O3|^I4|^I5", names(data_2022w))] %>% 
  chartr('abcdefghi', '123456789',  .) 
names(data_2022w)[grepl("^J", names(data_2022w))] <- str_replace(names(data_2022w)[grepl("^J", names(data_2022w))], "_", "")


### FINAL DATASETS ###
data_2022w <- data_2022w %>% 
  dplyr::select(id, weight, collect_method,
                sexual_orientation,
                sex, gender, age, education, work_status, income, relationship_status, size_place_residence,
                c(starts_with(c("A1", "A2", "A3",
                                "E1_", "E2", "E3", "E7",
                                "F1", "F2", 
                                "G1", "G2", "G3", "G4", 
                                "O1_", "O2", "O3", 
                                "I1", "I2", "I3", "I4", "I5", "I6",
                                "J1", "J2", "J3",
                                "C")),
                  E4_1:E4_9, E4_10:E4_13), 
                PHQ9_total, GAD7_total, PHQ9_1:PHQ9_9,
                help_seeking_12m, psychiatrist, psychologist, general_practinioner, crisis_intervention, online_therapy)


data_household <- filter(data_2022w, collect_method == "household sampling and personal interviewing")
data_panel <- filter(data_2022w, collect_method %in% c("panel sampling and online interviewing", "panel sampling and telephone interviewing"))



##################################
##### DESCRIPTIVE STATISTICS #####
##################################

# descriptive statistics: counts (%) and mean (sd) 
# by sexual orientation group (heterosexuals and SM subgroups) 
descriptives_props <-  data_panel %>% 
  dplyr::select(sexual_orientation, sex, gender, education, relationship_status, work_status, income, size_place_residence) %>% 
  pivot_longer(-c(sexual_orientation)) %>% 
  count(sexual_orientation, name, value) %>%
  group_by(sexual_orientation, name) %>%
  mutate(prop = round(n/sum(n)*100, 2),
         stat = paste(n, paste0("(", prop, ")"))) %>% 
  dplyr::select(sexual_orientation, name, value, stat) %>% 
  pivot_wider(names_from = c(sexual_orientation), values_from = stat) 

descriptives_age <- data_panel %>% 
  dplyr::select(sexual_orientation, age) %>% 
  group_by(sexual_orientation) %>% 
  summarise(age = paste0(round(mean(age), 2)," (", round(sd(age), 2), ")")) %>% 
  pivot_wider(names_from = c(sexual_orientation), values_from = age) %>% 
  mutate(name = "age", value = "age")

descriptives_panel <- bind_rows(descriptives_props, descriptives_age) %>% 
  mutate(across(everything(),
                ~ ifelse(is.na(.x), "0 (0)", .x))) %>% 
  mutate(name = factor(as.character(name),
                       levels = c("sex", "gender", "age", "relationship_status", "education", "work_status", "income", "size_place_residence"),
                       ordered = TRUE),
         value = factor(as.character(value),
                        levels = c("female", "male", "women", "men", "non_binary", "transgender", "age", 
                                   "married/in rel, together", "married/in rel, apart", "single", "divorced", "widowed",
                                   "primary", "lwr_secondary", "upr_secondary", "universitary",
                                   "employed", "unemployed", "self-employed", "student", "retired", "parental_leave", "other",
                                   "0-9k", "10-19k", "20-29k", "30-39k", "40-49k", "50+k", "no reply",
                                   ">5000", "100000+", "20000-99999", "5000-19999"),
                        ordered = TRUE)) %>% 
  arrange(name, value) %>% 
  select(name, value, heterosexual, gay_or_lesbian, bisexual, more_diverse)

write.csv(descriptives_panel, "C:/Users/hp/Desktop/NUDZ/LGBTIQ/Results/Supplements/Tables/descriptives.csv")




###########################################
##### PREVALENCE OF MENTAL DISORDERS #####
##########################################

# adding diagnoses based on M.I.N.I. assessment
data_2022_diag_panel <- data_panel %>% 
  mutate(across(.cols = c(starts_with(c("A1", "A2", "A3","E1_", "E2", "E3", "E7","F1", "F2","G1", "G2", "G3", "G4","O1_", "O2", "O3","I1", "I2", "I3", "I4", "I5", "I6","J1", "J2", "J3","C")),
                          E4_1:E4_9, E4_10:E4_13),
                ~ replace(., is.na(.), 0))) %>% 
  mutate(major_depressive_episode = as.numeric((A1 == 2 | A2 == 2) & rowSums(dplyr::select(., A1:A3_7) == 2) >= 5),
         panic_disorder = as.numeric(E1_1 == 2 & E1_2 == 2 & E2 == 2 & E3 == 2 & rowSums(dplyr::select(., E4_1:E4_13) == 2) >= 4 & E7 == 2),
         agoraphobia = as.numeric(F1 == 2 & F2 == 2),
         social_phobia = as.numeric(rowSums(dplyr::select(.,  G1, G2, G3, G4) == 2) == 4),
         GAD = as.numeric(O1_1 == 2 & O1_2 == 2 & O2 == 2 & rowSums(dplyr::select(., O3_1:O3_6) == 2) >= 3),
         PTSD = as.numeric(I1 == 2 & I2 == 2 & I3 == 2 & rowSums(dplyr::select(., starts_with("I4")) == 2) >= 3 & rowSums(dplyr::select(., starts_with("I5")) == 2) >= 2 & I6 == 2),
         Jx = as.numeric(J3aa == 2 & J3ab == 2),
         Jy = as.numeric(J2ba == 2 | J2bb == 2),
         alcohol_dependence = as.numeric(J1 == 2 & rowSums(dplyr::select(., starts_with("J2")) == 2) >= 3),
         alcohol_abuse = as.numeric(J1 == 2 & rowSums(dplyr::select(., starts_with("J3")) == 2) >= 1),
         suicidal_thoughts_and_behaviours = as.numeric(rowSums(dplyr::select(., C1:C6) == 2) >= 1)) %>% 
  mutate(anxiety_disorders = as.numeric(rowSums(dplyr::select(.,panic_disorder, agoraphobia, social_phobia, PTSD, GAD) == 1) >= 1), 
         alcohol_use_disorders = as.numeric(rowSums(dplyr::select(., alcohol_dependence, alcohol_abuse) == 1) >= 1),
         any_mental_disorder = as.numeric(major_depressive_episode| anxiety_disorders | alcohol_use_disorders | suicidal_thoughts_and_behaviours))


### by sexual orientation ###
prevalence_disorders_panel <- data_2022_diag_panel %>% 
  select(id, sexual_orientation,
         major_depressive_episode, suicidal_thoughts_and_behaviours, anxiety_disorders, alcohol_use_disorders, any_mental_disorder) %>% 
  pivot_longer(names_to = "diagnosis",
               cols = -c(id, sexual_orientation)) %>% 
  group_by(diagnosis, sexual_orientation) %>% 
  nest() %>% 
  mutate(successes = map_dbl(.x = data,
                             ~ sum(.x$value == 1)),
         sample_size = map_dbl(.x = data,
                               ~ length(.x$value)),
         prop = successes/sample_size*100,
         prevalence_clop_pears = map2(.x = successes,
                                      .y = sample_size,
                                      ~ round(BinomCI(.x, .y, 
                                                      method = "clopper-pearson")*100, 2)), # binomial  (Clopper–Pearson) method
         results_clop_pears = map(.x = prevalence_clop_pears,
                                  ~ as.data.frame(.x) %>% mutate(estimate = paste0(.x[[1]], " (", .x[[2]], ", ", .x[[3]], ")"))),
         prevalence_delta_mean = map(.x = data,
                                     ~ lm(.x$value ~ 1)),
         prevalence_delta_CI = map(.x = prevalence_delta_mean,
                                   ~ confint(.x, type = "delta.method", level = 0.95)),
         results_delta = map2(.x = prevalence_delta_mean,
                              .y = prevalence_delta_CI,
                              ~ as.data.frame(cbind(.x$coefficients, .y)) %>% 
                                rownames_to_column(.) %>% 
                                transmute(est =  V1,
                                          lwr.ci = `2.5 %`,
                                          upr.ci = `97.5 %`) %>% 
                                mutate(across(where(is.numeric),
                                              ~ round((.x*100),2)),
                                       estimate = paste0(est, " (", lwr.ci, ", ", upr.ci, ")"))),
         results_delta = ifelse(successes <= 5, results_clop_pears, results_delta))  # if a subgroup has   ≤   5  individuals,   we  will  calculate   exact   binomial  (Clopper–Pearson)   CIs, otherwise delta methods

prevalence_disorders_plotting_panel <- prevalence_disorders_panel %>% 
  dplyr::select(results_delta) %>% 
  unnest()  

prevalence_disorders_results_panel <- prevalence_disorders_plotting_panel %>% 
  dplyr::select(-c(est, lwr.ci, upr.ci)) %>% 
  pivot_wider(names_from = "diagnosis",
              values_from = "estimate") %>% 
  select(c("any_mental_disorder", "alcohol_use_disorders", "major_depressive_episode", "anxiety_disorders", "suicidal_thoughts_and_behaviours")) %>% 
  mutate(sexual_orientation = factor(sexual_orientation,
                                     levels = c("heterosexual", "gay_or_lesbian", "bisexual", "more_diverse"),
                                     ordered = TRUE)) %>% 
  arrange(sexual_orientation)

write.csv(prevalence_disorders_results_panel, "C:/Users/hp/Desktop/NUDZ/LGBTIQ/Results/Supplements/Tables/prevalence_disorders_results_panel.csv")


### GRAPH: prevalence by LGBTIQ category ###
my_colors4   <- c("heterosexual" = "#648FFF", 
                  "gay or lesbian" = "#785EF0", 
                  "bisexual" = "#DC267F", 
                  "more diverse" = "#FE6100") 

my_colors3   <- c("gay or lesbian" = "#785EF0", 
                  "bisexual" = "#DC267F",
                  "more diverse" = "#FE6100")

font_import()
loadfonts(device="win")
windowsFonts(Times = windowsFont("Times New Roman"))

plot_prevalence_panel <- prevalence_disorders_plotting_panel %>%
  mutate(diagnosis = str_replace_all(diagnosis, "_", " "),
         sexual_orientation = str_replace_all(sexual_orientation, "_", " "),
         diagnosis = factor(as.character(diagnosis),
                            levels = c("any mental disorder", "alcohol use disorders", "major depressive episode", "anxiety disorders", "suicidal thoughts and behaviours"),
                            ordered = TRUE),
         sexual_orientation = factor(as.character(sexual_orientation),
                                     levels = c("heterosexual", "gay or lesbian", "bisexual", "more diverse"),
                                     ordered = TRUE)) %>% 
  ggplot(aes(x = est,
             y = sexual_orientation,
             color = sexual_orientation,
             fill = sexual_orientation)) +
  geom_col(width = 0.8) +
  geom_errorbar(aes(xmin = lwr.ci,
                     xmax = upr.ci),
                 width = 0.25,
                 color = "black") +
  scale_color_manual(values = my_colors4,
                     guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = my_colors4,
                    guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~ diagnosis,
             ncol = 1,
             scales = "free_x") +
  scale_x_continuous(limits = c(-0.1,72),
                     breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  labs(title = "Prevalence of mental disorders",
       x = "Prevalence (95% CI)", 
       y = "",
       fill = "Group") +
  theme_minimal() +
  guides(color = "none") +
  theme(text = element_text(size=18,
                            family = "Times"),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 20,
                                  face = "bold",
                                  hjust = 0.5),
        axis.line.x = element_line(color="black", linewidth = 1),
        axis.line.y = element_line(color="black", linewidth = 1))







#########################################################################
### Relative risk of mental disorders: heterosexuals vs. SM subgroups ###
########################################################################

# HETEROSEXUALS as reference group
disorders_rel_risk_panel <- data_2022_diag_panel %>% 
  dplyr::select(id, sexual_orientation, 
                any_mental_disorder, major_depressive_episode, anxiety_disorders, alcohol_use_disorders, suicidal_thoughts_and_behaviours,
                gender, age, education, work_status, income, relationship_status, size_place_residence) %>%  
  mutate(sexual_orientation = relevel(factor(sexual_orientation, 
                                             ordered = FALSE), 
                                      ref = "heterosexual")) %>% 
  pivot_longer(names_to = "disorder",
               values_to = "diagnosed",
               cols = c("any_mental_disorder", "major_depressive_episode", "anxiety_disorders", "alcohol_use_disorders", "suicidal_thoughts_and_behaviours")) %>% 
  group_by(disorder) %>% 
  nest() %>% 
  mutate(logit_models = map(.x = data,
                            ~ glm(I(diagnosed == 1) ~ sexual_orientation + gender + age + education + work_status + income + relationship_status + size_place_residence,
                                  family = binomial(link="logit"), 
                                  data = .x)),
         point_estimates = map(.x = logit_models,
                               ~ exp(summary(.x)$coefficients[,"Estimate"])),
         CI_estimates = map(.x = logit_models,
                            ~ exp(confint(.x, method = "likelihood",level = 0.95))),
         model_estimates = map2(.x = point_estimates,
                                .y = CI_estimates,
                                ~ as.data.frame(cbind(.x, .y)) %>% 
                                  transmute(est =  .x,
                                            lwr.ci = `2.5 %`,
                                            upr.ci = `97.5 %`,
                                            estimate = paste0(round(est,2), " (", round(lwr.ci,2), ", ", round(upr.ci,2), ")")) %>% 
                                  rownames_to_column() %>% 
                                  filter(str_detect(rowname, "^sexual")))) %>% 
  dplyr::select(model_estimates) %>% 
  unnest() %>%
  mutate(rowname = sub("sexual_orientation", "", rowname))

disorders_rel_risk_results_panel <- disorders_rel_risk_panel %>% 
  dplyr::select(disorder, rowname, estimate) %>% 
  pivot_wider(names_from = "disorder",
              values_from = "estimate") %>% 
  mutate(rowname = factor(rowname,
                          levels = c("gay_or_lesbian", "bisexual", "more_diverse"),
                          ordered = TRUE)) %>% 
  arrange(rowname)

write.csv(disorders_rel_risk_results_panel, "C:/Users/hp/Desktop/NUDZ/LGBTIQ/Results/Supplements/Tables/disorders_rel_risk_results.csv")


# GAYS and LESBIANS as reference group
disorders_rel_risk_bi_panel <- data_2022_diag_panel %>% 
  dplyr::select(id, sexual_orientation, 
                any_mental_disorder, major_depressive_episode, anxiety_disorders, alcohol_use_disorders, suicidal_thoughts_and_behaviours,
                gender, age, education, work_status, income, relationship_status, size_place_residence) %>% 
  mutate(sexual_orientation = relevel(factor(sexual_orientation, 
                                             ordered = FALSE), 
                                      ref = "gay_or_lesbian")) %>% 
  pivot_longer(names_to = "disorder",
               values_to = "diagnosed",
               cols = c("any_mental_disorder", "major_depressive_episode", "anxiety_disorders", "alcohol_use_disorders", "suicidal_thoughts_and_behaviours")) %>% 
  group_by(disorder) %>% 
  nest() %>% 
  mutate(logit_models = map(.x = data,
                            ~ glm(I(diagnosed == 1) ~ sexual_orientation + gender + age + education + work_status + income + relationship_status + size_place_residence,
                                  family = binomial(link="logit"), 
                                  data = .x)),
         point_estimates = map(.x = logit_models,
                               ~ exp(summary(.x)$coefficients[,"Estimate"])),
         CI_estimates = map(.x = logit_models,
                            ~ exp(confint(.x, method = "likelihood",level = 0.95))),
         model_estimates = map2(.x = point_estimates,
                                .y = CI_estimates,
                                ~ as.data.frame(cbind(.x, .y)) %>% 
                                  transmute(est =  .x,
                                            lwr.ci = `2.5 %`,
                                            upr.ci = `97.5 %`,
                                            estimate = paste0(round(est,2), " (", round(lwr.ci,2), ", ", round(upr.ci,2), ")")) %>% 
                                  rownames_to_column() %>% 
                                  filter(str_detect(rowname, "^sexual")))) %>% 
  dplyr::select(model_estimates) %>% 
  unnest() %>%
  mutate(rowname = sub("sexual_orientation", "", rowname))

disorders_rel_risk_results_bi_panel <- disorders_rel_risk_bi_panel %>% 
  dplyr::select(disorder, rowname, estimate) %>% 
  filter(rowname != "heterosexual") %>% 
  pivot_wider(names_from = "disorder",
              values_from = "estimate") %>% 
  mutate(rowname = factor(rowname,
                          levels = c("bisexual", "more_diverse"),
                          ordered = TRUE)) %>% 
  arrange(rowname)

write.csv(disorders_rel_risk_results_bi_panel, "C:/Users/hp/Desktop/NUDZ/LGBTIQ/Results/Supplements/Tables/disorders_rel_risk_results_bi.csv")


### GRAPH: heterosexuals vs. SM subgroups ###
graph_relative_risk_panel <- disorders_rel_risk_panel %>%
  mutate(diagnosis = str_replace_all(disorder, "_", " "),
         sexual_orientation = str_replace_all(rowname, "_", " "),
         diagnosis = factor(as.character(diagnosis),
                            levels = c("any mental disorder", "alcohol use disorders", "major depressive episode", "anxiety disorders", "suicidal thoughts and behaviours"),
                            ordered = TRUE),
         sexual_orientation = factor(as.character(sexual_orientation),
                                     levels = c("heterosexual", "gay or lesbian", "bisexual", "more diverse"),
                                     ordered = TRUE)) %>% 
  ggplot(aes(x = est,
             y = sexual_orientation,
             color = sexual_orientation)) +
  scale_color_manual(values = my_colors3,
                     guide = guide_legend(reverse = TRUE)) +
  geom_point(size = 3.5,
             position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(xmin = lwr.ci,
                     xmax = upr.ci),
                 linewidth = 1,
                 width = 0.25,
                 position = position_dodge(width = 0.8)) +
  scale_x_continuous(trans = "log10",
                     limits = c(0.3, 5.1),
                     breaks = c(0.5, 1, 2, 5)) +
  geom_vline(xintercept = 1, 
             linetype = 2,
             linewidth = 1) +
  labs(y = "",
       x = "Odds ratio (95% CI)",
       title = "Relative risk of mental disorder occurrence",
       color = "Group") +
  theme_minimal() +
  facet_wrap(~ diagnosis,
             ncol = 1,
             scales = "free_x") +
  theme(plot.title = element_text(size = 20,
                                  face = "bold",
                                  hjust = 0.5),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(size=18,
                            family = "Times"),
        axis.line.x = element_line(color="black", linewidth = 1),
        axis.line.y = element_line(color="black", linewidth = 1))


graph_MINI_diag_panel <- ggarrange(plot_prevalence_panel, NULL, graph_relative_risk_panel, 
                                            widths = c(1, 0.05, 1), nrow=1, common.legend = TRUE, legend="right")

ggsave(filename = "graph_MINI_diag_panel.eps",
       path = "C:/Users/hp/Desktop/NUDZ/LGBTIQ/Results/Supplements/Graphs", 
       width = 24,
       height = 16,
       device= cairo_ps, 
       dpi=700)




#################################################
########## MENTAL DISTRESS SEVERITY ############
### anxiety- and depression related symptoms ###
################################################

symptom_severity_panel <- data_panel %>% 
  dplyr::select(id, sexual_orientation,
                PHQ9_total, GAD7_total) %>% 
  pivot_longer(names_to = "symptoms",
               cols = -c(id, sexual_orientation)) %>% 
  group_by(symptoms, 
           sexual_orientation) %>% 
  nest() %>% 
  mutate(mean = map(.x = data,
                    ~ lm(.x$value ~ 1)),
         CIs = map(.x = mean,
                   ~ confint(.x, type = "delta.method", level = 0.95)),
         results_delta = map2(.x = mean,
                              .y = CIs,
                              ~ as.data.frame(cbind(.x$coefficients, .y)) %>% 
                                rownames_to_column(.) %>% 
                                transmute(est =  V1,
                                          lwr.ci = `2.5 %`,
                                          upr.ci = `97.5 %`,
                                          estimate = paste0(round(est,2), " (", round(lwr.ci,2), ", ", round(upr.ci,2), ")"))))

symptom_severity_plotting_panel <- symptom_severity_panel %>% 
  dplyr::select(results_delta) %>% 
  unnest()  

symptom_severity_results_panel <- symptom_severity_plotting_panel %>% 
  dplyr::select(-c(est, lwr.ci, upr.ci)) %>% 
  pivot_wider(names_from = "symptoms",
              values_from = "estimate") %>% 
  mutate(sexual_orientation = factor(sexual_orientation,
                                     levels = c("heterosexual", "gay_or_lesbian", "bisexual", "more_diverse"),
                                     ordered = TRUE)) %>% 
  arrange(sexual_orientation)

write.csv(symptom_severity_results_panel, "C:/Users/hp/Desktop/NUDZ/LGBTIQ/Results/Supplements/Tables/symptom_severity_results_panel.csv")


### GRAPH ###
plot_sympt_sev_means_panel <- symptom_severity_plotting_panel %>%
  mutate(sexual_orientation = str_replace_all(sexual_orientation, "_", " "),
         symptoms = case_when(symptoms == "PHQ9_total" ~ "Depression",
                              symptoms == "GAD7_total" ~ "Anxiety"),
         sexual_orientation = factor(as.character(sexual_orientation),
                                     levels = c("heterosexual", "gay or lesbian", "bisexual", "more diverse"),
                                     ordered = TRUE)) %>% 
  ggplot(aes(x = est,
             y = sexual_orientation,
             color = sexual_orientation,
             fill = sexual_orientation)) +
  geom_col(width = 0.8) +
  geom_errorbar(aes(xmin = lwr.ci,
                     xmax = upr.ci),
                 linewidth = 0.5,
                 width = 0.25,
                 color = "black") +
  scale_color_manual(values = my_colors4,
                     guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = my_colors4,
                    guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~ symptoms,
             ncol = 1,
             scales = "free_x") +
  scale_x_continuous(limits = c(-0.1,10.6),
                     breaks = c(0, 2, 4, 6, 8, 10)) +
  labs(title = "Mean score of mental distress severity",
       x = "Prevalence (95% CI)", 
       y = "",
       fill = "Group") +
  theme_minimal() +
  guides(color = "none") +
  theme(text = element_text(size=18,
                            family = "Times"),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size = 20,
                                  face = "bold",
                                  hjust = 0.5),
        axis.line.x = element_line(color="black", linewidth = 1),
        axis.line.y = element_line(color="black", linewidth = 1))



##### RELATIVE RISK ###


#  NB_models = map(.x = data,
#                  ~ glm.nb(value ~ sexual_orientation + gender + age + education + work_status + income + relationship_status + size_place_residence,
#                           data = .x)),
#  ZINB_models = map(.x = data,
#                   ~ zeroinfl(value ~ sexual_orientation + gender + age + education + work_status + income + relationship_status + size_place_residence,
#                              dist="negbin",
#                              link="logit",
#                              data = .x)),

sympt_sev_rel_risk_panel <- data_2022_diag_panel %>% 
  dplyr::select(id, sexual_orientation, 
                PHQ9_total, GAD7_total,
                gender, age, education, work_status, income, relationship_status, size_place_residence) %>%  
  mutate(sexual_orientation = relevel(factor(sexual_orientation, 
                                             ordered = FALSE), 
                                      ref = "heterosexual"),
         across(c("PHQ9_total", "GAD7_total"),
                ~ .x + 1)) %>%
  pivot_longer(names_to = "symptoms",
               cols = c("PHQ9_total", "GAD7_total")) %>% 
  group_by(symptoms) %>% 
  nest() %>% 
  mutate(excess_zeros = map_dbl(.x = data,
                                ~ sum(.x$value == 0)/length(.x$value)*100),
         linear_models = map(.x = data,
                             ~ lm(log(value) ~ sexual_orientation + gender + age + education + work_status + income + relationship_status + size_place_residence,
                                  data = .x)),
         point_estimates = map(.x = linear_models,
                               ~ exp(summary(.x)$coefficients[,"Estimate"])),
         CI_estimates = map(.x = linear_models,
                            ~ exp(coefci(.x, vcov = vcovHC(.x, type = 'HC0')))),
         model_estimates = map2(.x = point_estimates,
                                .y = CI_estimates,
                                ~ as.data.frame(cbind(.x, .y)) %>%
                                  transmute(est =  .x,
                                            lwr.ci = `2.5 %`,
                                            upr.ci = `97.5 %`,
                                            estimate = paste0(round(est,2), " (", round(lwr.ci,2), ", ", round(upr.ci,2), ")")) %>% 
                                  rownames_to_column() %>% 
                                  filter(str_detect(rowname, "^sexual")))) 

sympt_sev_rel_risk_plotting_panel <- sympt_sev_rel_risk_panel %>% 
  dplyr::select(model_estimates) %>% 
  unnest() %>%
  mutate(rowname = sub("sexual_orientation", "", rowname))

sympt_sev_rel_risk_results_panel <- sympt_sev_rel_risk_plotting_panel %>% 
  dplyr::select(symptoms, rowname, estimate) %>% 
  pivot_wider(names_from = "symptoms",
              values_from = "estimate") %>% 
  mutate(rowname = factor(rowname,
                          levels = c("gay_or_lesbian", "bisexual", "more_diverse"),
                          ordered = TRUE)) %>% 
  arrange(rowname)

write.csv(sympt_sev_rel_risk_results_panel, "C:/Users/hp/Desktop/NUDZ/LGBTIQ/Results/Supplements/Tables/sympt_sev_rel_risk_results_panel.csv")


# GAYS and LESBIANS as reference group
sympt_sev_rel_risk_bi_panel <- data_2022_diag_panel %>% 
  dplyr::select(id, sexual_orientation, 
                PHQ9_total, GAD7_total,
                gender, age, education, work_status, income, relationship_status, size_place_residence) %>%  
  mutate(sexual_orientation = relevel(factor(sexual_orientation, 
                                             ordered = FALSE), 
                                      ref = "gay_or_lesbian"),
         across(c("PHQ9_total", "GAD7_total"),
                ~ .x + 1)) %>%
  pivot_longer(names_to = "symptoms",
               cols = c("PHQ9_total", "GAD7_total")) %>% 
  group_by(symptoms) %>% 
  nest() %>% 
  mutate(excess_zeros = map_dbl(.x = data,
                                ~ sum(.x$value == 0)/length(.x$value)*100),
         linear_models = map(.x = data,
                             ~ lm(log(value) ~ sexual_orientation + gender + age + education + work_status + income + relationship_status + size_place_residence,
                                  data = .x)),
         point_estimates = map(.x = linear_models,
                               ~ exp(summary(.x)$coefficients[,"Estimate"])),
         CI_estimates = map(.x = linear_models,
                            ~ exp(coefci(.x, vcov = vcovHC(.x, type = 'HC1')))),
         model_estimates = map2(.x = point_estimates,
                                .y = CI_estimates,
                                ~ as.data.frame(cbind(.x, .y)) %>%
                                  transmute(est =  .x,
                                            lwr.ci = `2.5 %`,
                                            upr.ci = `97.5 %`,
                                            estimate = paste0(round(est,2), " (", round(lwr.ci,2), ", ", round(upr.ci,2), ")")) %>% 
                                  rownames_to_column() %>% 
                                  filter(str_detect(rowname, "^sexual")))) 

sympt_sev_rel_risk_plotting_bi_panel <- sympt_sev_rel_risk_bi_panel %>% 
  dplyr::select(model_estimates) %>% 
  unnest() %>%
  mutate(rowname = sub("sexual_orientation", "", rowname))

sympt_sev_rel_risk_results_bi_panel <- sympt_sev_rel_risk_plotting_bi_panel %>% 
  dplyr::select(symptoms, rowname, estimate) %>% 
  pivot_wider(names_from = "symptoms",
              values_from = "estimate") %>% 
  filter(rowname != "heterosexual") %>% 
  mutate(rowname = factor(rowname,
                          levels = c("bisexual", "more_diverse"),
                          ordered = TRUE)) %>% 
  arrange(rowname) 

write.csv(sympt_sev_rel_risk_results_bi_panel, "C:/Users/hp/Desktop/NUDZ/LGBTIQ/Results/Supplements/Tables/sympt_sev_rel_risk_bi_results_panel.csv")




### GRAPH: heterosexuals vs. SM subgroups ###
graph_sympt_sev_rel_risk_panel <- sympt_sev_rel_risk_plotting_panel %>%
  mutate(sexual_orientation = str_replace_all(rowname, "_", " "),
         symptoms = case_when(symptoms == "PHQ9_total" ~ "Depression",
                              symptoms == "GAD7_total" ~ "Anxiety"),
         sexual_orientation = factor(as.character(sexual_orientation),
                                     levels = c("heterosexual", "gay or lesbian", "bisexual", "more diverse"),
                                     ordered = TRUE)) %>% 
  ggplot(aes(x = est,
             y = sexual_orientation,
             color = sexual_orientation)) +
  scale_color_manual(values = my_colors3,
                     guide = guide_legend(reverse = TRUE)) +
  geom_point(size = 3.5,
             position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(xmin = lwr.ci,
                     xmax = upr.ci),
                 linewidth = 1,
                width = 0.25,
                 position = position_dodge(width = 0.8)) +
  scale_x_continuous(trans = "log10",
                     limits = c(0.7, 2.2),
                     breaks = c(1, 2)) +
  geom_vline(xintercept = 1, 
             linetype = 2,
             linewidth = 1) +
  labs(y = "",
       x = "Beta (95% CI)",
       title = "Linear regression models of mental distress severity ",
       color = "Group") +
  theme_minimal() +
  facet_wrap(~ symptoms,
             ncol = 1,
             scales = "free_x") +
  theme(plot.title = element_text(size = 20,
                                  face = "bold",
                                  hjust = 0.5),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(size=18,
                            family = "Times"),
        axis.line.x = element_line(color="black", linewidth = 1),
        axis.line.y = element_line(color="black", linewidth = 1))


graph_sympt_sev_panel <- ggarrange(plot_sympt_sev_means_panel, NULL, graph_sympt_sev_rel_risk_panel, 
                                           widths = c(1, 0.05, 1), nrow=1, common.legend = TRUE, legend="right")

ggsave(filename = "graph_sympt_sev_panel.eps",
       path = "C:/Users/hp/Desktop/NUDZ/LGBTIQ/Results/Supplements/Graphs", 
       width = 24,
       height = 16,
       device=cairo_ps, 
       dpi=700)


######################
### TREATMENT GAP ###
#####################

data_2022_treat_panel <- data_2022_diag_panel %>% 
  select(sexual_orientation, 
         gender, age, education, work_status, income, relationship_status, size_place_residence,
         any_mental_disorder, major_depressive_episode, anxiety_disorders, alcohol_use_disorders, suicidal_thoughts_and_behaviours,
         help_seeking_12m, psychiatrist, psychologist, general_practinioner, crisis_intervention, online_therapy)  %>% 
  mutate(SM_status = case_when(sexual_orientation == "heterosexual" ~ "heterosexual",
                               TRUE ~ "sexual minority"),
         across(c("psychiatrist", "psychologist", "general_practinioner", "crisis_intervention", "online_therapy"),
                ~ replace(., is.na(.), 0)),
         not_treated = as.numeric(psychiatrist == 0 & psychologist == 0 & general_practinioner == 0 & crisis_intervention == 0 & online_therapy == 0),
         sought_help = ifelse(not_treated == 1, 0, 1)) %>% 
  pivot_longer(names_to = "disorder",
               values_to = "diagnosed",
               cols = c("any_mental_disorder", "alcohol_use_disorders", "major_depressive_episode", "anxiety_disorders", "suicidal_thoughts_and_behaviours")) %>% 
  filter(diagnosed == 1) 

treatment_gap_panel <- data_2022_treat_panel %>% 
  group_by(SM_status,
           disorder) %>% 
  nest() %>% 
  mutate(counts = map_dbl(.x = data,
                          ~ sum(.x$not_treated == 1)),
         CIs = map(.x = data,
                   ~ BinomCI(x = sum(.x$not_treated == 1), 
                             n = length(.x$not_treated),
                             method = "clopper-pearson",
                             conf.level = 0.95)),
         prevalence = map(.x = CIs,
                          ~ as.data.frame(.x) %>% 
                            mutate(across(where(is.numeric),
                                          ~ round((.x*100),2)),
                                   estimate = paste0(est, " (", lwr.ci, ", ", upr.ci, ")"))))

results_treatment_gap_panel <- treatment_gap_panel %>% 
  dplyr::select(prevalence) %>% 
  unnest() %>% 
  dplyr::select(-c(est, lwr.ci, upr.ci)) %>% 
  pivot_wider(names_from = "SM_status",
              values_from = "estimate") %>% 
  mutate(disorder = factor(as.character(disorder),
                           levels = c("any_mental_disorder", "alcohol_use_disorders", "major_depressive_episode", "anxiety_disorders", "suicidal_thoughts_and_behaviours"),
                           ordered = TRUE)) %>% 
  arrange(disorder) 



###############################
### HELP-SEEKING BEHAVIOR #####
###############################

models_help_seek_panel <- data_2022_treat_panel %>% 
  group_by(disorder) %>% 
  nest() %>% 
  mutate(counts = map_dbl(.x = data,
                          ~ sum((.x$sought_help == 1) & (.x$SM_status == "sexual minority"))),
         logit_models = map(.x = data,
                            ~ glm(I(sought_help == 1) ~ SM_status + gender + age + education + work_status + income + relationship_status + size_place_residence,
                                  family = binomial(link="logit"), 
                                  data = .x)),
         point_estimates = map(.x = logit_models,
                               ~ exp(summary(.x)$coefficients[,"Estimate"])),
         CI_estimates = map(.x = logit_models,
                            ~ exp(confint(.x, method = "likelihood", level = 0.95))),
         model_estimates = map2(.x = point_estimates,
                                .y = CI_estimates,
                                ~ as.data.frame(cbind(.x, .y)) %>% 
                                  transmute(est =  .x,
                                            lwr.ci = `2.5 %`,
                                            upr.ci = `97.5 %`,
                                            estimate = paste0(round(est,2), " (", round(lwr.ci,2), ", ", round(upr.ci,2), ")")) %>% 
                                  rownames_to_column() %>% 
                                  filter(str_detect(rowname, "^SM_status"))))

results_help_seek_panel <- models_help_seek_panel %>% 
  dplyr::select(counts, model_estimates) %>% 
  unnest() %>%
  mutate(rowname = sub("SM_status", "", rowname)) %>% 
  dplyr::select(disorder, rowname, counts, estimate) %>% 
  mutate(estimate = ifelse(counts <= 5, "NA", estimate)) %>% 
  pivot_wider(names_from = "rowname",
              values_from = "estimate") %>% 
  transmute(disorder = factor(as.character(disorder),
                              levels = c("any_mental_disorder", "alcohol_use_disorders", "major_depressive_episode", "anxiety_disorders", "suicidal_thoughts_and_behaviours"),
                              ordered = TRUE),
            OR = `sexual minority`) %>% 
  arrange(disorder)

results_help_seek_treat_gap_panel <- cbind(results_treatment_gap_panel, results_help_seek_panel[,2])

write.csv(results_help_seek_treat_gap_panel, "C:/Users/hp/Desktop/NUDZ/LGBTIQ/Results/Supplements/Tables/results_help_seek_treat_gap_panel.csv")

