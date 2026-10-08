install.packages("haven")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("survey")

library("haven")
library("dplyr")
library("ggplot2")
library("survey")

demo<-read_xpt("DEMO_J.xpt")
diabetes<-read_xpt("DIQ_J.xpt")
prescriptions<-read_xpt("RXQ_RX_J.xpt")

nrow(demo)
nrow(diabetes)
nrow(prescriptions)

#Keep only adults (age 18+) and diagnosed with diabetes
#DIQ010 = 1 means "Yes, doctor told me I have diabetes"
#RIDAGEYR is age in years
#SEQN is the unique participant ID

diabetic_adults <- diabetes %>%
  filter(DIQ010 == 1) %>%
  inner_join(demo %>% filter (RIDAGEYR >=18), by = "SEQN")

#How many diabetic patients do we have?
nrow(diabetic_adults)

#List of GLP-1 mediation names to search for 
glp1_drugs<-c("semaglutide", "liraglutide", "dulaglutide", "exenatide", "albiglutide", "tirzepatide",
              "ozempic", "victoza", "trulicity", "byetta", "bydureon", "rybelsus")
#Find GLP-1 users in the presciption file
#RXDDRUG is the variable containing the drug names
glp1_users <- prescriptions %>%
  filter (tolower(RXDDRUG) %in% glp1_drugs) %>%
  select (SEQN) %>%
  distinct ()

#How many people are taking GLP-1 medications?
nrow(glp1_users)
#Add GLP-1 flag to diabetic adults dataset
diabetic_adults <- diabetic_adults %>%
  mutate (glp1_user = ifelse(SEQN %in% glp1_users$SEQN,1,0))

#Check how many are flagged
table(diabetic_adults$glp1_user)

#Compare GLP-1 users to non-users
diabetic_adults %>%
  group_by(glp1_user) %>%
  summarize(
    n = n(),
    mean_age = mean(RIDAGEYR, na.rm = TRUE),
    pct_female = mean(as.numeric(RIAGENDR) == 2, na.rm = TRUE) * 100,
    mean_income_ratio = mean(INDFMPIR, na.rm = TRUE),
    pct_on_insulin = mean(as.numeric(DIQ050) == 1, na.rm = TRUE) * 100
    )

#Bar chart of GLP-1 use
diabetic_adults %>%
  mutate(glp1_label = ifelse(glp1_user == 1,
                             "GLP-1 user",
                             "non-GLP-1 user")) %>%
  ggplot(aes(x = glp1_label, fill = glp1_label)) +
  geom_bar() +
  labs(
    title = "GLP-1 Agonist Use Among US Adults with Diabetes",
    subtitle = "NHANES 2017-2018",
    x = "",
    y = "Number of Participants",
    caption = "source: NHANES 2017-2018",
    ) +
  theme_minimal() +
  theme(legend.position = "none")

#Age distribution by GLP-1 use
diabetic_adults %>%
  mutate(glp1_label = ifelse(glp1_user ==1,
                             "GLP-1 User",
                             "Non GLP-1 User")) %>%
  ggplot(aes(x = RIDAGEYR, fill = glp1_label)) +
  geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
scale_fill_manual(values = c("GLP-1 User" = "black",
                             "Non GLP-1 User" = "steelblue")) +
  labs(
    title = "Age Distribution of US Adults with Diabetes by GLP-1 Use",
    subtitle = "NHANES 2017-2018",
    x= "Age (years)",
    y= "Number of Participants",
    fill = "Medication Status",
    caption = "Source: NHANES 2017-2018"
    ) +
  theme_minimal()

#Save plot
ggsave("age distribution_glp1.png",
       width = 10,
       height = 6,
       dpi = 300)

#Insulin use comparison
diabetic_adults %>%
  mutate(glp1_label = ifelse(glp1_user ==1,
                             "GLP-1 User",
                             "Non GLP-1 User"),
        insulin_label = ifelse(as.numeric(DIQ050) == 1,
                               "On Insulin",
                               "Not On Insulin"))%>%
          ggplot(aes(x = glp1_label, fill = insulin_label)) +
          geom_bar(position = "fill") +
          scale_fill_manual(values = c("On Insulin"= "steelblue",
                                        "Not On Insulin" = "lightgrey"))+
          scale_y_continuous(labels = scales::percent)+
          labs(
            title = "Insulin Use Among Diabetic Adults by GLP-1 Status",
            subtitle = "NHANES 2017-2018",
            x = "",
            y = "Percentage",
            fill = "Insulin Status",
            caption = "Source: NHANES 2017-2018"
            )+
  theme_minimal()

ggsave("insulin_status_glp1.png",
       width = 10,
       height = 6,
       dpi = 300)
#Add race/ethnicity labels and compare by GLP-1 status
diabetic_adults %>%
  mutate(
    race_ethnicity = case_when(
      RIDRETH1 == 1 ~ "Mexican American",
      RIDRETH1 == 2 ~ "Other Hispanic",
      RIDRETH1 == 3 ~ "Non-Hispanic White",
      RIDRETH1 == 4 ~ "Non-Hispanic Black",
      RIDRETH1 == 5 ~ "Other/Multi-racial",
    )
  ) %>%
  group_by(race_ethnicity, glp1_user) %>%
  summarise(n = n()) %>%
  mutate(pct = round(n / sum(n) * 100, 1)
  )
diabetic_adults %>%
  mutate(
    race_ethnicity = case_when(
      RIDRETH1 == 1 ~ "Mexican American",
      RIDRETH1 == 2 ~ "Other Hispanic",
      RIDRETH1 == 3 ~ "Non-Hispanic White",
      RIDRETH1 == 4 ~ "Non-Hispanic Black",
      RIDRETH1 == 5 ~ "Other/Multi-racial"
    ),
    glp1_label = if_else(glp1_user == 1, "GLP-1 User", "Non GLP-1 User")
    ) %>%
  ggplot(aes(x = race_ethnicity, fill = glp1_label)) +
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("GLP-1 User" = "steelblue",
                               "Non GLP-1 User" = "lightgrey"))+
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "GLP-1 Use Among Diabetic Adults by Race/Ethnicity",
    subtitle = "NHANES 2017-2018",
    x = "",
    y = "Percentage",
    fill = "Medication Status",
    caption = "Source: NHANES 2017-2018"
    
  )+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave("race_ethnicity_glp1.png",
         width = 10,
         height = 6,
         dpi = 300)
  
  #Summary Table
  summary_table <- diabetic_adults %>%
    summarise(
      total_diabetic_adults = n(),
      glp1_users = sum(glp1_user),
      glp1_prevalance_pct = round(mean(glp1_user) * 100, 1),
      mean_age_glp1 = round(mean(RIDAGEYR[glp1_user ==1], na.rm = TRUE),1),
      mean_age_non_glp1 = round(mean(RIDAGEYR[glp1_user == 0], na.rm = TRUE), 1),
      pct_female_glp1 = round(mean(as.numeric(RIAGENDR[glp1_user ==1]) ==2) * 100, 1),
      pct_insulin_glp1 = round(mean(as.numeric(DIQ050[glp1_user == 1]) ==1) * 100, 1),
      pct_insulin_non_glp1 = round(mean(as.numeric(DIQ050[glp1_user ==0]) ==1) *100, 1)
    )
  print (summary_table, width = Inf)
  
  #Prepare variables for logistic regression
  diabetic_adults <- diabetic_adults %>%
    mutate(
      female = ifelse(as.numeric(RIAGENDR) == 2,1,0),
      on_insulin = ifelse(as.numeric(DIQ050) == 1,1,0),
      race = as.factor (RIDRETH1)
    )
  #Run logistic regression
  glp1_model <- glm(glp1_user ~ RIDAGEYR + female + race + on_insulin + INDFMPIR, 
                    data = diabetic_adults,
                    family = binomial)
  #View results
  summary(glp1_model)
  
  #Convert to odds rations
  exp(cbind(OR = coef(glp1_model),
            confint(glp1_model)))
  
  