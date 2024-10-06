# Load in the libraries
library(haven)
library(tidyverse)
library(ggplot2)

# Read in the SPSS file
HSE_2019_full <- read_sav("hse_2019_eul_202110062.sav")

# These show the labels associated with each variable
#HSE_2019_full$Sex %>% attr('labels')
#HSE_2019_full$ag16g10 %>% attr('labels')
#HSE_2019_full$bmivg3 %>% attr('labels')
#HSE_2019_full$menwhgp %>% attr('labels')
#HSE_2019_full$womwhgp %>% attr('labels')

# Filter on the columns which are needed
HSE_2019_selected <- select(HSE_2019_full,
                            SerialA,
                            Sex,
                            ag16g10,
                            bp1, 
                            TypeD, 
                            bmivg3,
                            menwhgp,
                            womwhgp)


# Show the values for each column
apply(HSE_2019_selected, 2, table)

# Convert columns as factors
HSE_2019_factored <- HSE_2019_selected %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(ag16g10 = as.factor(ag16g10)) %>%
  mutate(bp1 = as.factor(bp1)) %>% 
  mutate(TypeD = as.factor(TypeD)) %>%
  mutate(bmivg3 = as.factor(bmivg3)) %>% 
  mutate(menwhgp = as.factor(menwhgp)) %>%
  mutate(womwhgp = as.factor(womwhgp))

## Recode the following variables

# Sex
HSE_2019_factored <- HSE_2019_factored %>%
  mutate(Sex = recode(Sex, `2` = 0, `1` = 1))

# Age
index_age_young = HSE_2019_factored$ag16g10 == 1 | HSE_2019_factored$ag16g10 == 2
index_age_not_young = HSE_2019_factored$ag16g10 == 3 | HSE_2019_factored$ag16g10 == 4 | HSE_2019_factored$ag16g10 == 5 | HSE_2019_factored$ag16g10 == 6 | HSE_2019_factored$ag16g10 == 7
HSE_2019_factored$`Young Age` = NA
HSE_2019_factored$`Young Age`[ index_age_young ] = 1 # true: They are young
HSE_2019_factored$`Young Age`[ index_age_not_young ] = 0 # false: They are old

index_age_old = HSE_2019_factored$ag16g10 == 6 | HSE_2019_factored$ag16g10 == 7 
index_age_not_old = HSE_2019_factored$ag16g10 == 1 | HSE_2019_factored$ag16g10 == 2 | HSE_2019_factored$ag16g10 == 3 | HSE_2019_factored$ag16g10 == 4 | HSE_2019_factored$ag16g10 == 5
HSE_2019_factored$`Old Age` = NA
HSE_2019_factored$`Old Age`[ index_age_old ] = 1 # true: They are old
HSE_2019_factored$`Old Age`[ index_age_not_old ] = 0 # false: They are young

# BMI obese
index_bmi_yes = HSE_2019_factored$bmivg3 == 3
index_bmi_no = HSE_2019_factored$bmivg3 == 1 | HSE_2019_factored$bmivg3 == 2
HSE_2019_factored$bmi_obese = NA
HSE_2019_factored$bmi_obese[ index_bmi_yes ] = 1 # true: obese
HSE_2019_factored$bmi_obese[ index_bmi_no ] = 0 # false: not obese

# MWHR Obese
index_menwhgp_yes = HSE_2019_factored$menwhgp == 4 | HSE_2019_factored$menwhgp == 5 | HSE_2019_factored$menwhgp == 6
index_menwhgp_no = HSE_2019_factored$menwhgp == 1 | HSE_2019_factored$menwhgp == 2 | HSE_2019_factored$menwhgp == 3
HSE_2019_factored$menwhgp_obese = NA
HSE_2019_factored$menwhgp_obese[ index_menwhgp_yes ] = 1 # true: Obese
HSE_2019_factored$menwhgp_obese[ index_menwhgp_no ] = 0 # false: Not obese

# WWHR Obese
index_womwhgp_yes = HSE_2019_factored$womwhgp == 5 | HSE_2019_factored$womwhgp == 6
index_womwhgp_no = HSE_2019_factored$womwhgp == 1 | HSE_2019_factored$womwhgp == 2 | HSE_2019_factored$womwhgp == 3 | HSE_2019_factored$womwhgp == 4
HSE_2019_factored$womwhgp_obese = NA
HSE_2019_factored$womwhgp_obese[ index_womwhgp_yes ] = 1 # true: Obese
HSE_2019_factored$womwhgp_obese[ index_womwhgp_no ] = 0 # false: Not obese

# Coalesce
HSE_2019_factored <- HSE_2019_factored %>% 
  mutate(whgp_obese = coalesce(menwhgp_obese, womwhgp_obese))

# Recode Blood Pressure answers
HSE_2019_factored <- HSE_2019_factored %>%
  mutate(bp1 = recode(bp1, `2` = 0, `1` = 1))

summary(HSE_2019_factored)

# Remove unnecessary columns
HSE_2019_factored <- HSE_2019_factored[ -c(3,5:8,12:13) ]

# Create new columns to show which age group are obese
HSE_2019_factored <- HSE_2019_factored %>%
  mutate(`bmi young` = `Young Age`*bmi_obese) %>%
  mutate(`whgp young` = `Young Age`*whgp_obese) %>%
  mutate(`bmi old` = `Old Age`*bmi_obese) %>%
  mutate(`whgp old` = `Old Age`*whgp_obese)

# glm for Blood Pressure
m.glm2 = glm(formula = bp1 ~ bmi_obese + whgp_obese + Sex + `Young Age` + `Old Age` + `whgp young` + `whgp old`,
            data = HSE_2019_factored, 
            family= binomial(link = "logit"))

# Inspect results
summary(m.glm2)

# Odds Ratios
`Odds Ratio` <- exp(coef(m.glm2))

# Number of Observations
nobs(m.glm2)
# Examine model accuracy 
# see https://rpubs.com/jpmurillo/153750 
table(HSE_2019_factored$bp1)
pred = predict(m.glm2, type = "response", newdata = HSE_2019_factored)
HSE_2019_factored$pred = 1*(pred > .50)
tab = table(HSE_2019_factored$pred, HSE_2019_factored$bp1)
tab
sum(diag(tab))/sum(tab)

# Another way of examining model accuracy using Log likelihood
logLik(m.glm2)
logLik(update(m.glm2, . ~ 1))

# CIs
`Confidence Intervals` <- exp(confint(m.glm2, level = 0.95))

data <- data.frame(predictor = names(`Odds Ratio`),
                   odds_ratio = `Odds Ratio`, 
                   lower_ci = `Confidence Intervals`[,1], 
                   upper_ci = `Confidence Intervals`[,2])

data1 <- data[-c(1, 4:6),]


# Plot the results!

p <- ggplot(data1, aes(x = odds_ratio, y = predictor)) + 
    geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = upper_ci, xmin = lower_ci), size = .5, height = 
                     .2, color = "black") +
    geom_point(size = 3.5, color = "#0072CE") +
    scale_y_discrete(labels=c('Old Age with High WHR','Young Age with High WHR', 'High BMI', 'High WHR')) +
    theme_bw()+
    theme(panel.grid.minor = element_blank(), axis.text=element_text(size=12, colour = "black"), axis.title=element_text(size=14, colour = "black")) +
    ylab("") +
    xlab("Odds ratio") +
    ggtitle("Risk of High Blood Pressure")

p

# Save output

ggsave("ForestPlot.png",
       dpi = 600,
       width = 8.71,
       height = 6.81)