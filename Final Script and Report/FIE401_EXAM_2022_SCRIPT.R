rm(list =ls())
options(scipen = 999)

# packages
library(tidyverse)
library(ggplot2)
library(DescTools)
library(knitr)
library(MatchIt)
library(lmtest)
library(sandwich)
library(car)
library(mfx)
library(stargazer)
library(AER)
library(plm)
library(kableExtra)

rm(list =ls())

###############################################################################
# TASK 1 AND 2
###############################################################################

# Load data
hor <- read.delim("1976-2020-house.tab")
emissions <- readRDS("emissions.rds")
emissions_location <- readRDS("emissions_location.rds")

# We create a data frame with congress number and election year
congress_year <- tibble(
  congress = seq(from = 107, to = 117, by = 1),
  year = seq(from = 2000, to = 2020, by = 2),
)

# We create a data frame with congress and year in office:
congress_year_inoffice <- congress_year %>% 
  mutate(year_end = dplyr::lead(year),
         year_start = year_end-1)

start <- congress_year_inoffice %>% 
  dplyr::select(-year_end) %>% 
  rename(inoffice = year_start)

end <- congress_year_inoffice %>% 
  dplyr::select(-year_start) %>% 
  rename(inoffice = year_end)

congress_year_inoffice <- start %>% 
  rbind(end) %>% 
  rename(election_year = year) %>%
  filter(congress != 117)

rm(list = c("start", "end"))

# Calculate party who won in each district across time
hor_won <- hor %>% 
  mutate(share_vote = candidatevotes/totalvotes) %>% 
  
  # Group by year, state and district to calculate who won in each state-district: 
  group_by(year, state, district) %>% 
  
  filter(share_vote == max(share_vote),
         year >= 2000, 
         year <=2018,
         special == "FALSE") %>% 
  
  # Select only democrat and republican candidates:
  subset((party %in% c("DEMOCRAT", "REPUBLICAN"))) %>% 
  
  # Add congress in each year:
  left_join(congress_year, by = "year") %>%
  ungroup() %>% 
  
  mutate(representation_year_start = year + 1,
         representation_year_end = year + 2,
         state_district_congress = paste(state_po, district, congress))

# Counting number of districts with republican representative for every year
district_count <- hor_won %>%
  mutate(d_rep = ifelse(party == "REPUBLICAN", 1, 0)) %>% 
  group_by(year) %>% 
  count(d_rep, name = "rep_districts") %>% 
  filter(d_rep == 1)


# Plot number of republican controlled districts for each election year:
district_count %>% 
  ggplot(aes(x = year, y = rep_districts)) +
  geom_line(linetype = 1, size = 1) +
  theme_bw() +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "darkgoldenrod2", size = 1) +
  xlab("Election Year") + 
  ylab("Number of Republican districts") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold"))


# Next, we find each facilityID in the emissions_location data frame: 
emissions_location <- emissions_location %>%
  
  # Remove NAs:
  na.omit() %>%
  
  # Create a variable for state_district_congress that we can use to merge
  mutate(district = as.integer(district), 
         state_district_congress = paste(stateAbb, district, congress))


# We create a new data frame which is our main data: 
meta <- hor_won %>% 
  left_join(emissions_location, by = "state_district_congress")  
                
# We split each congress period (two years) to be able to merge the meta data:
year_end <- meta %>% 
  dplyr::select(-representation_year_start) %>% 
  rename(year_power = representation_year_end)

year_start <- meta %>% 
  dplyr::select(-representation_year_end) %>% 
  rename(year_power = representation_year_start)

meta <- year_end %>%
  rbind(year_start) %>% 
  rename(inoffice = year_power)

rm(list = c("year_start", "year_end"))

# Merge years in office with emissions dataset
emissions <- emissions %>% 
  rename(inoffice = reportingYear) %>% 
  left_join(congress_year_inoffice, by = "inoffice")

meta <- meta %>%
  dplyr::select(-congress.x, -district.x) %>% 
  rename(congress = congress.y, district = district.y) %>% 
  left_join(emissions, by = c("inoffice", "facilityID", "congress"))

################################################################################
# DATA CLEANING OF MAIN DATA FRAME
################################################################################

rm(congress_year_inoffice, emissions, hor_won)

# Convert the units of measurement and winsorize all emissions: 
df <- meta %>% 
  
  # Filter and remove years with NAs
  filter(cleanAirActIndicator == "YES",
         inoffice<2017,
         emissionAir != is.na(emissionAir),
         emissionAir == ifelse(unitOfMeasurement == "Grams", emissionAir*0.002205, emissionAir)) %>% 
  
  # Remove unecessary variables:
  dplyr::select(-unitOfMeasurement, 
                -cleanAirActIndicator, 
                -office, 
                -stage, 
                -special, 
                -candidatevotes, 
                -totalvotes,
                -mode,
                -writein,
                -statename)



# Next, we create a panel data set where we have an identity variable called facility_chemical, consisting of facilityID and chemicalID 
# we can use this new variable as entity in a panel data regression.
# We also winsorize all continous variables:
reg.df <- df %>% 
  mutate(facility_chemical = paste0(facilityID,"-",chemicalID),
         d_rep = ifelse(party == "REPUBLICAN", 1, 0),
         emissionAir = Winsorize(emissionAir, probs = c(0.01,0.99)),
         prodIndexNormLevel = Winsorize(prodIndexNormLevel, probs = c(0.01,0.99), na.rm = T),
         prodIndexChange= Winsorize(prodIndexChange, probs = c(0.025,0.975), na.rm = T)) 


hist(reg.df$prodIndexChange, breaks = 100)
hist(log(reg.df$emissionAir), breaks = 100)


###############################################################################
# Descriptive Table
###############################################################################
variable.names(reg.df)

desc.stat <- reg.df %>% 
  group_by(party) %>% 
  summarise(Average_emissions = mean(emissionAir),
            Std_emissions = sd(emissionAir),
            Observations = n(),
            First_congress = min(congress),
            Last_congress = max(congress),
            First_voting_yr = min(year),
            Last_voting_yr = max(year)) %>% 
  ungroup() %>% 
  t() 


colnames(desc.stat) <- desc.stat[1,]
desc.stat <- as.matrix(desc.stat)[-1,]

desc.stat %>%
  kbl(digits = 4) %>%
  kable_classic_2(full_width = F, html_font = "Times New Roman")



###############################################################################
# Regressions
###############################################################################

# Declare the data to be panel data:
pdf <- pdata.frame(reg.df, index = c("facility_chemical", "inoffice"))


# 1.Time panel data regression with republican-dummy as independent variable where:
#               - ENTITY = facility-chamical, i.e. the chemical used at each facility
#               - TIME =  year in office
time_fix_reg <- plm(log(emissionAir) ~ d_rep, data = pdf, model = "within", effect = "time")
se_time_fix_reg <- coeftest(time_fix_reg, vcov=vcovHC(time_fix_reg,cluster="group", type="sss"))[,2]

# 2.Twoway panel data regression with republican-dummy as independent variable where:
#               - ENTITY = facility-chamical, i.e. the chemical used at each facility
#               - TIME =  year in office
twoway_fix_reg <- plm(log(emissionAir) ~ d_rep, data = pdf, model = "within", effect = "twoway")
se_twoway_fix_reg <- coeftest(twoway_fix_reg, vcov=vcovHC(twoway_fix_reg,cluster="group", type="sss"))[,2]


stargazer(list(time_fix_reg, twoway_fix_reg),
          se=list(se_time_fix_reg, se_twoway_fix_reg),
          covariate.labels = c("Republican Dummy", "Change in production level"),
          type="text",
          keep.stat=c("n"),
          report=('vc*t'))

# Idividual fixed effect:
entity_fix_reg <-  plm(log(emissionAir) ~ d_rep, data = pdf, model = "within", effect = "individual")
se_entity_fix_reg <- coeftest(entity_fix_reg, vcov=vcovHC(entity_fix_reg,cluster="group", type="sss"))[,2]

# WHY WE DONT RUN TWO-WAY REGRESSION WITH FACILITY-TIME: 
# We cannot make a panel data set with facility and time without aggregating all pollution on a facility level. 
# E.g., one facility can emit 100 different chemicals which all have a different impact on environment per pound. 
# So by using a panel data set with facility and time we would get duplicated facility-time values, as each facility has multiple chemicals in each time period. 
# Further, in some cases some facilities use different chemicals across facilities. Therefore it is not appropriate to compare the aggregated emissions across facilities. 
# This means that we CANNOT include facility-time effects. 


######################## DiD in 111th and 112th congress ######################
# Treatement identification
reg.df.dem.before <- reg.df %>% 
  filter(inoffice %in% c("2009","2010"),
         party == "DEMOCRAT") 

reg.df.rep.after <- reg.df %>% 
  filter(inoffice %in% c("2011","2012"),
         party == "REPUBLICAN") 

reg.df.rep.after$treatment <- reg.df.rep.after$facility_chemical %in% reg.df.dem.before$facility_chemical # We match the two samples

treatment <- reg.df.rep.after %>% 
  filter(treatment == T)

# Create a new data frame for DiD
reg.df.did <- reg.df %>% 
  filter(inoffice %in% c("2009","2010", "2011", "2012"))
  
reg.df.did$treatment <- reg.df.did$facility_chemical %in% treatment$facility_chemical

reg.df.did <- reg.df.did %>%
  mutate(TREAT = as.double(treatment),
         AFTER = ifelse(congress == 112, 1, 0), 
         prodIndexChange = as.double(prodIndexChange))

reg.pdf.did <- pdata.frame(reg.df.did, index = c("facility_chemical", "inoffice"))

# Without fixed effects: 
did <- plm(log(emissionAir) ~ TREAT*AFTER, data = reg.pdf.did, model = "within", effect = "twoway")
se_did <- coeftest(did, vcov = vcovHC(did, cluster = "group", type="sss"))[,2]

# Final stargazer
stargazer(list(time_fix_reg, twoway_fix_reg, did),
          se=list(se_time_fix_reg, se_twoway_fix_reg, se_did),
          type="html",
          out = "table2.html",
          keep.stat=c("n"),
          report=('vc*t'))

###############################################################################
# TASK 3
###############################################################################

# Calculate which elections that were "randomly" won:
hor2 <- hor %>% 
  mutate(share_vote =as.double(candidatevotes/totalvotes),
         state_district_year = paste0(state_po,"-",district,"-",year)
  ) %>% 
  filter(party %in% c("DEMOCRAT", "REPUBLICAN"),
         year >= 2000, 
         year <=2018) %>% 
  pivot_wider(names_from = party, values_from = share_vote) %>% 
  dplyr::select(state_district_year, DEMOCRAT,REPUBLICAN)

hor.rep <- hor2 %>% 
  dplyr::select(-DEMOCRAT) %>% 
  na.omit()

hor.dem <- hor2 %>% 
  dplyr::select(-REPUBLICAN) %>% 
  na.omit()

# Save it in random df:
random <- hor.dem %>% 
  left_join(hor.rep, by = "state_district_year") %>% 
  na.omit() %>% 
  mutate(vote_margin = abs(DEMOCRAT - REPUBLICAN)) %>% 
  filter(vote_margin <= 0.05)

# Link random election to main data by merging with common variable:
# First, we create the common variable in reg.df: 
reg.df <- reg.df %>% 
  mutate(
    state_district_year = paste0(state_po,"-",district,"-",year)
  )

# Next we match the random elections to main data:
reg.df$random <- as.double(reg.df$state_district_year %in% random$state_district_year)

# Then we filter so that only "randomly" elections are used in the regression:
reg.df.random <- reg.df %>% 
  filter(random == 1)

pdf.random <- pdata.frame(reg.df.random, index = c("facility_chemical", "inoffice"))

# 1.Time panel data regression with republican-dummy as independent variable where:
#               - ENTITY = facility-chamical, i.e. the chemical used at each facility
#               - TIME =  year in office
time_fix_reg <- plm(log(emissionAir) ~ d_rep, data = pdf.random, model = "within", effect = "time")
se_time_fix_reg <- coeftest(time_fix_reg, vcov=vcovHC(time_fix_reg,cluster="group", type="sss"))[,2]

# 2.Twoway panel data regression with republican-dummy as independent variable where:
#               - ENTITY = facility-chamical, i.e. the chemical used at each facility
#               - TIME =  year in office
twoway_fix_reg <- plm(log(emissionAir) ~ d_rep, data = pdf.random, model = "within", effect = "twoway")
se_twoway_fix_reg <- coeftest(twoway_fix_reg, vcov=vcovHC(twoway_fix_reg,cluster="group", type="sss"))[,2]


stargazer(list(time_fix_reg, twoway_fix_reg),
          se=list(se_time_fix_reg, se_twoway_fix_reg),
          covariate.labels = c("Republican Dummy", "Change in production level"),
          type="text",
          keep.stat=c("n"),
          report=('vc*t'))

# Idividual fixed effect:
entity_fix_reg <-  plm(log(emissionAir) ~ d_rep, data = pdf, model = "within", effect = "individual")
se_entity_fix_reg <- coeftest(entity_fix_reg, vcov=vcovHC(entity_fix_reg,cluster="group", type="sss"))[,2]

######################## DiD in 111th and 112th congress ######################
# Treatement identification
reg.df.dem.before <- reg.df.random %>% 
  filter(inoffice %in% c("2009","2010"),
         party == "DEMOCRAT") 

reg.df.rep.after <- reg.df.random %>% 
  filter(inoffice %in% c("2011","2012"),
         party == "REPUBLICAN") 

reg.df.rep.after$treatment <- reg.df.rep.after$facility_chemical %in% reg.df.dem.before$facility_chemical # We match the two samples

treatment <- reg.df.rep.after %>% 
  filter(treatment == T)

# Create a new data frame for DiD
reg.df.did <- reg.df.random %>% 
  filter(inoffice %in% c("2009","2010", "2011", "2012"))

length(unique(reg.df.did$facilityID))

reg.df.did$treatment <- reg.df.did$facility_chemical %in% treatment$facility_chemical

reg.df.did <- reg.df.did %>%
  mutate(TREAT = as.double(treatment),
         AFTER = ifelse(congress == 112, 1, 0), 
         prodIndexChange = as.double(prodIndexChange))

reg.pdf.did <- pdata.frame(reg.df.did, index = c("facility_chemical", "inoffice"))

# Without fixed effects: 
did <- plm(log(emissionAir) ~ TREAT*AFTER, data = reg.pdf.did, model = "within", effect = "time")
se_did <- coeftest(did, vcov = vcovHC(did, cluster = "group", type="sss"))[,2]

# Final stargazer
stargazer(list(time_fix_reg, twoway_fix_reg, did),
          se=list(se_time_fix_reg, se_twoway_fix_reg, se_did),
          type="html",
          out = "table3.html",
          keep.stat=c("n"),
          report=('vc*t'))

################################################################################
# TASK 4
################################################################################

# Create a variable with firm identifier and year in office
reg.df <- reg.df %>% 
  mutate(firmID_inoffice = paste0(dbnrParent,"-",inoffice))

# Counting the number of facilities each firm have in republican and democrat districts
firm.df <- reg.df %>%
  distinct(facilityID, .keep_all = T) %>%
  group_by(party, firmID_inoffice) %>% 
  count(party) %>% 
  ungroup() %>% 
  pivot_wider(values_from = n, names_from = party, values_fill = 0)

# Creating a dummy variable for exposure to republican districts 
reg.df.firm <- reg.df %>% 
  left_join(firm.df, by="firmID_inoffice") %>% 
  filter(dbnrParent != is.na(dbnrParent)) %>% 
  mutate(DEMOCRAT = ifelse(is.na(DEMOCRAT), 0, DEMOCRAT),
         REPUBLICAN = ifelse(is.na(REPUBLICAN), 0, REPUBLICAN)) %>% 
  rename(facilities_rep = REPUBLICAN,
         facilities_dem = DEMOCRAT) %>% 
  mutate(exposure = facilities_rep - facilities_dem,
         exposed_rep = ifelse(exposure>0, 1, 0))

######################## DiD in 111th and 112th congress ######################
# Treatement identification
reg.df.dem.before.firm <- reg.df.firm %>% 
  filter(inoffice %in% c("2009","2010"),
         party == "DEMOCRAT") 

reg.df.rep.after.firm <- reg.df.firm %>% 
  filter(inoffice %in% c("2011","2012"),
         party == "REPUBLICAN") 

reg.df.rep.after.firm$treatment <- reg.df.rep.after.firm$facility_chemical %in% reg.df.dem.before.firm$facility_chemical # We match the two samples

treatment <- reg.df.rep.after.firm %>% 
  filter(treatment == T)

# Create a new data frame for DiD
reg.df.firm.did <- reg.df.firm %>% 
  filter(inoffice %in% c("2009","2010", "2011", "2012"))

length(unique(reg.df.firm.did$facilityID))

reg.df.firm.did$treatment <- reg.df.firm.did$facility_chemical %in% treatment$facility_chemical

reg.df.firm.did <- reg.df.firm.did %>%
  mutate(TREAT = as.double(treatment),
         AFTER = ifelse(congress == 112, 1, 0), 
         prodIndexChange = as.double(prodIndexChange))

reg.pdf.firm.did <- pdata.frame(reg.df.firm.did, index = c("facility_chemical", "inoffice"))

# Regression models DiD: 
did.firm1 <- plm(log(emissionAir) ~ exposed_rep + TREAT*AFTER, data = reg.pdf.firm.did, model = "pooling")
se.did.firm1 <- coeftest(did.firm1, vcov = vcovHC(did.firm1, cluster = "group", type="sss"))[,2]

did.firm2 <- plm(log(emissionAir) ~ exposed_rep + TREAT*AFTER, data = reg.pdf.firm.did, model = "within", effect = "time")
se.did.firm2 <- coeftest(did.firm2, vcov = vcovHC(did.firm2, cluster = "group", type="sss"))[,2]

did.firm3 <- plm(log(emissionAir) ~ exposed_rep + TREAT*AFTER, data = reg.pdf.firm.did, model = "within", effect = "individual")
se.did.firm3 <- coeftest(did.firm3, vcov = vcovHC(did.firm3, cluster = "group", type="sss"))[,2]

summary(did.firm1)

# Final stargazer for tabel 4
stargazer(list(did.firm1, did.firm2, did.firm3),
          se=list(se.did.firm1, se.did.firm2, se.did.firm3),
          type="html",
          out = "table4.html",
          keep.stat=c("n"),
          report=('vc*t'))


