## FIGURE 6: Contrasted ORO types addressed as a function of GDP / energy demand

## PANEL A: Map – GDP/per capita (cartogram ?) ; color per continent showing energy demand 
# (test first correlation ?)

# here the relevant files:
gdp_per_capita <- readr::read_csv(here::here("data/external/gdp-per-capita/gdp-per-capita-worldbank-2021.csv"),
                                  show_col_types = FALSE)

# File for energy demand. Note the two following variables may be of interest:
# energy_per_capita and energy_per_gdp
energy_demand <- readr::read_csv(here::here("data/external/ghg-emissions/owid-co2-data.csv"),
                                 show_col_types = FALSE)





## PANEL B: Bar plot per continent based on #pub / ORO type – relative distribution of ORO types

# For this panel, the predictions for the different ORO types are grouped into 
# separate tables based on which branch they are relevant for. So make sure to combine them all, and
# order them along the x axis so that they are grouped by ORO branch (i.e. Mitigation vs Natural vs Societal)
# and not alphabetically 

# Read in the formatted data table from the database
pred_oro_type_long <- tbl(dbcon, "pred_oro_type_long") %>% collect()
uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references

# Join with author affiliation data from uniquerefs
pred_oro_type_continent <- uniquerefs %>%
  select(analysis_id, affiliation) %>%
  collect()%>%
  right_join(pred_oro_type_long, by = "analysis_id")

# Now just need to match each affiliation to a continent, and then summarize the number of 
# predicted oro types according to the prediction boundary for mean and lower and upper 
# the latter two will form the whiskers of the barplot


## My previous code for the BARPLOT OF NUMBER OF ARTICLES PREDICTED RELEVANT FOR EACH TYPE 
# you can adapt this code to break it up by region 
branchPal <- c("Mitigation" = "#35a7d9","Nature" = "forestgreen", "Societal"="#7670a8")

temp <- pred_oro_type_long %>%
  group_by(oro_branch, oro_type)%>%
  summarise(n_mean = sum(0.5 <= mean),
            n_lower = sum(0.5 <= lower),
            n_upper = sum(0.5 <= upper))%>%
  arrange(oro_branch,n_mean)%>%
  ungroup()%>%
  mutate(valueOrder = as.factor(row_number()))

ggplot(temp, aes(x=valueOrder, y=n_mean, fill = oro_branch))+
  geom_col()+
  geom_errorbar(aes(ymin = n_lower, ymax = n_upper), width=.2)+
  geom_text(aes(label = n_mean), nudge_y = 1000, size=3, col="red")+ #nudge_x=0.2,
  scale_fill_manual(name = "ORO Branch", values = as.vector(branchPal))+
  scale_x_discrete(limits = levels(temp$valueOrder), 
                   labels = gsub("_"," ", temp$oro_type[order(temp$valueOrder)]))+
  #ylim(c(0, max(temp$n_upper)+2000))+
  labs(y="Articles predicted relevant (n)")+
  theme(
    panel.background = element_rect(fill="white",colour = "black"),
    panel.grid.major = element_line(colour = "grey"),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank()
  )




