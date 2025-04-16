
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
to_add <- data.frame(matrix(NA, empty_bar*nlevels(as.factor(npaper_per_conti$continent2)), ncol(npaper_per_conti)))
colnames(to_add) <- colnames(npaper_per_conti)
to_add$continent2 <- rep(levels(as.factor(npaper_per_conti$continent2)), each = empty_bar)
data <- rbind(npaper_per_conti, to_add)
data <- data %>% arrange(continent2, oro_branch, -n_mean)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(continent2) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]


ggplot(data, aes(x = id, y = n_mean, fill = oro_type)) +
  
  # Bars
  geom_col(position = position_dodge(), show.legend = T) +
  geom_errorbar(aes(ymin = n_lower, ymax = n_upper), 
                    position = position_dodge(0.9),
                    width = .2) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  # geom_segment(data=grid_data, aes(x = end, y = 10000, xend = start, yend = 10000), colour = "grey10", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # geom_segment(data=grid_data, aes(x = end, y = 7500, xend = start, yend = 7500), colour = "grey10", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # geom_segment(data=grid_data, aes(x = end, y = 5000, xend = start, yend = 5000), colour = "grey10", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # geom_segment(data=grid_data, aes(x = end, y = 2500, xend = start, yend = 2500), colour = "grey10", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey10", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # 
  # Add text showing the value of each 100/75/50/25 lines
  # annotate("text", x = rep(max(data$id),5), y = c(0, 2500, 5000, 7500, 10000), label = c("0", "2500", "5000", "7500", "10000") , color="grey10", size=3 , angle=0, fontface="bold", hjust=1) +
  
  # Colors
  scale_fill_manual(name   = "ORO type",
                    values = c("Marine renewable energy"            = "#026996",
                               "CO2 removal or storage"             = "#0688c2",
                               "Increase efficiency"                = "#9ed7f0",                               
                               "Conservation"                       = "#078257",
                               "Human assisted evolution"           = "#43b08a",
                               "Built infrastructure & technology"  = "#600787",
                               "Socio-institutional"                = "#ad5ad1")) +
  # scale_x_discrete(limits = levels(temp$valueOrder), 
  #                  labels = gsub("_"," ", temp$oro_type[order(temp$valueOrder)]))+
  #ylim(c(0, max(temp$n_upper)+2000))+
  labs(y = "Articles predicted relevant (n)") +
  # geom_col(position = position_dodge(), show.legend = F) +
  # ylim(-5000, 12000) +
  theme_bw() +
  theme(legend.position = c(0.85,0.75),
        axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.title.y    = element_text(size = 16),
        axis.text.y     = element_text(size = 14),
        legend.text     = element_text(size = 14),
        legend.title    = element_text(size = 16)) +
  # coord_polar() +
  
  # geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  # geom_segment(data = base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE ) +
  geom_text(data = base_data, aes(x = title, y = -500, label=continent2), hjust=c(rep(0.5,6)), colour = "black", alpha=0.8, size=5.5, fontface="bold", inherit.aes = FALSE)

ggsave(here::here("figures", "main", "Figure6_B.jpeg"), width = 13, height = 5, device = "jpeg")

