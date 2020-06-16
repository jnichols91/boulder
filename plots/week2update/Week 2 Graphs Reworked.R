
###Graph 1 of Alum 
alumphos <- phosfax_10m %>% # phosfax only alum coagulant every 10 minutes 
  filter(date >= ymd("2019-10-22")) %>%
  filter(date <= ymd("2019-12-15"))

rects1 <- data.frame(xstart = as.POSIXct('2019-11-06 15:00:00'), 
                     xend = as.POSIXct('2019-12-01 00:00:00'))

a1 <- ggplot() + 
  geom_point(data = alumphos, aes(date, op_conc_mg_p_l, color = "op_conc_mg_p_l"), size = 4) + 
  geom_point(data = alum_data, aes(date, op_conc_mg_p_l_hourly, color = "op_conc_mg_p_l_hourly"), size = 4) +
  theme(plot.title = element_text(face="bold", size = 40, hjust = 0.5),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 40),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30)) +
  scale_color_brewer(palette = "Dark2", name = "Effluent Phosphorus", labels = c("Original Data", "Merged Data")) +
  labs(title = "Alum Effluent OP",
       x = "Date",
       y = "Ortho-phosphate (mg/L)") +
  ylim(0, 4) +
  geom_rect(data = rects1, aes(xmin = xstart, xmax = xend, 
    ymin = -Inf, ymax = Inf),fill = "red",col = "red", alpha = 0.1)

  
grid.arrange(a1, ncol = 1)

###Graph 2 of Ferric

ferrphos <- phosfax_10m %>%
  filter(date >= ymd("2019-08-15")) %>%
  filter(date <= ymd("2019-09-07"))

rects2 <- data.frame(xstart = as.POSIXct('2019-08-30 15:00:00'), 
                     xend = as.POSIXct('2019-09-02 10:00:00'))

b1 <- ggplot() + 
  geom_point(data = ferrphos, aes(date, op_conc_mg_p_l, color = "op_conc_mg_p_l"), size = 4, alpha = .8) + 
  geom_point(data = ferr_data, aes(date, op_conc_mg_p_l_hourly, color = "op_conc_mg_p_l_hourly"), size = 4) +
  theme(plot.title = element_text(face="bold", size = 40, hjust = 0.5),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 40),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30)) +
  scale_color_brewer(palette = "Dark2", name = "Effluent Phosphorus", labels = c("Original Data", "Merged Data")) +
  labs(title = "Ferric Effluent OP",
       x = "Date",
       y = "Ortho-phosphate (mg/L)") +
  ylim(0, 4) +geom_rect(data = rects2, aes(xmin = xstart, xmax = xend, 
            ymin = -Inf, ymax = Inf),fill = "red",col = "red", alpha = 0.1)

grid.arrange(b1, ncol = 1)



#### Graph 3 Ferric and Alum OP Difference

ferr_op_melt <- melt(ferr_data[,c(1,3:4)], id = "date")

c1 <- ferr_op_melt %>% ggplot() + 
  geom_point(aes(date, value, col = variable), size = 4) +  
  labs(title = "Ferric OP Concentration at Dosing",
       x = "Date",
       y = "Hourly OP (mg/L)") +
  scale_color_brewer(palette = "Dark2", name = "Location", labels = c("Dosing Station", "Effluent")) +
  theme(legend.title = element_text(size = 40),
        legend.text = element_text(size = 30),
        plot.title = element_text(face="bold", size = 40, hjust = 0.5),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 40),
        strip.text.x = element_text(size = 30),
        legend.position = c(.86,.89),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key = element_blank()) +
  ylim(0,4)

alum_op_melt <- melt(alum_data[,c(1,3:4)], id = "date")


c2 <- alum_op_melt %>% ggplot() + 
  geom_point(aes(date, value, col = variable), size = 4) +  
  labs(title = "Alum OP Concentration at Dosing",
       x = "Date",
       y = "Hourly OP (mg/L)") +
  scale_color_brewer(palette = "Dark2", name = "Location", labels = c("Dosing Station", "Effluent")) +
  theme(legend.title = element_text(size = 40),
        legend.text = element_text(size = 30),
        plot.title = element_text(face="bold", size = 40, hjust = 0.5),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 40),
        strip.text.x = element_text(size = 30),
        legend.position = c(.86,.89),
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "black"),
        legend.key = element_blank()) +
  ylim(0,4)

grid.arrange(c1, c2, ncol = 2)

####Graph 4 Ferric and Alum Concentration
install.packages("ggpmisc")
library("ggpmisc")
install.packages("ggpubr")
library("ggpubr")

d1 <- ferr_data %>% ggplot(aes(op_mg_p_l, op_conc_mg_p_l_hourly)) + 
  geom_point(size = 4) +
  stat_smooth(method="lm", se= TRUE) + 
  stat_regline_equation(size = 10) +
  labs(title = "Ferric Effluent vs. Dosing OP Concentrations",
       x = "Dosing OP (mg/L)",
       y = "Effluent OP (mg/L)") +
  theme(plot.title = element_text(face="bold", size = 40, hjust = 0.5),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 40)) +
  ylim(0, 3.5)

d2 <- alum_data %>% ggplot(aes(op_mg_p_l, op_conc_mg_p_l_hourly)) + 
  geom_point(size = 4) +
  stat_smooth(method="lm", se= TRUE) + 
  stat_regline_equation(size = 10) +
  labs(title = "Alum Effluent vs. Dosing OP Concentrations",
       x = "Dosing OP (mg/L)",
       y = "Effluent OP (mg/L)") +
  theme(plot.title = element_text(face="bold", size = 40, hjust = 0.5),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 40)) +
  ylim(0,3.5)

grid.arrange(d1, d2, ncol = 2)



#pairs test to see if any variables are linear

pairs(ferr_data)
pairs(alum_data)


#Ancova Test
aov(data = ferr_data[3:14], op_mg_p_l ~ .)

aov(data = ferr_data, op_mg_p_l ~  op_conc_mg_p_l_hourly)

plot(data = ferr_data, x = op_conc_mg_p_l_hourly, y = ras_gpm)


