library(tidyverse)
library(scales)
library(prais)
library(DT)
data <- read_csv(r'(aligned_data.csv)')


data <- data %>%
  # Define variables time, event (intervention), and time_event (time after intervention)
  mutate(time = row_number())%>%
  mutate(event = if_else(time >=4,1,0))%>%
  mutate(time_event = case_when(event == 1 ~ row_number()-3, TRUE~0))


ggplot(data = data)+
  # Plotting line chart
  geom_point(aes(x = Year, y = depression_prev),color ='orange')+
  geom_line(aes(x = Year, y = depression_prev),group =1,color ='orange')+
  geom_point(aes(x = Year, y = ui_prev),color ='blue')+
  geom_line(aes(x = Year, y = ui_prev),group =1,color ='blue')+
  # Plotting vertical line to segmented the time series (before and after event)
  geom_vline(xintercept = 2014, linetype="dotdash", color ='red',size=1.2)+
  # Following code is aesthetic adjustment
  scale_y_continuous(limits = c(0,0.25),labels = comma)+
  xlab('Time')+
  ylab('Prevalence')+
  labs(title = 'Time Series Depression UI')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3,size = 16),
        axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size=20))

# Fit regression for depression
model <- prais::prais_winsten(depression_prev~ time + event + time_event,
                              index ='time', 
                              data = data)

summary(model)
# Fit regression for UI
model2 <- prais::prais_winsten(ui_prev~ time + event + time_event,
                              index ='time', 
                              data = data)
summary(model2)
# Add model coefficients to tha table
data<- data%>%
  mutate(factual_trend= model$coefficients[1]+
           model$coefficients[2]*time+
           model$coefficients[3]*event+
           model$coefficients[4]*time_event
  )%>%
  mutate(counter_fact = model$coefficients[1]+
           model$coefficients[2]*time)

# Add model coefficients to tha table
data<- data%>%
  mutate(factual_trend2= model2$coefficients[1]+
           model2$coefficients[2]*time+
           model2$coefficients[3]*event+
           model2$coefficients[4]*time_event
  )%>%
  mutate(counter_fact2 = model2$coefficients[1]+
           model2$coefficients[2]*time)

# plot regression model for Depression
plot1 <- ggplot(data = data)+
  ##Plotting line chart
  geom_point(aes(x = Year, y = depression_prev),size=2)+
  geom_line(aes(x = Year, y = depression_prev),group =1,size=0.8)+
  ##Plotting vertical line to segmented the time series (before and after COVID-19)
  geom_vline(xintercept = 2013, linetype="dotdash", color ='red',size=1.2)+
  ##plotting factual trend
  geom_line(data = data%>%filter(time>=4)
            ,aes(x = Year, y = factual_trend,color = 'Regression line after 2014'),group =1, linetype="dashed", size=1.2)+
  ##plotting counter-factual trend
  geom_line(aes(x = Year, y = counter_fact,color = 'Regression line before 2014'),group =1, linetype="dashed", size=1.2)+
  ## Setting x axis labels
  scale_x_continuous(breaks = seq(2008, 2022, by = 1)) + 
  ##Following code is aesthetic adjustment
  scale_y_continuous(limits = c(0,15),labels = comma)+
  scale_color_manual(values = c('blue', 'darkorange'))+
  xlab('Year')+
  ylab('Prevalence (%)')+
  labs(title = 'Age-standardized depression prevalence',color='')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6,size = 20,color='black'),
        axis.text.y = element_text(size = 20,color='black'),
        axis.title.x = element_text(size = 20, vjust = 0.2,color = "black"),
        axis.title.y = element_text(size = 20, color = "black"),
        axis.text = element_text(size=20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20,hjust = 0.5),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
        legend.position = c(0.75,0.9),
        legend.text = element_text(size=16),
        legend.background = element_rect(fill = "white", colour = "black",linewidth = 0.4),
        legend.title = element_blank())

ggsave("plot_depression.png", plot = plot1, width = 10, height = 7, dpi = 300)


# plot regression model for UI
plot2 <- ggplot(data = data)+
  ##Plotting line chart
  geom_point(aes(x = Year, y = ui_prev),size=2)+
  geom_line(aes(x = Year, y = ui_prev),group =1,size=0.8)+
  ##Plotting vertical line to segmented the time series (before and after COVID-19)
  geom_vline(xintercept = 2013, linetype="dotdash", color ='red',size=1.2)+
  ##plotting factual trend
  geom_line(data = data%>%filter(time>=4)
            ,aes(x = Year, y = factual_trend2,color = 'Regression line after 2014'),group =1, linetype="dashed", size=1.2)+
  ##plotting counter-factual trend
  geom_line(aes(x = Year, y = counter_fact2,color = 'Regression line before 2014'),group =1, linetype="dashed", size=1.2)+
  ## Setting x axis labels
  scale_x_continuous(breaks = seq(2008, 2022, by = 1)) + 
  ##Following code is aesthetic adjustment
  scale_y_continuous(limits = c(0,15),labels = comma)+
  scale_color_manual(values = c('blue', 'darkorange'))+
  xlab('Year')+
  ylab('Prevalence (%)')+
  labs(title = 'Age-standardized UI prevalence',color='')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6,size = 20,color='black'),
        axis.text.y = element_text(size = 20,color='black'),
        axis.title.x = element_text(size = 20, vjust = 0.2,color = "black"),
        axis.title.y = element_text(size = 20, color = "black"),
        axis.text = element_text(size=20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size=20,hjust = 0.5),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
        legend.position = c(0.75,0.9),
        legend.text = element_text(size=16),
        legend.background = element_rect(fill = "white", colour = "black",linewidth = 0.4),
        legend.title = element_blank())


ggsave("plot_UI.png", plot = plot2, width = 10, height = 7, dpi = 300)

model_linear <- prais::prais_winsten(depression_prev ~ time, data = data, index = 'time')
linear_model <- lm(depression_prev ~ time, data=data)
summary(model_linear)

residuals <- resid(model)
fitted <- fitted(model)
plot(fitted, residuals, main = "Residuals vs Fitted")
abline(h = 0, col = "red")

qqnorm(residuals)
qqline(residuals, col = "red")
shapiro.test(residuals)

residuals <- resid(model2)
fitted <- fitted(model2)
plot(fitted, residuals, main = "Residuals vs Fitted")
abline(h = 0, col = "red")

qqnorm(residuals)
qqline(residuals, col = "red")
shapiro.test(residuals)

coefs <- summary(model)$coefficients
estimates <- coefs[, "Estimate"]
std_errors <- coefs[, "Std. Error"]

# Compute 95% confidence intervals manually
conf_int <- data.frame(
  Term = rownames(coefs),
  Estimate = estimates,
  Lower = estimates - 1.96 * std_errors,
  Upper = estimates + 1.96 * std_errors
)

print(conf_int)
