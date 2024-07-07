
figure_efficacy <- figure_data %>% 
  filter(Outcome == "Composite") %>% 
  filter(scenario  %in% c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4',
                          'Scenario 5', 'Scenario 6'))

# Convert to factor for ordering
figure_efficacy$scenario <- factor(figure_efficacy$scenario, 
                                   levels = unique(figure_efficacy$scenario))
figure_efficacy$Analysis <- factor(figure_efficacy$Analysis, 
                                   levels = rev(unique(figure_efficacy$Analysis)))

#bitmap("Primary efficacy outcome.tiff", width = 5, height = 10, units = 'in', type = 'tifflzw', res=600)

#pdf("Primary efficacy outcome.pdf", height = 10)

# Create the plot
ggplot(figure_efficacy) +
  # Incidence bars for Drug A and Comparator
  geom_bar(aes(y = Analysis, x = risk_initiators, fill = "Initiators"), stat = "identity", 
           position = position_nudge(y = 0.2, x = 0.2), width = 0.4, alpha = 0.7) +
  geom_bar(aes(y = Analysis, x = risk_non_initiators, fill = "Non-Initiators"), stat = "identity", 
           position = position_nudge(y = -0.2, x = 0.2), width = 0.4, alpha = 0.4) +
  # Points and error bars for Risk Difference
  geom_point(aes(y = Analysis, x = rd), 
             #position = position_dodge(width = 0.8),
             #position = position_nudge(x = 0.7)
  ) +
  geom_errorbar(aes(y = Analysis, xmin = rd_lcl, xmax = rd_ucl), 
                #position = position_dodge(width = 0.8), 
                #position = position_nudge(x = 0.7),
                width = 0.2) +
  facet_wrap(scenario ~ ., ncol = 1, strip.position = "left") +
  # Customizing the axes
  scale_x_continuous(
    name = "Risk Difference (per 100)",
    limits = c(-0.1, 0.80),
    breaks = seq(-0.1, 0.1, by = 0.1),
    sec.axis = sec_axis(~ ., name = "Incidence (%)",
                        breaks = seq(0.2, 0.8, 0.2),
                        labels = c(0, 0.2, 0.4, 0.6))
  )  +
  geom_vline(xintercept = 0.15, ) +
  # geom_vline(data = function(x) x %>% 
  #              filter(Analysis == 'Truth') %>% 
  #              group_by(scenario) %>% 
  #              summarize(truth = min(rd)),
  #            aes(xintercept = truth), color = "black", linetype = 'dotted', linewidth = 1) +
  #scale_fill_manual(values = c("Drug A" = "blue", "Comparator" = "red")) +
  #scale_color_manual(values = c("Female" = "blue", "Male" = "red")) +
  labs(y = NULL, 
       x = NULL,
       fill = "Treatment Group", 
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.x.top = element_text(hjust = 0.7),
        axis.title.x.bottom = element_text(hjust = 0)) +
  theme(strip.placement = "outside") +
  geom_rect(xmin = -0.1, xmax = 0.1,
            ymin = -0.5, ymax = 3.5,
            alpha = 0.1)

#dev.off()