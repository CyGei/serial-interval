fig_overall_si <- all_si_df %>%
  select(serial_interval) %>%
  count(serial_interval) %>%
  ggplot() +
  aes(x = serial_interval, y = n) +
  geom_col(col = "black", fill = "steelblue") +
  geom_line(size = 1.2)+
  geom_area(alpha = 0.2)+
  geom_vline(aes(xintercept = mean(all_si_df$serial_interval, na.rm = TRUE)),
             size = 1.3,
             col = "red",
             linetype = "dotted")+
  theme_minimal()+
  labs(title = "Serial Interval of Covid-19")


#print(fig_overall_si)




fig_violin <- all_si_df %>%
  ggplot() +
  aes(x = Variant,
      y = serial_interval,
      fill = Variant) +
  geom_violin(
    #scale = "count",
    adjust = 1.5,
    draw_quantiles = c(0.1, 0.9),
    alpha = 0.3,
    color = "white"
  ) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(
    width = 0.1,
    outlier.shape = 4,
    outlier.size = 1,
    color = "black"
  ) +
  stat_summary(
    fun.y = mean,
    geom = "point",
    shape = 17,
    color = "black"
  ) +
  theme_minimal() +
  labs(x = "",
       y = "serial interval (days)",
       fill = "Variant")


#print(fig_violin)

fig_si_conf <- table_variants %>%
  ggplot() +
  aes(y = mean, x = Variant, color = Variant) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lwr.ci, ymax = upr.ci), width = 0.5) +
  theme_minimal() +
  labs(x = "",
       y = "serial interval (days)")

#print(fig_si_conf)



panel_conf_violin <-
  ggarrange(
    fig_si_conf + theme(axis.text.x = element_blank()),
    fig_violin,
    labels = c("A", "B"),
    nrow = 2
  )

print(panel_conf_violin)


fig_ridgeline <- all_si_df %>%
  ggplot() +
  aes(
    x = serial_interval,
    y = Variant,
    group = Variant,
    fill = 0.5 - abs(0.5 - stat(ecdf))
  ) +
  ggridges::geom_density_ridges_gradient(calc_ecdf = TRUE, quantile_lines = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability") +
  scale_x_continuous(breaks = seq(
    min(all_si_df$serial_interval),
    max(all_si_df$serial_interval),
    by = 2
  )) +
  geom_vline(
    xintercept = 0,
    size = 2,
    linetype = "dotted",
    color = "darkgrey"
  ) +
  theme_ridges()








fig_histogram_ridges <- all_si_df %>%
  select(serial_interval, Variant) %>%
  group_by(Variant) %>%
  count(serial_interval) %>%
  ggplot() +
  aes(x = serial_interval, y = n, fill = Variant) +
  geom_col(col = "black") +
  # geom_line(size = 0.5) +
  # geom_area(alpha = 0.5 ) +
  facet_wrap( ~ Variant, scales = "free_y", ncol = 1) +
  geom_vline(data =
               all_si_df %>%
               filter(str_detect(Variant, "0")), 
             aes(xintercept = mean(serial_interval)))+
  geom_vline(data =
               all_si_df %>%
               filter(str_detect(Variant, "1")), 
             aes(xintercept = mean(serial_interval)))+
  geom_vline(data =
               all_si_df %>%
               filter(str_detect(Variant, "2")), 
             aes(xintercept = mean(serial_interval)))+
  geom_vline(data =
               all_si_df %>%
               filter(str_detect(Variant, "3")), 
             aes(xintercept = mean(serial_interval)))+
  geom_vline(data =
               all_si_df %>%
               filter(str_detect(Variant, "4")), 
             aes(xintercept = mean(serial_interval)))+
  geom_vline(
    xintercept = 0,
    size = 2,
    linetype = "dotted",
    color = "darkgrey"
  )+
  scale_x_continuous(breaks = seq(
    min(all_si_df$serial_interval),
    max(all_si_df$serial_interval),
    by = 2
  ))+
  theme_minimal() +
  theme(strip.text.x = element_blank())+
  labs(title = "Serial Interval of Covid-19")



