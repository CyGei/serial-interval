# Incubation Period
# Tan et al
incub_mu <- 5.7
incub_sd <- 3.5

incub <- VirusWatch::make_dist("gamma",
                               incub_mu,
                               incub_sd,
                               w = 1)

incub$plot +
  xlim(0, 20) +
  labs(title = "incubation period")

# Generation Time
# Ganyani et al
gi_mu <- 3.95
gi_sd <- 1.51


gi <- VirusWatch::make_dist("gamma",
                            gi_mu,
                            gi_sd,
                            w = 1)

gi$plot +
  xlim(0, 15) +
  labs(title = "generation time")

cat(
  blue(
    "##################################################\n" %+%
      green$underline$bold("single_distributions.R ran successfully\n") %+%
      "##################################################\n"
  )
)