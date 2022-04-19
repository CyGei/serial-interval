

output_folder <- paste0("Results/",
                        last_update,
                        "/", 
                        "max_si_", 
                        max_si,
                        "/")

output_folder <- ifelse(prune_households==TRUE, 
                        paste0(output_folder, "prune_TRUE_", household_size),
                        paste0(output_folder, "prune_FALSE"))
output_folder <- paste0(getwd(), "/", output_folder)
output_folder
dir.create(output_folder, recursive = TRUE)

warning("saving results for the parameters.......")
cat("max_si:",max_si, "\n",
          "prune_households:", prune_households, "\n",
          "household_size:", household_size)

setwd(output_folder)
ggsave("panel_conf_violin.png", panel_conf_violin)
ggsave("panel_hist_gamma.png", panel_hist_gamma)

table_demographics %>% gtsummary::as_flex_table() %>% 
  flextable::save_as_docx(path = paste0(output_folder, "/table_demographics.docx"))
table_variants %>% flextable::flextable() %>% 
  flextable::save_as_docx(path = paste0(output_folder, "/table_variants.docx"))
table_gamma_dist %>% flextable::flextable() %>% 
  flextable::save_as_docx(path = paste0(output_folder, "/table_gamma_dist.docx"))

warning("results saved in ")
paste(output_folder)