source("RAgena.R")

model <- from_cmpx("Models/CarCosts.cmpx")
network <- model$networks[[1]]

model$create_dataSet(id = "sa")

sa_config <- create_sensitivity_config(
  target="total_cost",
  sensitivity_nodes="*",
  dataset="sa",
  report_settings = list(summaryStats = c("mean", "variance"))
)

credentials <- login("test@example.com", "1234567890")

sensitivity_analysis(model, credentials, sa_config)