# Fixed cost parameters
lodge_d      <- 250     # daily lodging cost per family
days         <- 13      # duration of trip
lodge_total  <- lodge_d * days  # total lodging cost per family
activity.adult <- 500   # total activity cost per adult (>12 years) over 14 days
activity.child <- 250   # total activity cost per child (<=12 years) over 14 days
labor_total    <- 6000    # total labor cost for 14 days
tot.miles.travel <- 2500 # total estimated mileage 
minivan.mpg  <- 20
van.mpg      <- 15
gas.price    <- 2.89
base_margin_rate <- 0.3 # base target profit margin rate

# Define valid family compositions (1-2 adults, 0-2 children)
compositions <- data.frame(
  num_adult    = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4),
  num_children = c(0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3)
)

# Create an empty data frame to store results
results <- data.frame(
  family_id = character(),
  family_number = integer(),
  family_size = integer(),
  num_adult = integer(),
  num_children = integer(),
  total_num_people=integer(),
  car_rental_cost = numeric(),
  travel_expense = numeric(),
  family_activity_cost = numeric(),
  family_lodge_cost    = numeric(),
  family_total_cost    = numeric(),
  family_person_cost    = numeric(),
  family_total_price   = numeric(),
  family_person_price   = numeric(),
  family_total_profit  = numeric()
)

# Loop over family numbers (1 to 4)
for (fam_no in 1:4) {
  # Loop over each family in the scenario
  for (fam_idx in 1:fam_no) {
    family_id <- paste0("F", fam_idx)
    # Loop over all valid compositions for this family
    for (comp_idx in 1:nrow(compositions)) {
      num_adult <- compositions$num_adult[comp_idx]
      num_children <- compositions$num_children[comp_idx]
      family_size <- num_adult + num_children
      total_no.people <- fam_no*family_size
      # Determine car rental cost based on total number of people 
      if (total_no.people <= 5) {
        car_rental <- 1500  # 1 minivan
        tot.travel.expense <- (tot.miles.travel/minivan.mpg)*gas.price  # total travel expense
      } else if (total_no.people %in% 6:12) {
        car_rental <- 2000  # 1 full-size van
        tot.travel.expense <- (tot.miles.travel/van.mpg) * gas.price  # total travel expense
      } else if (total_no.people >12) {
        car_rental <- 3500  # 1 full-size van + 1 minivan
        tot.travel.expense <- (tot.miles.travel/van.mpg) * gas.price + (tot.miles.travel/minivan.mpg)*gas.price  # total travel expense
      }
      # Calculate activity cost for the family
      activity_cost <- activity.adult * num_adult + activity.child * num_children
      # Calculate total cost for the family
      tot_cost_family <- (lodge_total + activity_cost) + (labor_total + car_rental + tot.travel.expense)/fam_no
      # Calculate per person cost for the family
      family_person_cost <- tot_cost_family/family_size
      # Calculate weighted target margin rate (10% increase per family)
      target_margin_rate <- base_margin_rate * (1.1)^(fam_no - 1)
      # Calculate target profit and total price for the family
      target_profit_from_the_family <- target_margin_rate * tot_cost_family / (1 - target_margin_rate)
      tot_price_per_family <- tot_cost_family + target_profit_from_the_family
      # Calculate per person price for the family
      person_price_per_family <- tot_price_per_family/family_size
      # Store results in data frame
      results <- rbind(results, data.frame(
        family_id = family_id,
        family_number = fam_no,
        family_size = family_size,
        num_adult = num_adult,
        num_children = num_children,
        total_num_people=total_no.people,
        car_rental_cost = car_rental,
        travel_expense = tot.travel.expense,
        family_activity_cost = activity_cost,
        family_lodge_cost    = lodge_total,
        family_total_cost    = tot_cost_family,
        family_person_cost    = family_person_cost,
        family_total_price   = tot_price_per_family,
        family_person_price   = person_price_per_family,
        family_total_profit  = target_profit_from_the_family
      ))
    }
  }
}

# Save results for Shiny app
saveRDS(results, "tour_cost_results.rds")
# View the results
View(results)

library(ggplot2)
# Create the plot
ggplot(results, aes(x = family_size, y = family_person_price, color = factor(num_adult), shape = factor(num_children))) +
  geom_point(size = 3) +
  facet_wrap(~ family_number, ncol = 2, labeller = labeller(family_number = function(x) paste("Family Number =", x))) +
  scale_color_manual(values = c("1" = "blue", "2" = "red", "3" = "green", "4" = "purple"), name = "Number of Adults") +
  scale_shape_manual(values = c("0" = 16, "1" = 17, "2" = 15, "3" = 18), name = "Number of Children") +
  labs(
    title = "Per-Person Price vs. Family Size by Family Number",
    x = "Family Size",
    y = "Per-Person Price (USD)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14),
    strip.text = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = 1:7)

# Save the plot
ggsave("family_person_price_plot.png", width = 8, height = 6)

View(results %>% filter(family_size==4, num_adult==1, num_children==3)  )
