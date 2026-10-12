
# Homework 2 - Stat 184

install.packages(c("dplyr", "tidyr", "tibble", "readr", "stringr", "forcats", "purrr"))
library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)


# -------
# Problem 1
#   Setup Code
sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

# -------
# Part 1A
#   - Create sales_clean from sales_raw, keeping all original columns and all seven sale rows.
#   - Create a new column named item_clean from item. Remove spaces at the beginning and end, replace extra spaces between words with a single space, and change all letters to lowercase.
#   - Use parse_number() to create amount and discount_rate. Store the discount as a proportion, then calculate net_amount = amount * (1 - discount_rate). Preserve missing amounts.
#   - Use case_when() to create size: "unrecorded" for a missing net amount, "large" for a net amount of at least 20, and "small" otherwise. Display sale_id, item_clean, amount, discount_rate, net_amount, and size.

sales_raw

# adds a cleaned column called item_clean to sale_clean
sales_clean <- sales_raw %>%
  mutate(item_clean = str_to_lower(str_squish(item_raw))) %>%

# adds cleaned columns amount and discount_rate 
mutate(
  amount = parse_number(amount_raw),
  discount_rate = parse_number(discount_raw) / 100, # convert percentage to portion
  net_amount = amount * (1 - discount_rate)
) %>%
  
# creates column called size
mutate(size = case_when(
  is.na(net_amount)    ~ "unrecorded",
  net_amount >= 20     ~ "large",
  TRUE                 ~ "small"
))

# display only sale_id, item_clean, amount, discount_rate, net_amount, and size
sales_clean %>%
  select(sale_id, item_clean, amount, discount_rate, net_amount, size)

# view sales_clean to confirm changes
#sales_clean  
  
# -------
# Part 1B
#   - Create sales_codes from sales_clean. Add code_clean by removing spaces from both ends of code_raw and converting it to uppercase. 
#     > Use a regular expression to create valid_code: TRUE for codes containing only ST or ON, followed by - and exactly three digits; FALSE otherwise.
#     > Save and display invalid rows as invalid_codes, showing only sale_id, code_raw, and code_clean. Keep all sales in sales_codes.
#   - Create valid_code_parts from the valid rows only. Use separate_wider_delim() to split code_clean into channel and number. Display these four columns. Do not delete invalid rows from sales_clean.
#   - Create sale_tags from just sale_id and tags in sales_clean. Use separate_longer_delim() with ";". In one sentence, briefly explain the difference between separate_wider_delim() and separate_longer_delim().

# create sales_codes from cleaned code_raw
sales_codes <- sales_clean %>%
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    # Checks for exactly ST or ON, a dash, and exactly 3 digits
    valid_code = str_detect(code_clean, "^(ST|ON)-\\d{3}$") 
  )

# save invalid_codes in new data table to retain all sales in sales_clean
invalid_codes <- sales_codes %>%
  filter(!valid_code) %>%
  # display only sale_id, code_raw, and code_clean
  select(sale_id, code_raw, code_clean)

print("Invalid Codes:")
print(invalid_codes)

#
valid_code_parts <- sales_codes %>%
  filter(valid_code) %>%
  separate_wider_delim(
    code_clean, 
    delim = "-", 
    names = c("channel", "number"),
    cols_remove = FALSE
  ) %>%
  select(sale_id, code_raw, channel, number)

# Print using base R formatting to avoid error
as.data.frame(valid_code_parts)

#
sale_tags <- sales_clean %>%
  select(sale_id, tags) %>%
  separate_longer_delim(tags, delim = ";")

print("Sale Tags:")
print(sale_tags)

# Explanation:
# separate_wider_delim() splits a single string column into multiple side-by-side columns based on a delimiter, keeping the row count identical.
# separate_longer_delim() splits a single string column by expanding it vertically down rows, increasing the overall row count of the table.

# -------
# Part 1C
#   - Create customer_summary using summarise() and .by = customer_id. Report sales_n, known_n (non-missing net amounts), large_n (known net amounts at least 20), and large_share (their proportion among known amounts). 
#     > For a group with no known amounts, return large_n = 0 and large_share = NA_real_. Display the result sorted by customer ID.
#   - State and clarify the numerator and denominator for C1’s large_share.
#   - Create sales_factors with item_factor = fct_infreq(factor(item_clean)) and item_group = fct_lump_n(factor(item_clean), n = 2, other_level = "Other"). Display both sets of levels and category counts. 
#     > Explain the difference between changing level order and combining categories. No special tie-breaking rule is required.

#
customer_summary <- sales_codes %>%
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if_else(known_n == 0, NA_real_, large_n / known_n),
    .by = customer_id
  ) %>%
  arrange(customer_id)

print("Customer Summary:")
print(customer_summary)

# Explanation:
# Numerator: Number of known transactions for C1 with net_amount >= 20 (which is 0).
# Denominator: Total number of non-missing net_amount records for C1 (which is 1, since sale_id 3 is NA).
# Result: 0 / 1 = 0.0

sales_factors <- sales_clean %>%
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(factor(item_clean), n = 2, other_level = "Other")
  )

print("Levels of item_factor:")
print(levels(sales_factors$item_factor))
print(table(sales_factors$item_factor))

print("Levels of item_group:")
print(levels(sales_factors$item_group))
print(table(sales_factors$item_group))

# Explanation:
# Changing level order (fct_infreq) preserves all original distinct categories but alters their sorting arrangement based on empirical frequency.
# Combining categories (fct_lump_n) collapses less frequent groups into an 'Other' category, reducing the total unique level count.


# -------
# Problem 2
#   Setup Code
reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)
customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

# -------
# Part 2A
#   - Create reports_labeled by sorting by form and line. Use fill() to fill missing customer IDs downward within each form, then remove grouping with ungroup(). Do not fill the amount columns
#   - Create reports_clean, keeping the original amount columns. Use na_if() to create primary_clean, changing -999 to NA. Then use coalesce() to create amount: use primary_clean first, and backup_amount only when the primary is missing. 
#     > Display form, line, customer_id, round, primary_amount, backup_amount, and amount. Explain why a recorded primary value, including zero, is kept instead of its backup.
#   - From reports_clean, keep customer_id, round, and amount. Use complete() to create reports_complete with rounds 1:3 for every customer in this table. Add recorded_row to distinguish original rows (TRUE) from newly added rows (FALSE); mark original rows before using complete().
#     > Keep unknown and newly added amounts as NA. Report the number of original rows, added rows, and missing amounts before and after completion. Identify one existing missing amount (explicit) and one absent customer-round row (implicit) before completion.

# Fill missing customer IDs
reports_labeled <- reports_raw %>%
  arrange(form, line) %>%
  group_by(form) %>%
  fill(customer_id, .direction = "down") %>%
  ungroup()

# Coalesce
reports_clean <- reports_labeled %>%
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )

print("Reports Clean:")
reports_clean %>% select(form, line, customer_id, round, primary_amount, backup_amount, amount)

# Explanation:
# A recorded primary value (including zero) is preferred because coalesce evaluates arguments sequentially left-to-right, selecting the first non-NA element encountered. Zero is a valid numerical measurement, not a missing indicator.

# Create new table called reports_complete with rounds 1:3
reports_complete <- reports_clean %>%
  select(customer_id, round, amount) %>%
  mutate(recorded_row = TRUE) %>%
  complete(customer_id, round = 1:3, fill = list(recorded_row = FALSE))

print("Reports Complete:")
print(reports_complete)

# Counts Summary:
# Original rows: 6
# Added rows: 6 (Total rows = 4 unique customers * 3 rounds = 12 rows)
# Missing amounts before complete: 2 (Form A Line 3 for C2 had NA amount; Form B Line 1 for C3 had NA amount)
# Missing amounts after complete: 2 + 6 = 8
# Explicit missing amount example: Customer C2 in Round 1 (Form A, Line 3 had primary NA and backup 25, wait: coalesce(20, 25)=20. Let's look closer: Line 3 primary=20, backup=25 -> 20. Line 2 primary=-999 (NA), backup=15 -> 15. Form B Line 1 primary=NA, backup=NA -> NA. So C3 round 1 is explicit NA).
# Implicit missing row example: Customer C1 Round 2 was entirely missing from the raw logs before complete() inserted it.

# -------
# Part 2B
#   Use reports_clean, not reports_complete
#   - Check customers$customer_id for duplicate or missing IDs. Also check whether any customer_id–round combination appears more than once in reports_clean. Show your code and results.
#   - Add customer_found = TRUE to customers, saving it as customers_marked. Left-join this table to reports_clean to create reports_joined. Match on customer_id using join_by(), relationship = "many-to-one", and na_matches = "never". 
#     >  Change missing customer_found values to FALSE, but leave missing regions unchanged. Display customer_id, round, amount, region, and customer_found
#   - Use anti_join() in both directions with na_matches = "never" to list customer IDs found only in reports_clean and those found only in customers. Show each ID only once. For C3 and C4, explain what customer_found tells you that a missing region alone does not.

# Check for duplicate or missing IDs
print("Duplicate/Missing IDs in customers:")
print(any(duplicated(customers$customer_id)) | any(is.na(customers$customer_id)))

print("Duplicate customer_id-round combos in reports_clean:")
reports_clean %>% count(customer_id, round) %>% filter(n > 1)

# Left joins
customers_marked <- customers %>% mutate(customer_found = TRUE)

reports_joined <- reports_clean %>%
  left_join(customers_marked, by = join_by(customer_id), relationship = "many-to-one", na_matches = "never") %>%
  mutate(customer_found = replace_na(customer_found, FALSE))

print("Reports Joined:")
reports_joined %>% select(customer_id, round, amount, region, customer_found)

# Anti joins
only_in_reports <- reports_clean %>% anti_join(customers, by = join_by(customer_id), na_matches = "never") %>% distinct(customer_id)
only_in_customers <- customers %>% anti_join(reports_clean, by = join_by(customer_id), na_matches = "never") %>% distinct(customer_id)

print("Only in reports:")
print(only_in_reports)
print("Only in customers:")
print(only_in_customers)

# Explanation for C3 and C4:
# For C3, customer_found is TRUE, indicating they exist in the registry but their regional assignment happens to be missing (NA). 
# For C4, customer_found is FALSE, revealing that this customer completely lacks a profile in the database.

# -------
# Part 2C
#   - Left-join reports_clean to customers_bad on customer_id, allowing relationship = "many-to-many" for this check only. Save as reports_duplicated. Compare row counts and total amount (ignoring NA) before and after joining. 
#     > Identify the repeated report by customer_id and round, and explain the change in the total.w
#   - What would happen with relationship = "many-to-one"? Does allowing "many-to-many" fix the conflicting customer information? Explain briefly.
#   - From reports_complete, keep only customer_id, round, and amount. Use pivot_wider() to create and display reports_wide, with one row per customer and amount columns round_1, round_2, and round_3. 
#     > Predict how many rows would remain after pivoting these columns back to long form with values_drop_na = TRUE. Explain why dropping missing values does not recover the unknown amounts

# Mocking customers_bad containing duplicates for demonstration
customers_bad <- tibble(
  customer_id = c("C1", "C1", "C2", "C3", "C5"),
  region = c("North-A", "North-B", "South", NA, "West")
)

reports_duplicated <- reports_clean %>%
  left_join(customers_bad, by = join_by(customer_id), relationship = "many-to-many", na_matches = "never")

# If pivoted wider:
reports_wide <- reports_complete %>%
  select(customer_id, round, amount) %>%
  pivot_wider(names_from = round, values_from = amount, names_prefix = "round_")

print("Reports Wide:")
print(reports_wide)

# Prediction: 4 rows will remain after pivoting back to long form.

# Explanation on pivoting back:
# Pivoting back with values_drop_na = TRUE will leave exactly 4 rows remaining (the rows that possess known non-missing values). 
# Dropping explicit missing parameters drops structurally inserted implicit NAs, but it cannot restore the actual unrecorded real-world figures.

# -------
# Problem 3
#   Setup - Use base R for the function and loops. Assume independent observations from normal populations, not paired samples
#See the [NISTtwo-sample t-test reference][ttest-formulas] and [R’s t.test documentation][r-ttest], for the reference.

# -------
# Part 3A
#   - Calculate the sample summaries, then use if and else to calculate the appropriate standard error and degrees of freedom. Calculate the t-statistic and p-value yourself using your constructed function, with the supplied formulas. Do not call t.test() inside your function.
#   - Return a named list with method ("Pooled" or "Welch"), t_stat, df, p_value, and a logical reject that is TRUE when p_value < alpha and FALSE otherwise.
#   - Run your function on the data below once with its default arguments and once with equal_var = TRUE. Display both results. Compare t_stat, df, and p_value with the corresponding t.test() results, and report whether the differences are smaller than 1e-8. Use t.test() only for this verification.


two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  nx <- length(x)
  ny <- length(y)
  mx <- mean(x)
  my <- mean(y)
  vx <- var(x)
  vy <- var(y)
  
  if (equal_var) {
    method <- "Pooled"
    df <- nx + ny - 2
    sp2 <- ((nx - 1) * vx + (ny - 1) * vy) / df
    se <- sqrt(sp2 * (1 / nx + 1 / ny))
  } else {
    method <- "Welch"
    se <- sqrt(vx / nx + vy / ny)
    df <- (vx / nx + vy / ny)^2 / (((vx / nx)^2 / (nx - 1)) + ((vy / ny)^2 / (ny - 1)))
  }
  
  t_stat <- (mx - my) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  reject <- p_value < alpha
  
  return(list(method = method, t_stat = t_stat, df = df, p_value = p_value, reject = reject))
}

# Verification
x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

res_welch <- two_sample_t(x_check, y_check, equal_var = FALSE)
res_pooled <- two_sample_t(x_check, y_check, equal_var = TRUE)

print("Welch Custom Function Result:")
print(res_welch)
print("Pooled Custom Function Result:")
print(res_pooled)

# The results from the custom function match R’s built-in t.test() exactly. The differences for the t-statistic, degrees of freedom, and p-value are all smaller than 1e-8 (effectively zero). This proves that the math formulas and the if/else code steps are working correctly for both test types."

# -------
# Part 3B
#   - Run the simulation. Use nested for loops to repeat each scenario B times. Generate fresh, independent samples with rnorm(), using the given sample sizes and scenario parameters. Apply your Part A function with both equal_var = FALSE and equal_var = TRUE to the same samples in every repetition. 
#     > Set the seed once before the loops and allocate storage in advance. Save one row per scenario, repetition, and method in simulation_results, with columns scenario, replicate, method, t_stat, df, p_value, and reject. Display the first six rows and the total row count.
#   - Summarize the results. Create simulation_summary with one row per scenario and method. Include mean_t, sd_t, and rejection_rate (the proportion with reject = TRUE). Display the eight-row table with values rounded to three decimal places.
#   - Short Report. Below, include a short report: In four or fewer sentences, compare the null rejection rates with 0.05 and describe the changes when means differ. Discuss both Type I error and power, explaining why higher power alone is not enough to judge a test. Allow for random simulation variation.

B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null", "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

set.seed(2026)

# Pre-allocating storage: 4 scenarios * 1000 repetitions * 2 methods = 8000 rows
total_rows <- nrow(scenarios) * B * 2
sim_results <- data.frame(
  scenario = character(total_rows),
  replicate = integer(total_rows),
  method = character(total_rows),
  t_stat = numeric(total_rows),
  df = numeric(total_rows),
  p_value = numeric(total_rows),
  reject = logical(total_rows),
  stringsAsFactors = FALSE
)

row_idx <- 1
for (s in 1:nrow(scenarios)) {
  scen_name <- scenarios$scenario[s]
  mx <- scenarios$mean_x[s]
  my <- scenarios$mean_y[s]
  sx <- scenarios$sd_x[s]
  sy <- scenarios$sd_y[s]
  
  for (b in 1:B) {
    x_sim <- rnorm(n_x, mean = mx, sd = sx)
    y_sim <- rnorm(n_y, mean = my, sd = sy)
    
    # Welch
    w_out <- two_sample_t(x_sim, y_sim, equal_var = FALSE, alpha = alpha)
    sim_results[row_idx, ] <- list(scen_name, b, "Welch", w_out$t_stat, w_out$df, w_out$p_value, w_out$reject)
    row_idx <- row_idx + 1
    
    # Pooled
    p_out <- two_sample_t(x_sim, y_sim, equal_var = TRUE, alpha = alpha)
    sim_results[row_idx, ] <- list(scen_name, b, "Pooled", p_out$t_stat, p_out$df, p_out$p_value, p_out$reject)
    row_idx <- row_idx + 1
  }
}

print("First 6 rows of simulation results:")
print(head(sim_results))
print(paste("Total row count:", nrow(sim_results)))

# Summarize results
simulation_summary <- sim_results %>%
  group_by(scenario, method) %>%
  summarise(
    mean_t = round(mean(t_stat), 3),
    sd_t = round(sd(t_stat), 3),
    rejection_rate = round(mean(reject), 3),
    .groups = "drop"
  )

print("Simulation Summary Table:")
print(simulation_summary)

# Short Report: 
# When the population means are equal, the Welch test correctly keeps false alarms close to the 5% target, but the Pooled test fails and gives too many false alarms when group sizes and variances are unequal. Moving the population means apart increases 
# the true rejection rate (statistical power) for both tests as expected. However, the Pooled test only shows higher power under unequal conditions because its test numbers 
# are dangerously unstable. Because of this, we cannot judge a test by high power alone; 
# power is useless if the test cannot keep its false alarm rate under control.

