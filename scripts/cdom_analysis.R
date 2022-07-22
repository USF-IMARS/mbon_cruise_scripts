library("ggplot2")
library("tibble")
library("tidyr")
library("readr")
library("purrr")
library("dplyr")
library("stringr")
library("forcats")
library("lubridate")
library("glue")
library("fs")
library("magrittr")
# library("broom") # optional

root <- rprojroot::find_rstudio_root_file() 

date_run <- "22MAY10"

df_cdom <-
  fs::dir_ls(paste0(root, "/data/raw/cdom/", date_run, "/"),
             regexp = "\\.SP") %>%
  map_dfr(., ~ read_tsv(.x, skip = 86, col_names = F), .id = "file") %>%
  mutate(file = gsub("\\.SP$", "", basename(file))  %>%
           str_to_lower) %>%
  rename(lambda = 2, absorb = 3)

file_match <-
  fs::dir_ls(paste0(root, "/data/metadata/"), regexp = regex(str_to_lower("10MAY2022"))) %>%
  readxl::read_xlsx(skip = 6, .name_repair = janitor::make_clean_names) %>%
  select(sample_id, comments) %>%
  # mutate(scan1 = str_split(sample_id," \\+ ", simplify = T)[,1])
  separate(sample_id,
           c("scan1", "scan2"),
           sep = " \\+ ",
           remove = F) %>%
  mutate(
    scan2 = str_c(substr(scan1, 1 , 5), scan2),
    cruise_id = case_when(
      str_detect(comments, "Milli-q|Nano")  ~ "blank",
      TRUE ~ str_split(comments, " ", simplify = T)[, 1]
    )
  ) %>%
  pivot_longer(
    cols = c(scan1, scan2), names_to = "scans", values_to = "file"
  )

cruise <- file_match %>% 
  select(cruise_id) %>%
  filter(!grepl("blank", cruise_id)) %$%
  sub("-.*","", cruise_id)[1]

# length of cuvette
L     <-  10
const <-  L * log(10)

# join 
df_cdom_mg <- left_join(df_cdom, file_match, by = "file") %>%
  relocate(sample_id:scans, .before = file) %>%
  group_by(sample_id, lambda) 


# extract blanks and scans
blanks <- 
  df_cdom_mg %>%
  filter(grepl("blank",cruise_id)) 

avg_blank <- blanks %>%
  group_by(lambda) %>%
  summarise(avg = mean(absorb),
            med = median(absorb),
            stdd = sd(absorb)
            ) %>%
  arrange(-lambda) %>%
  mutate(
    avg_l = avg * const,
    med_l = med * const,
    stdd_l = stdd * const
  )

scan1 <- df_cdom_mg %>%
  filter(!grepl("blank",cruise_id) & !grepl("2",scans)) %>%
  mutate(
    abs = absorb * const
  )

scan2 <- df_cdom_mg %>%
  filter(!grepl("blank",cruise_id) & !grepl("1",scans)) %>%
  mutate(
    abs = absorb * const
  )

# ---- smoothing ----
"10may005"

smthb <- avg_blank %$%
  zoo::rollmean(avg, k = 27, fill = "extend") %>%
  as_tibble() %>%
  mutate(
    lambda = avg_blank$lambda
  ) %>%
  right_join(avg_blank ) %>%
  relocate(value, .after = med_l)

smth1 <- scan1  %>%
filter(file == "10may005") %$%
zoo::rollmean(abs, k = 27, fill = "extend") %>%
as_tibble() %>%
mutate(
  file = scan1 %>%
    filter(file == "10may005") %$% file,
  lambda = scan1 %>%
    filter(file == "10may005") %$% lambda 
) %>%
right_join(scan1 %>%
             filter(file == "10may005") ) %>%
             relocate(value, .after = abs)

smth2 <- scan2  %>%
 filter(file == "10may006") %$%
 zoo::rollmean(abs, k = 27, fill = "extend") %>%
 as_tibble() %>%
 mutate(
   file = scan2 %>%
     filter(file == "10may006") %$% file,
   lambda = scan2 %>%
     filter(file == "10may006") %$% lambda 
 ) %>%
 right_join(scan2 %>%
              filter(file == "10may006") ) %>%
 relocate(value, .after = abs)


sta <- apply(cbind(smth1$value, smth2$value), 1, mean) - smthb$value %>%
  as_tibble() 

sta <- sta %>%
  mutate(
    lambda = smth1$lambda
  )


# ---- bias ----
bias <- sta %>%
  filter(lambda >= 680 & lambda <= 690) %>%
  summarise(avg = mean(value))

sta <- sta %>%
  mutate(
    test = value,
    # biased = value - bias
    value = value - bias[[1]]
  )

# ---- slope ----

slope <- lm(value ~ 1 + lambda, data = sta)
s <- slope$coefficients[2]

final <-  data.frame(lam = seq(800, 200, -1), est = NA)
for (i in 1:nrow(sta)) {
  print(i)
  final[i,2] <- sta$value[sta$lambda == 400] * exp(s * (final[i,1] - 400))
  
}

final


(ggplot() +
  geom_line(data = final, aes(x = lam, y = est)) + 
  geom_line(data = sta, aes(x = lambda, y = value), color = "blue") +
  geom_hline(yintercept = 0, color = "grey50", alpha = 0.5) +
  labs(
    title = paste(cruise, "- MBON"),
    x =  expression(Wavelength~(lambda~nm)),
    y = expression(a[CDOM]~(lambda)~(m^-1)) 
  ) +
  scale_x_continuous(expand=c(0,0)) +
  coord_cartesian( 
    xlim = c(400, 800),
    ylim = c(-0.05, 0.05)
    ) + 
  theme_bw()) # %>%
  # plotly::ggplotly()

(ggplot() +
    # geom_line(data = final, aes(x = lam, y = est)) + 
    geom_line(data = sta, aes(x = lambda, y = value), color = "blue") +
    scale_y_log10() +
    labs(
      title = paste(cruise, "- MBON"),
      x = expression(Wavelength~(lambda~nm)),
      y = expression(a[CDOM]~(lambda)~(m^-1)) 
    ) +
    scale_x_continuous(expand=c(0,0)) +
    coord_cartesian( 
      xlim = c(300, 800),
      ylim = c(0.0001,20)
    ) + 
    theme_bw()) #%>%
  # plotly::ggplotly()
