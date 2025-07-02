#SAPE-Factors with and without sociodemographic factors####
#Only With#####
# Load required libraries
library(ggplot2)
library(dplyr)

# Step 1: Create the CFA-only data frame
data <- data.frame(
  Factor = factor(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"),
                  levels = rev(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"))),
  Delta_b = c(1.925, -0.027, 0.677, 0.260, 0.463, -0.856)
)

# Step 2: Assign color based on ±0.156 threshold
threshold <- 0.156
data <- data %>%
  mutate(
    Color = ifelse(abs(Delta_b) <= threshold, "gray", "With")
  )

# Step 3: Plot
ggplot(data, aes(x = Delta_b, y = Factor, fill = Color)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(
    values = c("With" = "#4C72B0", "gray" = "gray70"),
    labels = c("With" = "Mit", "gray" = "nicht signifikant"),
    name = "Model"
  ) +
  geom_vline(xintercept = c(-threshold, threshold), linetype = "dotted", color = "black") +
  labs(
    title = "Gruppendifferenzen in der geschätzen Ausprägung der SAPE-Faktoren (Keine Kontrolle)",
    x = "Differenz Japan zu Deutschland (SD)",
    y = "SAPE-Faktoren"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

#With and Without#####
# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Step 1: Create the data frame
data <- data.frame(
  Factor = factor(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"),
                  levels = rev(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"))),
  CFA = c(1.925, -0.027, 0.677, 0.260, 0.463, -0.856),
  SEM = c(1.454, -0.145, 0.488, 0.152, 0.321, -0.747)
)

# Step 2: Reshape to long format
data_long <- pivot_longer(data, cols = c(CFA, SEM), names_to = "Model", values_to = "Delta_b")

# Step 3: Apply conditional coloring (gray if between ±0.156)
threshold <- 0.156
data_long <- data_long %>%
  mutate(
    Color = ifelse(abs(Delta_b) <= threshold, "gray", Model),
    ModelLabel = recode(Model, "CFA" = "With", "SEM" = "Without")  # For legend labeling
  )

# Step 4: Plot with custom legend
ggplot(data_long, aes(x = Delta_b, y = Factor, fill = Color)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(
    values = c("CFA" = "#4C72B0", "SEM" = "#55A868", "gray" = "gray70"),
    labels = c("CFA" = "Mit", "SEM" = "Ohne", "gray" = "nicht signifikant"),
    name = "Model"
  ) +
  geom_vline(xintercept = c(-threshold, threshold), linetype = "dotted", color = "black") +
  labs(
    title = "Gruppendifferenzen in der Ausprägung der SAPE-Faktoren mit und ohne Kontrolle sozidemographischer Faktoren",
    x = "Differenz Japan zu Deutschland (SD)",
    y = "SAPE-Faktoren"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")


#Experience####
##EXP1#####
library(ggplot2)
library(dplyr)
library(tidyr)

# Data
exp1_data <- data.frame(
  Factor = factor(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"),
                  levels = rev(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"))),
  GER_value = c(-0.306, 0.019, -0.369, -0.333, -0.142, -0.060),
  GER_sig   = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE),
  JP_value  = c(-0.231, 0.015, -0.115, -0.076, -0.045, 0.015),
  JP_sig    = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
)

# Reshape to long format
exp1_long <- exp1_data %>%
  pivot_longer(
    cols = -Factor,
    names_to = c("Group", "Type"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Type,
    values_from = Value
  )

# Define color based on significance
exp1_long <- exp1_long %>%
  mutate(Color = ifelse(sig,
                        ifelse(Group == "GER", "#1f77b4", "#ff7f0e"),
                        "gray70"))

# Plot with separated bars
ggplot(exp1_long, aes(x = Factor, y = value, fill = Color)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           width = 0.4,
           aes(group = Group)) +
  coord_flip() +
  labs(
    title = "Differenz in den SAPE-Faktoren von Leuten mit oder ohne Bekantschafft einer PWE",
    x = "SAPE-Faktoren",
    y = "Standardisierte Differenz 'Kennt PWE' zu 'Kennt keine PWE'(SD)"
  ) +
  scale_fill_identity(
    guide = "legend",
    labels = c("Deutsch", "Japanisch", "nicht signifikant"),
    breaks = c("#1f77b4", "#ff7f0e", "gray70"),
    name = "Gruppe"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )




##EXP2#####
library(ggplot2)
library(dplyr)
library(tidyr)

# Data for Exp2
exp2_data <- data.frame(
  Factor = factor(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"),
                  levels = rev(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"))),
  GER_value = c(0.024, 0.013, 0.084, 0.088, 0.115, 0.206),
  GER_sig   = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
  JP_value  = c(0.078, -0.022, -0.058, 0.113, 0.080, 0.000),
  JP_sig    = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
)

# Reshape to long format
exp2_long <- exp2_data %>%
  pivot_longer(
    cols = -Factor,
    names_to = c("Group", "Type"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Type,
    values_from = Value
  )

# Define fill color based on significance
exp2_long <- exp2_long %>%
  mutate(Color = ifelse(sig,
                        ifelse(Group == "GER", "#1f77b4", "#ff7f0e"),
                        "gray70"))

# Plot for Exp2
ggplot(exp2_long, aes(x = Factor, y = value, fill = Color)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           width = 0.4,
           aes(group = Group)) +
  coord_flip() +
  labs(
    title = "Differenz in den SAPE-Faktoren von Zeugen eines Anfalls zu Menschen ohne diese Erfahrung",
    x = "SAPE-Faktoren",
    y = "Standardisierte Differenz 'zeugen eines Anfalls' zu 'Kein Zeuge eines Anfalls' (SD)"
  ) +
  scale_fill_identity(
    guide = "legend",
    labels = c("Deutsch", "Japanisch", "nicht signifikant"),
    breaks = c("#1f77b4", "#ff7f0e", "gray70"),
    name = "Gruppe"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )


##EXP3#####
library(ggplot2)
library(dplyr)
library(tidyr)

# Data for Exp3
exp3_data <- data.frame(
  Factor = factor(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"),
                  levels = rev(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"))),
  GER_value = c(-0.170, -0.242, -0.320, -0.462, -0.097, -0.057),
  GER_sig   = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
  JP_value  = c(-0.333, 0.049, -0.061, -0.243, -0.057, -0.286),
  JP_sig    = c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
)

# Reshape to long format
exp3_long <- exp3_data %>%
  pivot_longer(
    cols = -Factor,
    names_to = c("Group", "Type"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Type,
    values_from = Value
  )

# Define color based on significance
exp3_long <- exp3_long %>%
  mutate(Color = ifelse(sig,
                        ifelse(Group == "GER", "#1f77b4", "#ff7f0e"),
                        "gray70"))

# Plot for Exp3
ggplot(exp3_long, aes(x = Factor, y = value, fill = Color)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           width = 0.4,
           aes(group = Group)) +
  coord_flip() +
  labs(
    title = "Differenz in den SAPE-Faktoren von Mensche mit oder ohne Kenntniss über richtiges Verhalten während eines Anfalls",
    x = "SAPE-Faktoren",
    y = "Standardisierte Differenz 'Mit Kenntniss' zu 'Ohne Kenntniss'(SD)"
  ) +
  scale_fill_identity(
    guide = "legend",
    labels = c("Deutsch", "Japanisch", "nicht signifikant"),
    breaks = c("#1f77b4", "#ff7f0e", "gray70"),
    name = "Gruppe"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )
##Exp4#####
library(ggplot2)
library(dplyr)
library(tidyr)

# Data for Exp4: yes (ref: no/I don’t know)
exp4_data <- data.frame(
  Factor = factor(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"),
                  levels = rev(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"))),
  GER_value = c(-0.103, 0.092, -0.193, -0.166, 0.194, 0.074),
  GER_sig   = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE),
  JP_value  = c(-0.337, -0.033, -0.334, -0.179, 0.015, -0.312),
  JP_sig    = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
)

# Reshape to long format
exp4_long <- exp4_data %>%
  pivot_longer(
    cols = -Factor,
    names_to = c("Group", "Type"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Type,
    values_from = Value
  )

# Define fill color based on significance
exp4_long <- exp4_long %>%
  mutate(Color = ifelse(sig,
                        ifelse(Group == "GER", "#1f77b4", "#ff7f0e"),
                        "gray70"))

# Plot
ggplot(exp4_long, aes(x = Factor, y = value, fill = Color)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           width = 0.4,
           aes(group = Group)) +
  coord_flip() +
  labs(
    title = "Differenz in den SAPE-Faktoren zwischen Personen mit oder ohne Auffassung Epilepsie sei erfolgreich Behandelbar",
    x = "SAPE-Faktoren",
    y = "Standardisierte Differenz 'Stimmt zu' zu 'Stimmt nicht zu'(SD)"
  ) +
  scale_fill_identity(
    guide = "legend",
    labels = c("Deutsch", "Japanisch", "nicht signifikant"),
    breaks = c("#1f77b4", "#ff7f0e", "gray70"),
    name = "Gruppe"
  )

#Knowledge####
##Know1#####
library(ggplot2)
library(dplyr)
library(tidyr)

# Data for Knowledge: Symptoms
symptom_data <- data.frame(
  Factor = factor(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"),
                  levels = rev(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"))),
  GER_value = c(-0.265, -0.626, 0.563, 0.167, -0.261, 0.352),
  GER_sig   = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE),
  JP_value  = c(-0.184, -0.476, 0.400, 0.355, -0.744, 0.199),
  JP_sig    = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)
)

# Reshape to long format
symptom_long <- symptom_data %>%
  pivot_longer(
    cols = -Factor,
    names_to = c("Group", "Type"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Type,
    values_from = Value
  )

# Define fill color based on significance
symptom_long <- symptom_long %>%
  mutate(Color = ifelse(sig,
                        ifelse(Group == "GER", "#1f77b4", "#ff7f0e"),
                        "gray70"))

# Plot
ggplot(symptom_long, aes(x = Factor, y = value, fill = Color)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           width = 0.4,
           aes(group = Group)) +
  coord_flip() +
  labs(
    title = "Differenz in den SAPE-Faktoren zwischen Menschen mit oder ohne Wissen über die Ursachen von Epilepsie",
    x = "SAPE-Faktoren",
    y = "Standardisierte Differenz 'Mit Wissen' zu 'Ohne Wissen'(SD)"
  ) +
  scale_fill_identity(
    guide = "legend",
    labels = c("Deutsch", "Japanisch", "nicht signifikant"),
    breaks = c("#1f77b4", "#ff7f0e", "gray70"),
    name = "Gruppe"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )


##Know2#####
library(ggplot2)
library(dplyr)
library(tidyr)

# Data for Knowledge: Causes
causes_data <- data.frame(
  Factor = factor(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"),
                  levels = rev(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"))),
  GER_value = c(0.183, 0.044, 0.245, 0.189, -0.086, 0.359),
  GER_sig   = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
  JP_value  = c(-0.063, 0.246, 0.119, -0.272, -0.406, 0.285),
  JP_sig    = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
)

# Reshape to long format
causes_long <- causes_data %>%
  pivot_longer(
    cols = -Factor,
    names_to = c("Group", "Type"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Type,
    values_from = Value
  )

# Define fill color based on significance
causes_long <- causes_long %>%
  mutate(Color = ifelse(sig,
                        ifelse(Group == "GER", "#1f77b4", "#ff7f0e"),
                        "gray70"))

# Plot
ggplot(causes_long, aes(x = Factor, y = value, fill = Color)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           width = 0.4,
           aes(group = Group)) +
  coord_flip() +
  labs(
    title = "Differenz in den SAPE-Faktoren zwischen Menschen mit oder ohne Wissen über die Symptome von Epilepsie",
    x = "SAPE-Faktoren",
    y = "Standardisierte Differenz 'Mit Wissen' zu 'Ohne Wissen'(SD)"
  ) +
  scale_fill_identity(
    guide = "legend",
    labels = c("Deutsch", "Japanisch", "nicht signifikant"),
    breaks = c("#1f77b4", "#ff7f0e", "gray70"),
    name = "Gruppe"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )

##Know3#####
library(ggplot2)
library(dplyr)
library(tidyr)

# Data for Knowledge: Treatment
treatment_data <- data.frame(
  Factor = factor(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"),
                  levels = rev(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"))),
  GER_value = c(-0.127, -0.180, -0.224, 0.007, -0.381, 0.298),
  GER_sig   = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
  JP_value  = c(0.151, 0.304, -0.039, 0.094, 0.280, 0.191),
  JP_sig    = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
)

# Reshape to long format
treatment_long <- treatment_data %>%
  pivot_longer(
    cols = -Factor,
    names_to = c("Group", "Type"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Type,
    values_from = Value
  )

# Define fill color based on significance
treatment_long <- treatment_long %>%
  mutate(Color = ifelse(sig,
                        ifelse(Group == "GER", "#1f77b4", "#ff7f0e"),
                        "gray70"))

# Plot
ggplot(treatment_long, aes(x = Factor, y = value, fill = Color)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           width = 0.4,
           aes(group = Group)) +
  coord_flip() +
  labs(
    title = "Differenz in den SAPE-Faktoren zwischen Menschen mit oder ohne Wissen über die Behandlung von Epilepsie",
    x = "SAPE-Faktoren",
    y = "Standardisierte Differenz 'Mit Wissen' zu 'Ohne Wissen'(SD)"
  ) +
  scale_fill_identity(
    guide = "legend",
    labels = c("Deutsch", "Japanisch", "nicht signifikant"),
    breaks = c("#1f77b4", "#ff7f0e", "gray70"),
    name = "Gruppe"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )

#Gender####
library(ggplot2)
library(dplyr)
library(tidyr)

# Data for Gender: female (ref: male)
gender_data <- data.frame(
  Factor = factor(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"),
                  levels = rev(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"))),
  GER_value = c(-0.309, -0.282, -0.208, -0.061, -0.346, 0.155),
  GER_sig   = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE),
  JP_value  = c(-0.103, -0.226, -0.172, -0.201, -0.364, -0.337),
  JP_sig    = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
)

# Reshape to long format
gender_long <- gender_data %>%
  pivot_longer(
    cols = -Factor,
    names_to = c("Group", "Type"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Type,
    values_from = Value
  )

# Define fill color based on significance
gender_long <- gender_long %>%
  mutate(Color = ifelse(sig,
                        ifelse(Group == "GER", "#1f77b4", "#ff7f0e"),
                        "gray70"))

# Plot
ggplot(gender_long, aes(x = Factor, y = value, fill = Color)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           width = 0.4,
           aes(group = Group)) +
  coord_flip() +
  labs(
    title = "Differenz in den SAPE-Faktoren zwischen Männern und Frauen ",
    x = "SAPE-Faktoren",
    y = "Standardisierte Differenz von Frauen zu Männern (SD)"
  ) +
  scale_fill_identity(
    guide = "legend",
    labels = c("Deutsch", "Japanisch", "nicht signifikant"),
    breaks = c("#1f77b4", "#ff7f0e", "gray70"),
    name = "Gruppe"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )


#Age <25 (ref >65)####
library(ggplot2)
library(dplyr)
library(tidyr)

# Data for Age <25 (ref >65)
age_data <- data.frame(
  Factor = factor(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"),
                  levels = rev(c("Soziale Distanz", "Stereotype", "Besorgnis", "Angst", "Ärger", "Mitleid"))),
  GER_value = c(-0.131, -0.097, -0.240, -0.229, 0.151, -0.391),
  GER_sig   = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
  JP_value  = c(-0.571, 0.577, 0.136, -0.036, 0.245, -0.093),
  JP_sig    = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
)

# Reshape to long format
age_long <- age_data %>%
  pivot_longer(
    cols = -Factor,
    names_to = c("Group", "Type"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Type,
    values_from = Value
  )

# Define fill color based on significance
age_long <- age_long %>%
  mutate(Color = ifelse(sig,
                        ifelse(Group == "GER", "#1f77b4", "#ff7f0e"),
                        "gray70"))

# Plot
ggplot(age_long, aes(x = Factor, y = value, fill = Color)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           width = 0.4,
           aes(group = Group)) +
  coord_flip() +
  labs(
    title = "Differenz in den SAPE-Faktoren zwischen Menschen jünger als 25 Jahre und Menschen älter als 65 Jahre ",
    x = "SAPE-Faktoren",
    y = "Standardisierte Differenz von Menschen <25 zu Menschen >65 (SD)"
  ) +
  scale_fill_identity(
    guide = "legend",
    labels = c("Deutsch", "Japanisch", "nicht signifikant"),
    breaks = c("#1f77b4", "#ff7f0e", "gray70"),
    name = "Gruppe"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )
