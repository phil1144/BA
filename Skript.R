install.packages(c("tidyverse","car","lmtest","sandwich","psych","broom","interactions"))

library(tidyverse)   # Datenmanipulation
library(car)         # Voraussetzungsprüfung (VIF, Levene etc.)
library(lmtest)      # Heteroskedastizitätstests (Breusch-Pagan)
library(sandwich)    # heteroskedastizitätskonsistente (robuste) SE
library(psych)        # deskriptive Statistiken, Cronbachs Alpha
library(broom)         # aufgeräumte Modell-Outputs
#library(interactions) # optional, falls installiert (siehe Hinweis oben)

# 1) DATEN EINLESEN -------------------------------------------------------
# Pfad ggf. anpassen
## readr::read_csv() erkennt das UTF-8-BOM automatisch und liest
## Leerstrings/"N/A" korrekt als fehlende Werte ein.
raw <- readr::read_csv("results.csv",
                       locale = readr::locale(encoding = "UTF-8"),
                       na = c("", "NA", "N/A"),
                       show_col_types = FALSE)
raw <- as.data.frame(raw, stringsAsFactors = FALSE)

nrow(raw)   # sollte 142 sein
ncol(raw)   # sollte 48 sein

# 2) SPALTEN UMBENENNEN (nach Position, da Originalnamen sehr lang sind) --
# Reihenfolge der Spalten im Export: siehe Kommentare
names(raw)[1:10] <- c("id", "datum", "letzte_seite", "sprache", "randstart",
                      "geschlecht_raw", "alter_raw", "psychologie_raw",
                      "beziehung_raw", "staatsbuerger_raw")

# BFI-2 Items (Spalten 10-33), Variablennamen kodieren: Domäne_Facette_Nr_[R]
bfi_names <- c(
  "C_org_1_R",   #  11   Ich bin eher unordentlich.
  "O_aes_1_R",   #  12   Ich bin nicht sonderlich kunstinteressiert.
  "C_prod_1_R",  #  13   Ich bin bequem, neige zu Faulheit.
  "O_cur_1",     #  14   Ich bin vielseitig interessiert.
  "C_resp_1",    #  15   Ich bin stetig, beständig.
  "O_crea_1",    #  16   Ich bin erfinderisch, mir fallen raffinierte Lösungen ein.
  "C_org_2",     #  17   Ich bin systematisch, halte meine Sachen in Ordnung.
  "O_aes_2",     #  18   Ich kann mich für Kunst, Musik und Literatur begeistern.
  "C_prod_2_R",  #  19   Ich neige dazu, Aufgaben vor mir herzuschieben.
  "O_cur_2_R",   #  20   Ich meide philosophische Diskussionen.
  "C_resp_2_R",  #  21   Ich bin manchmal ziemlich nachlässig.
  "O_crea_2_R",  #  22   Ich bin nicht besonders einfallsreich.
  "C_org_3",     #  23   Ich mag es sauber und aufgeräumt.
  "O_aes_3",     #  24   Ich weiß Kunst und Schönheit zu schätzen.
  "C_prod_3",    #  25   Ich bin effizient, erledige Dinge schnell.
  "O_cur_3",     #  26   Es macht mir Spaß, gründlich über komplexe Dinge nachzudenken...
  "C_resp_3",    #  27   Ich bin verlässlich, auf mich kann man zählen.
  "O_crea_3_R",  #  28   Ich bin nicht sonderlich fantasievoll.
  "C_org_4_R",   #  29   Ich bin eher der chaotische Typ, mache selten sauber.
  "O_aes_4_R",   #  30   Ich finde Gedichte und Theaterstücke langweilig.
  "C_prod_4",    #  31   Ich bleibe an einer Aufgabe dran, bis sie erledigt ist.
  "O_cur_4_R",   #  32   Mich interessieren abstrakte Überlegungen wenig.
  "C_resp_4_R",  #  33   Manchmal verhalte ich mich verantwortungslos, leichtsinnig.
  "O_crea_4"     #  34   Ich bin originell, entwickle neue Ideen.
)
names(raw)[11:34] <- bfi_names

# DASS-21 Items (Spalten 34-47): Subskalen Stress (S) und Depression (D)
# Reihenfolge/Zuordnung entspricht der Standard-DASS-21-Struktur
dass_names <- c(
  "S1",  #  35   Ich fand es schwer, mich zu beruhigen.
  "D1",  #  36   Ich konnte überhaupt keine positiven Gefühle mehr erleben.
  "D2",  #  37   Es fiel mir schwer, mich dazu aufzuraffen, Dinge zu erledigen.
  "S2",  #  38   Ich tendierte dazu, auf Situationen überzureagieren.
  "S3",  #  39   Ich fand alles anstrengend.
  "D3",  #  40   Ich hatte das Gefühl, dass ich mich auf nichts mehr freuen konnte.
  "S4",  #  41   Ich bemerkte, dass ich mich schnell aufregte.
  "S5",  #  42   Ich fand es schwierig, mich zu entspannen.
  "D4",  #  43   Ich fühlte mich niedergeschlagen und traurig.
  "S6",  #  44   Ich reagierte ungehalten auf alles ...
  "D5",  #  45   Ich war nicht in der Lage, mich für irgendetwas zu begeistern.
  "D6",  #  46   Ich fühlte mich als Person nicht viel wert.
  "S7",  #  47   Ich fand mich ziemlich empfindlich.
  "D7"   #  48   Ich empfand das Leben als sinnlos.
)
names(raw)[35:48] <- dass_names

df <- raw  # ab hier mit sprechenden Namen weiterarbeiten

# 3) DESKRIPTIVE PRÜFUNG FEHLENDER WERTE ---------------------------------
table(df$letzte_seite, useNA = "always")   # 3 = vollständig bis zum Ende

bfi_cols  <- c(grep("^O_", names(df), value = TRUE),
               grep("^C_", names(df), value = TRUE))
dass_cols <- c(grep("^S[0-9]$", names(df), value = TRUE),
               grep("^D[0-9]$", names(df), value = TRUE))

miss_pattern <- rowSums(is.na(df[, c(bfi_cols, dass_cols)]))
table(miss_pattern)
# -> 0 fehlende Werte: vollständige Fälle
# -> hohe Anzahl (z.B. 37/38): komplette Abbrecher bei BFI/DASS-Block

# 4) DATENBEREINIGUNG -----------------------------------------------------
# Plausibilitätsprüfung Alter (z.B. Jahreszahl statt Alter eingetragen)
df$alter_raw <- as.numeric(df$alter_raw)
df$alter <- ifelse(df$alter_raw < 15 | df$alter_raw > 90, NA, df$alter_raw)
sum(is.na(df$alter)) - sum(is.na(df$alter_raw))  # zusätzlich auf NA gesetzte Fälle -> prüfen!

# 5) LIKERT-TEXTLABELS IN ZAHLEN UMKODIEREN --------------------------------
bfi_levels <- c("stimme überhaupt nicht zu" = 1,
                "stimme eher nicht zu"      = 2,
                "teils, teils"              = 3,
                "stimme eher zu"            = 4,
                "stimme voll und ganz zu"   = 5)

dass_levels <- c("Trifft gar nicht zu"     = 0,
                 "Trifft manchmal zu"     = 1,
                 "Trifft oft zu"          = 2,
                 "Trifft fast immer zu"   = 3)

recode_likert <- function(x, levels_map) {
  unname(levels_map[x])
}

df[bfi_cols]  <- lapply(df[bfi_cols],  recode_likert, levels_map = bfi_levels)
df[dass_cols] <- lapply(df[dass_cols], recode_likert, levels_map = dass_levels)

# 6) UMPOLUNG (REVERSE CODING) DER BFI-2-ITEMS -----------------------------
rev_cols <- grep("_R$", names(df), value = TRUE)
df[rev_cols] <- lapply(df[rev_cols], function(x) 6 - x)   # 5-stufige Skala: 6 - x

# ab hier tragen alle BFI-Items (auch die vormals reversen) die "richtige"
# Polung; das Suffix "_R" bleibt nur als Herkunftskennzeichnung im Namen

# 7) SKALEN- UND FACETTENSCORES BERECHNEN ----------------------------------

## 7a) Domänenscores (zentral für H1-H4)
o_items <- grep("^O_", names(df), value = TRUE)
c_items <- grep("^C_", names(df), value = TRUE)

df$Offenheit_M       <- rowMeans(df[o_items], na.rm = FALSE)  # Mittelwert
df$Offenheit_Sum     <- rowSums(df[o_items],  na.rm = FALSE)  # Summenwert
df$Gewissenhaft_M    <- rowMeans(df[c_items], na.rm = FALSE)
df$Gewissenhaft_Sum  <- rowSums(df[c_items],  na.rm = FALSE)

## 7b) Facettenscores
## Die Zuordnung der Items zu den 6 Facetten (Ordnungsliebe, Fleiß,
## Verlässlichkeit / Ästhetisches Empfinden, Intellektuelle Neugierde,
## Kreativer Einfallsreichtum) sowie die Umpolung wurden gegen den
## offiziellen Auswertungsschlüssel von Danner, Rammstedt, Bluemke,
## Treiber, Berres, Soto & John (2016), "Die deutsche Version des
## Big Five Inventory 2 (BFI-2)", Tabelle 2, geprüft und stimmen exakt
## überein. Die hier verwendeten Variablennamen (O_cur/O_aes/O_crea,
## C_org/C_prod/C_resp) entsprechen also:
##   O_cur  = Intellektuelle Neugierde   | O_aes = Ästhetisches Empfinden
##   O_crea = Kreativer Einfallsreichtum | C_org = Ordnungsliebe
##   C_prod = Fleiß                      | C_resp = Verlässlichkeit
df$O_Neugier   <- rowMeans(df[c("O_cur_1","O_cur_2_R","O_cur_3","O_cur_4_R")], na.rm = FALSE)
df$O_Aesthetik <- rowMeans(df[c("O_aes_1_R","O_aes_2","O_aes_3","O_aes_4_R")], na.rm = FALSE)
df$O_Kreativ   <- rowMeans(df[c("O_crea_1","O_crea_2_R","O_crea_3_R","O_crea_4")], na.rm = FALSE)

df$C_Ordnung   <- rowMeans(df[c("C_org_1_R","C_org_2","C_org_3","C_org_4_R")], na.rm = FALSE)
df$C_Leistung  <- rowMeans(df[c("C_prod_1_R","C_prod_2_R","C_prod_3","C_prod_4")], na.rm = FALSE)
df$C_Verantw   <- rowMeans(df[c("C_resp_1","C_resp_2_R","C_resp_3","C_resp_4_R")], na.rm = FALSE)

## 7c) DASS-21 Subskalen (deutsche Kurzfassung, Nilges & Essau, 2021)
## Auswertung laut Manual: einfache Summenbildung über die 7 Items je
## Subskala (KEINE Verdopplung -- das ist nur bei der originalen
## englischen Version zum Vergleich mit der DASS-42 üblich, nicht bei
## dieser deutschen Kurzfassung).
s_items <- paste0("S", 1:7)
d_items <- paste0("D", 1:7)

df$Stress_Sum <- rowSums(df[s_items], na.rm = FALSE)
df$Depr_Sum   <- rowSums(df[d_items], na.rm = FALSE)
df$Stress_M   <- rowMeans(df[s_items], na.rm = FALSE)
df$Depr_M     <- rowMeans(df[d_items], na.rm = FALSE)

## Offizielle Cutoff-Werte der deutschen Kurzfassung (Nilges & Essau, 2021):
## Depression auffällig ab Summenwert >= 10, Stress auffällig ab >= 10.
## (Diese Cutoffs beziehen sich auf die einfache 0-3-Summe über 7 Items,
## OHNE Verdopplung.) Nützlich für die Stichprobenbeschreibung.
df$Depr_auffaellig   <- ifelse(df$Depr_Sum   >= 10, 1, 0)
df$Stress_auffaellig <- ifelse(df$Stress_Sum >= 10, 1, 0)

# 8) DEMOGRAFISCHE VARIABLEN KODIEREN ---------------------------------------
## Hinweis: car::recode() und dplyr::recode() haben denselben Namen, aber
## unterschiedliche Syntax. Da car nach dplyr geladen wird, "gewinnt"
## car::recode() -- deshalb hier bewusst case_when() aus dplyr verwenden.
df$geschlecht <- dplyr::case_when(
  df$geschlecht_raw %in% c("männlich", "Männlich") ~ 0,
  df$geschlecht_raw %in% c("weiblich", "Weiblich") ~ 1,
  df$geschlecht_raw %in% c("divers", "Divers")     ~ 2,
  TRUE ~ NA_real_
)

df$psychologie   <- ifelse(df$psychologie_raw   == "Ja", 1,
                           ifelse(df$psychologie_raw   == "Nein", 0, NA))
df$beziehung     <- ifelse(df$beziehung_raw     == "Ja", 1,
                           ifelse(df$beziehung_raw     == "Nein", 0, NA))
df$staatsbuerger <- ifelse(df$staatsbuerger_raw == "Ja", 1,
                           ifelse(df$staatsbuerger_raw == "Nein", 0, NA))

# 9) INTERNE KONSISTENZ (Cronbachs Alpha) PRÜFEN -----------------------------
psych::alpha(df[o_items])$total$raw_alpha
psych::alpha(df[c_items])$total$raw_alpha
psych::alpha(df[s_items])$total$raw_alpha
psych::alpha(df[d_items])$total$raw_alpha

# 10) FEHLENDE WERTE: LISTWISE DELETION VS. IMPUTATION -----------------------
analyse_vars <- c("alter", "geschlecht", "psychologie", "beziehung",
                  "staatsbuerger", "Stress_Sum", "Depr_Sum",
                  "Offenheit_Sum", "Gewissenhaft_Sum")

df_complete <- df[complete.cases(df[analyse_vars]), ]
nrow(df_complete)          # verbleibende Stichprobengröße
nrow(df) - nrow(df_complete)  # Anzahl ausgeschlossener Fälle

# --> Entscheidung gemäß Methodenteil: Bei ausreichender Stichprobengröße
#     Listwise Deletion (wie unten verwendet). Führt der Ausschluss zu
#     starkem Stichprobenverlust, alternativ Regressionsimputation, z.B.:
# library(mice)
# imp <- mice(df[analyse_vars], method = "norm.predict", m = 1, seed = 123)
# df_imputed <- complete(imp)

# Ab hier wird mit df_complete weitergearbeitet:
d <- df_complete

# 11) DESKRIPTIVSTATISTIK -----------------------------------------------------
describe_vars <- c("alter", "Stress_Sum", "Depr_Sum",
                   "Offenheit_Sum", "Gewissenhaft_Sum")
psych::describe(d[describe_vars])

table(d$geschlecht)
table(d$psychologie)
table(d$beziehung)
table(d$staatsbuerger)

# Anteil auffälliger Werte laut DASS-21-Cutoffs (Nilges & Essau, 2021)
table(d$Depr_auffaellig)
table(d$Stress_auffaellig)

# 12) ZENTRIERUNG DER PRÄDIKTOREN --------------------------------------------
d$Stress_c        <- scale(d$Stress_Sum,       center = TRUE, scale = FALSE)[,1]
d$Offenheit_c      <- scale(d$Offenheit_Sum,    center = TRUE, scale = FALSE)[,1]
d$Gewissenhaft_c   <- scale(d$Gewissenhaft_Sum, center = TRUE, scale = FALSE)[,1]

# 13) INTERAKTIONSTERME -------------------------------------------------------
d$Stress_x_Offenheit    <- d$Stress_c * d$Offenheit_c
d$Stress_x_Gewissenhaft <- d$Stress_c * d$Gewissenhaft_c

# 14) KORRELATIONEN (Pearson) -------------------------------------------------
cor_vars <- d[, c("Stress_Sum", "Depr_Sum", "Offenheit_Sum",
                  "Gewissenhaft_Sum", "alter")]
cor_matrix <- cor(cor_vars, use = "pairwise.complete.obs")
round(cor_matrix, 2)

# mit p-Werten:
psych::corr.test(cor_vars)$p

# 15) HIERARCHISCHE MULTIPLE REGRESSION (Kriterium: Depr_Sum) -----------------

## Schritt 1: Kovariaten
m1 <- lm(Depr_Sum ~ alter + geschlecht + psychologie + beziehung + staatsbuerger,
         data = d)
summary(m1)

## Schritt 2: Haupteffekte (zentriert)
m2 <- lm(Depr_Sum ~ alter + geschlecht + psychologie + beziehung + staatsbuerger +
           Stress_c + Offenheit_c + Gewissenhaft_c,
         data = d)
summary(m2)

## Schritt 3: Interaktionen (Moderation)
m3 <- lm(Depr_Sum ~ alter + geschlecht + psychologie + beziehung + staatsbuerger +
           Stress_c + Offenheit_c + Gewissenhaft_c +
           Stress_x_Offenheit + Stress_x_Gewissenhaft,
         data = d)
summary(m3)

# Modellvergleiche (Delta R^2 / F-Test je Schritt)
anova(m1, m2)
anova(m2, m3)

# 16) VORAUSSETZUNGSPRÜFUNG (bezogen auf finales Modell m3) -------------------

## Linearität & Homoskedastizität (visuell)
plot(m3, which = 1)   # Residuen vs. Fitted
## Normalverteilung der Residuen
plot(m3, which = 2)   # Q-Q-Plot
shapiro.test(residuals(m3))

## Homoskedastizität formal
lmtest::bptest(m3)    # Breusch-Pagan-Test

## Multikollinearität
car::vif(m3)

## Einflussreiche Fälle (Cook's Distance)
plot(m3, which = 5)
cooksd <- cooks.distance(m3)
influential <- which(cooksd > 4 / nrow(d))
influential

## Sensitivitätsanalyse: Modell ohne einflussreiche Fälle
m3_robust <- lm(Depr_Sum ~ alter + geschlecht + psychologie + beziehung + staatsbuerger +
                  Stress_c + Offenheit_c + Gewissenhaft_c +
                  Stress_x_Offenheit + Stress_x_Gewissenhaft,
                data = d[-influential, ])
summary(m3_robust)
# -> Koeffizienten von m3 und m3_robust vergleichen

## Bei Heteroskedastizität: robuste (HC) Standardfehler statt Neuschätzung
lmtest::coeftest(m3, vcov = sandwich::vcovHC(m3, type = "HC3"))

# 17) HYPOTHESENPRÜFUNG MIT GERICHTETEN/UNGERICHTETEN TESTS -------------------
# H1 (gerichtet, positiv):  Stress -> Depression
# H2a (gerichtet, negativ): Gewissenhaftigkeit -> Depression
# H2b (explorativ/kein Effekt erwartet): Offenheit -> Depression (zweiseitig)
# H3 (gerichtet): Stress x Gewissenhaftigkeit -> abschwächender Effekt
# H4 (ungerichtet): Stress x Offenheit -> Effekt in beliebige Richtung

model_summary <- broom::tidy(m3)

one_sided_p <- function(term, expected_sign = c("positive", "negative")) {
  expected_sign <- match.arg(expected_sign)
  row <- model_summary[model_summary$term == term, ]
  est <- row$estimate
  p_two <- row$p.value
  sign_ok <- (expected_sign == "positive" && est > 0) ||
    (expected_sign == "negative" && est < 0)
  p_one <- if (sign_ok) p_two / 2 else 1 - p_two / 2
  data.frame(term = term, estimate = est, p_two_sided = p_two, p_one_sided = p_one)
}

# H1: Stress positiv
one_sided_p("Stress_c", "positive")
# H2a: Gewissenhaftigkeit negativ
one_sided_p("Gewissenhaft_c", "negative")
# H3: Stress x Gewissenhaftigkeit negativ (abschwächend)
one_sided_p("Stress_x_Gewissenhaft", "negative")

# H2b und H4 werden zweiseitig getestet -> Werte direkt aus summary(m3)/model_summary
model_summary[model_summary$term %in% c("Offenheit_c", "Stress_x_Offenheit"), ]

# 18) JOHNSON-NEYMAN-PLOTS (nur bei signifikanten Interaktionen sinnvoll) -----
#
# Falls das Paket "interactions" bei dir installiert ist, kannst du
# stattdessen einfach folgende zwei Zeilen verwenden (auskommentiert):
# interactions::johnson_neyman(m3, pred = Stress_c, modx = Gewissenhaft_c, alpha = .05)
# interactions::johnson_neyman(m3, pred = Stress_c, modx = Offenheit_c,   alpha = .05)
# interactions::interact_plot(m3, pred = Stress_c, modx = Gewissenhaft_c, interval = TRUE)
# interactions::interact_plot(m3, pred = Stress_c, modx = Offenheit_c,   interval = TRUE)
#
# Eigene (paketunabhängige) Implementierung nach Bauer & Curran (2005):
# Für ein Modell Y = b0 + b1*X + b2*Z + b3*X:Z + Kovariaten testet die
# Funktion, für welche Werte von Z (Moderator) die einfache Steigung von
# X (Prädiktor) signifikant von Null verschieden ist.

johnson_neyman_manual <- function(model, pred, modx, int_term, alpha = .05,
                                  modx_range = NULL, n_points = 500) {
  # pred, modx: Namen der Haupteffekt-Variablen im Modell (bereits zentriert)
  # int_term:   Name der (vorab manuell berechneten) Interaktionsspalte,
  #             z. B. "Stress_x_Gewissenhaft"
  b  <- coef(model)
  V  <- vcov(model)
  stopifnot(pred %in% names(b), int_term %in% names(b))
  
  b1 <- b[[pred]]; b3 <- b[[int_term]]
  var_b1 <- V[pred, pred]
  var_b3 <- V[int_term, int_term]
  cov_b1b3 <- V[pred, int_term]
  
  df_resid <- df.residual(model)
  t_crit <- qt(1 - alpha / 2, df_resid)
  
  # quadratische Gleichung a*Z^2 + b*Z + c = 0
  a <- b3^2 - (t_crit^2) * var_b3
  bb <- 2 * (b1 * b3 - (t_crit^2) * cov_b1b3)
  cc <- b1^2 - (t_crit^2) * var_b1
  
  disc <- bb^2 - 4 * a * cc
  roots <- if (disc >= 0 && a != 0) {
    sort(c((-bb + sqrt(disc)) / (2 * a), (-bb - sqrt(disc)) / (2 * a)))
  } else {
    NULL
  }
  
  if (is.null(modx_range)) {
    modx_vals <- model$model[[modx]]
    modx_range <- range(modx_vals, na.rm = TRUE)
  }
  z_seq <- seq(modx_range[1], modx_range[2], length.out = n_points)
  slope <- b1 + b3 * z_seq
  se_slope <- sqrt(var_b1 + (z_seq^2) * var_b3 + 2 * z_seq * cov_b1b3)
  t_val <- slope / se_slope
  sig <- abs(t_val) > t_crit
  
  list(
    jn_points = roots,
    table = data.frame(modx = z_seq, slope = slope, se = se_slope,
                       t = t_val, significant = sig)
  )
}

jn_gewissenhaft <- johnson_neyman_manual(m3, pred = "Stress_c", modx = "Gewissenhaft_c",
                                         int_term = "Stress_x_Gewissenhaft")
jn_gewissenhaft$jn_points   # Werte von Gewissenhaftigkeit (zentriert), an denen die
# Steigung von Stress signifikant wird
jn_offenheit <- johnson_neyman_manual(m3, pred = "Stress_c", modx = "Offenheit_c",
                                      int_term = "Stress_x_Offenheit")
jn_offenheit$jn_points

# Plot: einfache Steigung von Stress über den Wertebereich des Moderators,
# inkl. Konfidenzband und JN-Grenzen (analog zum klassischen JN-Plot)
plot_johnson_neyman <- function(jn_result, modx_label) {
  tab <- jn_result$table
  t_crit <- qt(.975, df.residual(m3))
  tab$ci_low  <- tab$slope - t_crit * tab$se
  tab$ci_high <- tab$slope + t_crit * tab$se
  
  ggplot(tab, aes(x = modx, y = slope)) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = .15) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    { if (!is.null(jn_result$jn_points))
      geom_vline(xintercept = jn_result$jn_points, linetype = "dotted", color = "red")
    } +
    labs(x = paste0(modx_label, " (zentriert)"),
         y = "Einfache Steigung von Stress auf Depression",
         title = paste0("Johnson-Neyman-Plot: Moderation durch ", modx_label)) +
    theme_minimal()
}

plot_johnson_neyman(jn_gewissenhaft, "Gewissenhaftigkeit")
plot_johnson_neyman(jn_offenheit,    "Offenheit für Erfahrungen")

# 19) EXPLORATIV: INTERAKTIONEN AUF FACETTENEBENE -----------------------------
facetten_c <- c("O_Neugier", "O_Aesthetik", "O_Kreativ",
                "C_Ordnung", "C_Leistung", "C_Verantw")

for (f in facetten_c) {
  d[[paste0(f, "_c")]] <- scale(d[[f]], center = TRUE, scale = FALSE)[,1]
  d[[paste0("Stress_x_", f)]] <- d$Stress_c * d[[paste0(f, "_c")]]
}

facet_formulas <- lapply(facetten_c, function(f) {
  as.formula(paste0(
    "Depr_Sum ~ alter + geschlecht + psychologie + beziehung + staatsbuerger + ",
    "Stress_c + ", f, "_c + Stress_x_", f
  ))
})

facet_models <- lapply(facet_formulas, lm, data = d)
names(facet_models) <- facetten_c
lapply(facet_models, summary)

# 20) OBJEKTE SPEICHERN (optional, für Reproduzierbarkeit) --------------------
saveRDS(d, "daten_aufbereitet.rds")
write.csv(d, "daten_aufbereitet.csv", row.names = FALSE)

#Tabellen 1
demo_tab <- data.frame(
  Merkmal = c(
    "Geschlecht: m\u00e4nnlich", "Geschlecht: weiblich", "Geschlecht: divers",
    "Psychologiestudium: nein", "Psychologiestudium: ja",
    "Beziehung: nein", "Beziehung: ja",
    "\u00d6sterr. Staatsb\u00fcrgerschaft: nein", "\u00d6sterr. Staatsb\u00fcrgerschaft: ja"
  ),
  n = c(
    sum(d$geschlecht == 0, na.rm = TRUE), sum(d$geschlecht == 1, na.rm = TRUE),
    sum(d$geschlecht == 2, na.rm = TRUE),
    sum(d$psychologie == 0, na.rm = TRUE), sum(d$psychologie == 1, na.rm = TRUE),
    sum(d$beziehung == 0, na.rm = TRUE), sum(d$beziehung == 1, na.rm = TRUE),
    sum(d$staatsbuerger == 0, na.rm = TRUE), sum(d$staatsbuerger == 1, na.rm = TRUE)
  )
)
demo_tab$Prozent <- round(100 * demo_tab$n / nrow(d), 1)

## --- Teil B: Skalen-Deskriptivstatistik + Reliabilitaet ------------------
skalen <- c("alter", "Stress_Sum", "Depr_Sum", "Offenheit_Sum", "Gewissenhaft_Sum")
labels <- c("Alter", "Stress (DASS-21)", "Depression (DASS-21)",
            "Offenheit (BFI-2)", "Gewissenhaftigkeit (BFI-2)")
desc <- psych::describe(d[skalen])
alphas <- c(NA,
            psych::alpha(d[s_items])$total$raw_alpha,
            psych::alpha(d[d_items])$total$raw_alpha,
            psych::alpha(d[o_items])$total$raw_alpha,
            psych::alpha(d[c_items])$total$raw_alpha)
skalen_tab <- data.frame(
  Variable = labels, M = round(desc$mean, 2), SD = round(desc$sd, 2),
  Min = desc$min, Max = desc$max,
  Alpha = ifelse(is.na(alphas), "--", sprintf("%.2f", alphas))
)

## --- Hilfsfunktion: data.frame -> APA-HTML-Tabelle -----------------------
html_table_apa <- function(df, caption) {
  th <- paste0("<th style='text-align:left; border-top:1.5px solid black;",
               " border-bottom:1px solid black; padding:4px 10px;'>",
               names(df), "</th>", collapse = "")
  rows <- apply(df, 1, function(r) {
    tds <- paste0("<td style='padding:4px 10px;'>", r, "</td>", collapse = "")
    paste0("<tr>", tds, "</tr>")
  })
  last <- length(rows)
  rows[last] <- sub("<td", "<td style='border-bottom:1.5px solid black;'",
                    rows[last])
  paste0(
    "<p><i>", caption, "</i></p>",
    "<table style='border-collapse:collapse; font-family:\"Times New Roman\"; font-size:12pt;'>",
    "<tr>", th, "</tr>", paste(rows, collapse = ""), "</table><br>"
  )
}

html_out <- paste0(
  "<html><body>",
  html_table_apa(demo_tab,   "Tabelle 1. Demografische Merkmale der Stichprobe (N = 123)"),
  html_table_apa(skalen_tab, "Tabelle 2. Deskriptivstatistik und interne Konsistenz der zentralen Skalen (N = 123)"),
  "</body></html>"
)

writeLines(html_out, "Tabellen_APA.html", useBytes = TRUE)
cat("Fertig! Datei 'Tabellen_APA.html' im Ordner:", getwd(), "\n")
cat("Oeffne sie per Doppelklick oder ueber Word (Datei -> Oeffnen).\n")

#Tabelle 2
vars <- c("Stress_Sum", "Depr_Sum", "Offenheit_Sum", "Gewissenhaft_Sum", "alter")
labels <- c("1. Stress", "2. Depression", "3. Offenheit",
            "4. Gewissenhaftigkeit", "5. Alter")

k <- length(vars)
r_mat <- matrix(NA, k, k)
p_mat <- matrix(NA, k, k)

for (i in 1:k) {
  for (j in 1:k) {
    if (i != j) {
      test <- cor.test(d[[vars[i]]], d[[vars[j]]], method = "pearson")
      r_mat[i, j] <- test$estimate
      p_mat[i, j] <- test$p.value
    }
  }
}

## Signifikanz-Sterne
stars <- function(p) {
  ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
}

## Nur untere Dreiecksmatrix befuellen (APA-Standard), Diagonale = "--"
cor_tab <- data.frame(Variable = labels, stringsAsFactors = FALSE)
for (j in 1:(k - 1)) {
  col <- character(k)
  for (i in 1:k) {
    if (i == j) {
      col[i] <- "--"
    } else if (i > j) {
      col[i] <- paste0(sprintf("%.2f", r_mat[i, j]), stars(p_mat[i, j]))
    } else {
      col[i] <- ""
    }
  }
  cor_tab[[as.character(j)]] <- col
}

print(cor_tab, row.names = FALSE)

## Als CSV exportieren (Basis-R, kein Zusatzpaket noetig)
write.csv(cor_tab, "Tabelle2_Korrelationen.csv", row.names = FALSE)
cat("Gespeichert als Tabelle2_Korrelationen.csv\n")

#Tabelle 3
get_coefs <- function(model) {
  s <- summary(model)$coefficients
  data.frame(
    term = rownames(s),
    b  = round(s[, "Estimate"], 2),
    se = round(s[, "Std. Error"], 2),
    p  = s[, "Pr(>|t|)"]
  )
}

stars <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))

c1 <- get_coefs(m1); c2 <- get_coefs(m2); c3 <- get_coefs(m3)

alle_terms <- unique(c(c1$term, c2$term, c3$term))
alle_terms <- alle_terms[alle_terms != "(Intercept)"]
alle_terms <- c("(Intercept)", alle_terms)  # Intercept nach oben

fmt <- function(ctab, term) {
  row <- ctab[ctab$term == term, ]
  if (nrow(row) == 0) return("")
  paste0(sprintf("%.2f", row$b), stars(row$p), " (", sprintf("%.2f", row$se), ")")
}

reg_tab <- data.frame(
  Praediktor = alle_terms,
  Schritt1 = sapply(alle_terms, fmt, ctab = c1),
  Schritt2 = sapply(alle_terms, fmt, ctab = c2),
  Schritt3 = sapply(alle_terms, fmt, ctab = c3)
)

## Modellkennwerte unten anfuegen
r2 <- c(summary(m1)$r.squared, summary(m2)$r.squared, summary(m3)$r.squared)
adj_r2 <- c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared, summary(m3)$adj.r.squared)
delta_r2 <- c(NA, r2[2] - r2[1], r2[3] - r2[2])

a12 <- anova(m1, m2); a23 <- anova(m2, m3)
f_delta <- c(NA, a12$F[2], a23$F[2])
p_delta <- c(NA, a12$`Pr(>F)`[2], a23$`Pr(>F)`[2])

kennwerte <- data.frame(
  Praediktor = c("R\u00b2", "\u0394R\u00b2", "F (\u0394R\u00b2)"),
  Schritt1 = c(sprintf("%.3f", r2[1]), "", ""),
  Schritt2 = c(sprintf("%.3f", r2[2]), sprintf("%.3f%s", delta_r2[2], stars(p_delta[2])), sprintf("%.2f", f_delta[2])),
  Schritt3 = c(sprintf("%.3f", r2[3]), sprintf("%.3f%s", delta_r2[3], stars(p_delta[3])), sprintf("%.2f", f_delta[3]))
)

reg_tab_full <- rbind(reg_tab, kennwerte)
print(reg_tab_full, row.names = FALSE)

write.csv(reg_tab_full, "Tabelle4_Regression.csv", row.names = FALSE)
cat("Gespeichert als Tabelle4_Regression.csv\n")

#Tabelle 4
facet_labels <- c(
  O_Neugier   = "Intellektuelle Neugierde",
  O_Aesthetik = "Aesthetisches Empfinden",
  O_Kreativ   = "Kreativer Einfallsreichtum",
  C_Ordnung   = "Ordnungsliebe",
  C_Leistung  = "Fleiss",
  C_Verantw   = "Verlaesslichkeit"
)

stars <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .10, "~", ""))))

rows <- lapply(names(facet_models), function(f) {
  s <- summary(facet_models[[f]])$coefficients
  main_term <- paste0(f, "_c")
  int_term  <- paste0("Stress_x_", f)
  data.frame(
    Facette   = facet_labels[f],
    Haupteffekt = paste0(sprintf("%.2f", s[main_term, "Estimate"]), stars(s[main_term, "Pr(>|t|)"]),
                         " (", sprintf("%.2f", s[main_term, "Std. Error"]), ")"),
    p_Haupt   = sprintf("%.3f", s[main_term, "Pr(>|t|)"]),
    Interaktion = paste0(sprintf("%.2f", s[int_term, "Estimate"]), stars(s[int_term, "Pr(>|t|)"]),
                         " (", sprintf("%.2f", s[int_term, "Std. Error"]), ")"),
    p_Interaktion = sprintf("%.3f", s[int_term, "Pr(>|t|)"])
  )
})

facet_tab <- do.call(rbind, rows)
print(facet_tab, row.names = FALSE)

write.csv(facet_tab, "Tabelle5_Facetten.csv", row.names = FALSE)
cat("Gespeichert als Tabelle5_Facetten.csv\n")

c3_robust <- get_coefs(m3_robust)

alle_terms_robust <- unique(c(c3$term, c3_robust$term))
alle_terms_robust <- alle_terms_robust[alle_terms_robust != "(Intercept)"]
alle_terms_robust <- c("(Intercept)", alle_terms_robust)

reg_tab_robust <- data.frame(
  Praediktor = alle_terms_robust,
  Modell_gesamt   = sapply(alle_terms_robust, fmt, ctab = c3),
  Modell_ohne_einflussreiche = sapply(alle_terms_robust, fmt, ctab = c3_robust)
)

print(reg_tab_robust, row.names = FALSE)
write.csv(reg_tab_robust, "TabelleA1_Sensitivitaet.csv", row.names = FALSE)