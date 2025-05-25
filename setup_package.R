
# setup_package.R
# ⚙️ Script per documentare e installare il pacchetto biasTools localmente

# Imposta la directory del pacchetto (modifica se necessario)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Carica devtools (installa se serve)
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
library(devtools)

# Genera la documentazione e installa il pacchetto
document()
install()
