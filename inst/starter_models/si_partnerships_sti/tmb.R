# Load macpan2 library
library(macpan2)

# Define the model with two-sided formulas for initial conditions and N
spec <- mp_tmb_model_spec(
  
  # Define the initial population sizes using two-sided formulas
  before = list(
    S ~ 900,    # 900 susceptible individuals
    I ~ 100,    # 100 infected individuals
    SS ~ 200,   # 200 Susceptible-Susceptible partnerships
    SI ~ 100,   # 100 Susceptible-Infected partnerships
    II ~ 50,    # 50 Infected-Infected partnerships
    N ~ S + I + 2*(SS + SI + II)  # Total population size as the sum of individuals
  ),
  
  # Define partnership formation and dissolution flows
  during = list(
    mp_per_capita_flow("S", "SS", "partner_form_rate * S / N", "form_new_partnership"),
    mp_per_capita_flow("SS", "S", "partner_dissolve_rate", "partnership_dissolution"),
    
    # Within-partner transmission (SI -> II)
    mp_per_capita_flow("SI", "II", "beta_SI * I / N", "within_partner_transmission"),
    
    # Between-partner transmission (S -> I)
    mp_per_capita_flow("S", "I", "beta_external * I / N", "external_transmission")
  ),
  
  # Define default transmission rates and other parameters
  default = list(
    partner_form_rate = 0.05,    # Partnership formation rate
    partner_dissolve_rate = 0.02, # Partnership dissolution rate
    beta_SI = 0.1,               # Transmission rate within SI partnerships
    beta_external = 0.05         # External transmission rate
  )
)
