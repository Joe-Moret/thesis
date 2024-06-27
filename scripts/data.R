# load and subset dataset ---------------------------------
data <- read_dta(here("data", "All_data.dta")) %>% as_tibble()

data <- data %>% select(
  CUSIP, UGVKEY, sic, conm, tic, MthCalDt, MthCap, ShrOut, csho, 
  MthPrc, MthRet, datadate, fyear, fyr, act, che, lct, dlc, txp, dp, oancf, 
  xidoc, at, ib, dvt, bkvlps, teq, ceq, revt, ni, xint, xagt, mib, cogs
)

data <- data %>% rename(E = ib, A = at, D = dvt)

# harmonizing the scale of CRSP data to millions (note: book data (from COMPUSTAT) are reported in millions)
data <- data %>%
  mutate(
    MthCap = MthCap / 1000,
    ShrOut = ShrOut / 1000
  )

# map fiscal year ending (FYE) -----------------------------------
data <- data %>%
  mutate(
    mapped_fyear = case_when(
      fyr >= 4 & fyr <= 12 ~ fyear + 1,
      fyr >= 1 & fyr <= 3 ~ fyear,
      TRUE ~ NA_real_
    ),
    mapped_fyr = 6
  ) %>%
  relocate(mapped_fyear, mapped_fyr, .after = fyr)

# filter out redundant rows and arrange order (sorted by UGVKEY and mapped_fyear)  -----------------------------------
data <- data %>%
  mutate(MthCalDt = as.Date(MthCalDt)) %>%  
  filter(month(MthCalDt) == 6) %>% 
  arrange(UGVKEY, mapped_fyear) %>%
  filter(mapped_fyear >= 1963 & mapped_fyear <= 2023) # excluding 2024

# create Accruals (AC) variable -----------------------------------
data <- data %>%
  arrange(UGVKEY, mapped_fyear) %>%
  group_by(UGVKEY) %>%
  mutate(
    delta_act = act - lag(act),
    delta_che = che - lag(che),
    delta_lct = lct - lag(lct),
    delta_dlc = dlc - lag(dlc),
    delta_txp = txp - lag(txp),
    accruals_balance_sheet = (delta_act + delta_che) - (delta_lct - delta_dlc - delta_txp) - dp,
    accruals_cash_flow = E + xidoc - oancf,
    AC = ifelse(mapped_fyear < 1989 & (mapped_fyear != first(mapped_fyear)), 
                accruals_balance_sheet, 
                accruals_cash_flow)
  ) %>%
  group_by(UGVKEY, mapped_fyear) %>%
  mutate(AC = first(AC)) %>%
  ungroup() %>%
  select(-delta_act, -delta_che, -delta_lct, -delta_dlc, -delta_txp, -accruals_balance_sheet, -accruals_cash_flow)

# creating several variables -----------------------------------
data <- data %>%
  # Create dividends payments (DD) dummy
  mutate(DD = ifelse(D > 0, 1, 0)) %>%
  # Create negative earnings dummy (NegE) and interaction term (NegE_E)
  mutate(NegE = ifelse(E < 0, 1, 0),
         NegE_E = NegE * E) %>%
  # Create earnings per share (EPS) variable
  mutate(EPS = E / ShrOut) %>%
  # Create NegEPS dummy and interaction term (NegEPS_EPS)
  mutate(NegEPS = ifelse(EPS < 0, 1, 0),
         NegEPS_EPS = NegEPS * EPS) %>%
  # Create Book-to-Market (BM) variable
  mutate(BM = ceq / (MthPrc * ShrOut)) %>%  
  # Create Operating Profitability (OP) variable 
  mutate(OP = (revt - cogs - coalesce(xint, 0) - coalesce(xagt, 0)) / (ceq + coalesce(mib, 0))) %>%   
  # Create Investment (INV) variable
  arrange(UGVKEY, mapped_fyear) %>%
  mutate(INV = (A - lag(A)) / lag(A)) %>%     
  # Create Earnings/Price (EP) variable
  mutate(EP = EPS / MthPrc) %>%             
  # Create Return on Equity (ROE) variable
  mutate(ROE = ni / ceq) %>%                
  # Select and reorder columns
  select(
    CUSIP, UGVKEY, sic, conm, tic, MthCalDt, MthCap, ShrOut, csho, 
    MthPrc, MthRet, datadate, fyear, fyr, mapped_fyear, mapped_fyr, A, E, NegE, NegE_E, EPS, NegEPS, NegEPS_EPS, D, DD, AC, BM,
    OP, INV, EP, ROE, bkvlps, teq, ceq, revt, ni, xint, xagt, mib, cogs, act, che, lct, dlc, txp, dp, oancf, xidoc
  )

# create interest rate (IR) variable -----------------------------------
# Read the rates dataset
rates <- read_excel(here("data", "Yields_Tbill.xls"), sheet = 1, range = "A11:C732")

# Rename the columns for easier handling
colnames(rates) <- c("observation_date", "rate_1y", "rate_10y")

# Convert date column to Date type and filter for June
rates <- rates %>%
  mutate(observation_date = as.Date(observation_date)) %>%
  filter(month(observation_date) == 7) %>%    # Interest rates (1. July) is closest to the date of my analysis with the other variables (end of June)
  mutate(mapped_fyear = year(observation_date)) %>%
  select(mapped_fyear, `rate_1y`, `rate_10y`)

# Merge interest rates with main data based on mapped_fyear
data <- data %>%
  left_join(rates, by = "mapped_fyear")

# remove redundant dataset
rm(rates)


# only select necessary variables for analysis (computational efficiency) -----------------------------------
#  CUSIP, UGVKEY, sic, conm, tic, MthCalDt, MthCap, ShrOut, csho, 
#  MthPrc, MthRet, datadate, fyear, fyr, mapped_fyear, mapped_fyr, dependent_E, dependent_EPS, A, E, NegE, NegE_E, EPS, NegEPS, NegEPS_EPS, D, DD, AC, BM,
#  OP, INV, EP, ROE, bkvlps, teq, ceq, revt, ni, xint, xagt, mib, cogs, act, che, lct, dlc, txp, dp, oancf, xidoc
data <- data %>% 
  select(
  UGVKEY, sic, conm, MthCalDt, MthCap, ShrOut, csho, 
  MthPrc, MthRet, datadate, fyear, fyr, mapped_fyear, mapped_fyr, A, E, NegE, NegE_E, EPS, NegEPS, NegEPS_EPS, D, DD, AC, BM,
  OP, INV, EP, ROE, rate_1y, rate_10y
)


