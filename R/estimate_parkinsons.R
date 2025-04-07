library(dplyr)
library(readr)
library(here)
library(tidyr)


# downloaded from NHANES
# this is the drug table lookup
file_drug = here::here("data/RXQ_DRUG.xpt")
if (!file.exists(file_drug)) {
  url = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/1988/DataFiles/RXQ_DRUG.xpt"
  curl::curl_download(url, destfile = file_drug)
}
table_drug = haven::read_xpt(file_drug)
mat_grp = sapply(table_drug, function(r) grepl("PARK", r))
cn_parkinsons = names(which(colSums(mat_grp) > 0))
drug_park = table_drug[rowSums(mat_grp) > 0, ]


# read in the nhanesA::nhanes translated data
# don't do it every time because it sometimes times out
read_translated = function(nh_table = "RXQ_RX_G") {
  file_translated = here::here("data", paste0(nh_table, "_translated.rds"))
  if (!file.exists(file_translated)) {
    data = nhanesA::nhanes(nh_table = nh_table,
                           includelabels = TRUE, nchar = 1000)
    readr::write_rds(data, file_translated)
  }
  out = readr::read_rds(file_translated)
  out
}
pax_g = read_translated("PAXHD_G")
pax_h = read_translated("PAXHD_H")
pax = pax_g %>%
  mutate(wave = "G") %>%
  full_join(pax_h %>%
              mutate(wave = "H"))

# 2 waves
drug_g = read_translated("RXQ_RX_G")
drug_h = read_translated("RXQ_RX_H")

# see what columns are missing from one or other
double_setdiff = function(x, y) {
  c(
    setdiff(x = x, y = y),
    setdiff(y = x, x = y)
  )
}
sd = double_setdiff(colnames(drug_g), colnames(drug_h))
# make sure not missing the drug designations (but shouldn't be in there)
stopifnot(!any(sd %in% cn_parkinsons))
stopifnot(length(intersect(drug_g$SEQN, drug_h$SEQN)) == 0)

# get one data set
drug = drug_g %>%
  mutate(wave = "G") %>%
  full_join(drug_h %>%
              mutate(wave = "H"))


# get unique drugs and make sure in lookup table
udrug = drug %>%
  distinct(RXDDRGID, RXDDRUG)
x = udrug %>%
  left_join(table_drug, by = join_by(RXDDRGID))
check = x %>%
  filter(
    RXDDRUG.x != RXDDRUG.y |
      (is.na(RXDDRUG.x) & !is.na(RXDDRUG.y)) |
      (is.na(RXDDRUG.y) & !is.na(RXDDRUG.x))
  )
# these are fine - one real case and not important (esp for Parkinsons)
stopifnot(nrow(check) == 5)
uids = unique(check$RXDDRGID)
uids = uids[!is.na(uids) & !uids %in% ""]
stopifnot(length(uids) == 1)
# the only case is INSULIN ISOPHANE vs. INSULIN ISOPHANE (NPH)
stopifnot(all(uids %in% "d04370"))

# use the drug name from the drug lookup table not the x data
# for that one case and make it easier
drug = drug %>%
  select(-RXDDRUG) %>%
  left_join(table_drug, by = join_by(RXDDRGID))

drug = drug %>%
  select(all_of(c("SEQN", "wave", "RXDDRGID", "RXDDRUG", cn_parkinsons)))
id_drug_2021 = c("d00184",
                 "d04877",
                 "d03473",
                 "d04537",
                 "d00086",
                 "h00026",
                 "h00031",
                 "d04145",
                 "d05848",
                 "d04215",
                 "d00976",
                 "d04991",
                 "d00178",
                 "d04460",
                 "d00277",
                 "d04750",
                 "d04220",
                 "d05612")
drug = drug %>%
  mutate(
    is_parkinsons = RXDDRGID %in% drug_park$RXDDRGID,
    is_parkinsons_2021 = RXDDRGID %in% id_drug_2021
  )


df_drug = drug %>%
  group_by(SEQN, wave) %>%
  summarise(
    is_parkinsons_2021_prescribed = any(is_parkinsons_2021, na.rm = TRUE),
    is_parkinsons_prescribed = any(is_parkinsons, na.rm = TRUE),
    .groups = "drop"
  )
all(df_drug$SEQN %in% pax$SEQN)
all(pax$SEQN %in% df_drug$SEQN)
stopifnot(all(pax$PAXSTS %in% c("Yes", "No")))
pax = pax %>%
  mutate(
    has_accel = PAXSTS == "Yes"
  ) %>%
  distinct(SEQN, has_accel)
stopifnot(anyDuplicated(pax$SEQN) == 0)
df_drug = df_drug %>%
  left_join(pax, by = join_by(SEQN)) %>%
  mutate(
    has_accel = replace_na(has_accel, FALSE)
  )

readr::write_rds(df_drug, here::here("data", "df_prescribed_parkinsons.rds"))

