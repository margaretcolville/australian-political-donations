library(tabulizer)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(forcats)
library(tibble)
library(ggplot2)
library(ggrepel)
library(plotly)

clean_table_full <- function(t) {
  NameField <- ""
  ValueField <- ""
  TypeField <- ""

  if(ncol(t) == 3) {
    NameField <- names(t)[1]
    ValueField <- names(t)[2]
    TypeField <- names(t)[3]
  } else {
    NameField <- names(t)[2]
  
    for(col in 1:ncol(t)) {
      if(sum(grepl("\\$[0-9]+[,]*[0-9]*", t[[col]])) > 0) {
        ValueField <- names(t)[col]
      }
      if(
        sum(grepl("Donation",t[[col]]) + 
            grepl("Other Receipt",t[[col]]) +
            grepl("Financial",t[[col]]) +
            grepl("Non Financial", t[[col]])) > 0
      ) {
        TypeField <- names(t)[col]
      }
        
    }
  }
  
  #print(NameField)
  #print(ValueField)
  #print(TypeField)
  
  t %>%
    rename(Name = !!sym(NameField)) %>%
    rename(Value = !!sym(ValueField)) %>%
    rename(Type = !!sym(TypeField)) %>%
    select(Name, Value, Type) %>%
    mutate(Name = str_replace(Name, '\\*[0-9]','')) %>%
    mutate(Value_num = str_replace_all(Value, ',', '')) %>%
    mutate(Value_num = str_replace_all(Value_num, '\\$', '')) %>%
    mutate(Value_num = as.numeric(Value_num)) %>%
    filter(Value_num > 0) %>%
  identity()
    
}
extract_total <- function(filepath, searchstring) {
  extract_text(filepath) %>% 
  strsplit("\r\n") %>%
    data.frame(stringsAsFactors = FALSE) %>% 
    rename(text = 1) %>% 
    filter(grepl("all amounts received for the financial year", text)) %>%
    mutate(text = word(text,-1)) %>%
    mutate(text = str_replace_all(text, ',', '')) %>%
    mutate(text = str_replace_all(text, '\\$', '')) %>%
    mutate(text = as.numeric(text)) %>%
    pull()
  
}
extract_total_in <- function(filepath) {
  extract_total(filepath, "all amounts received for the financial year")
}

# Parameters ----
lib_colour <- "#1B4D97"
lab_colour <- "#DA373E"
nat_colour <- "#FCD403"
grn_colour <- "#00973B"
onenat_colour <- "#EC6A23"
colour_set <- c("Liberals" = lib_colour, 
                "Labor" = lab_colour, 
                "Nationals" = nat_colour,
                "Greens" = grn_colour,
                "One Nation" = onenat_colour)

# Liberals 17/18 ----
lib1718_total_in <- extract_total_in('Disclosures/1718/YJMZ9-Liberals1718.pdf')
lib1718_total_out <- 4901921


lib1718_interim <-
  extract_tables('Disclosures/1718/YJMZ9-Liberals1718.pdf',
                 output = 'data.frame',
                 header = FALSE)

lib1718_interim_cleaned <- list()

for (table in 2:(length(lib1718_interim)-1)) {
  lib1718_interim_cleaned[[table]] <- lib1718_interim[[table]] %>%
    clean_table_full()
}

lib1718_donations <- 
bind_rows (lib1718_interim_cleaned) %>%
  mutate(Name = ifelse(Name == "Pearce, Mr John", "Mercantile Solutions Pty Ltd", Name)) %>%
  mutate(Party = "Liberals") %>%
  mutate(Year = "17/18") %>%
  select(Party, Year, Name, Value, Type, Value_num)

lib1718_debts <- lib1718_interim[[length(lib1718_interim)]] %>%
  clean_table_full() %>%
  mutate(Party = "Liberals") %>%
  mutate(Year = "17/18") %>%
  select(Party, Year, Name, Value, Type, Value_num)

# Labor 17/18 ----
lab1718_total_in <- extract_total_in('Disclosures/1718/YRRM2-Labor1718.pdf')
lab1718_total_out <- 10099926

lab1718_interim <-
  extract_tables('Disclosures/1718/YRRM2-Labor1718.pdf',
                 output = 'data.frame',
                 header = FALSE)

lab1718_interim_cleaned <- list()

for (table in 2:(length(lab1718_interim)-1)) {
  lab1718_interim_cleaned[[table]] <- lab1718_interim[[table]] %>%
    clean_table_full()
}

lab1718_donations <- bind_rows(lab1718_interim_cleaned) %>%
  mutate(Party = "Labor") %>%
  mutate(Year = "17/18") %>%
  select(Party, Year, Name, Value, Type, Value_num)

lab1718_debts <- lab1718_interim[[length(lab1718_interim)]] %>%
  clean_table_full() %>%
  mutate(Party = "Labor") %>%
  mutate(Year = "17/18") %>%
  select(Party, Year, Name, Value, Type, Value_num)

# Nationals 17/18 ----
nat1718_total_in <- extract_total_in('Disclosures/1718/YLEU4-Nationals1718.pdf')
nat1718_total_out <- 2080705

nat1718_interim <-
  extract_tables('Disclosures/1718/YLEU4-Nationals1718.pdf',
                 output = 'data.frame',
                 header = FALSE)

nat1718_donations <- bind_rows(
  nat1718_interim[[2]] %>%
    clean_table_full(),
  
  nat1718_interim[[3]] %>%
    clean_table_full()
) %>%
  mutate(Party = "Nationals") %>%
  mutate(Year = "17/18") %>%
  select(Party, Year, Name, Value, Type, Value_num)

# Greens 17/18 ----
grn1718_total_in <- extract_total_in('Disclosures/1718/YLQI8-Greens1718.pdf')
grn1718_total_out <- 2400224

grn1718_interim <- 
  extract_tables('Disclosures/1718/YLQI8-Greens1718.pdf',
                 output = 'data.frame',
                 header = FALSE)

grn1718_donations <- 
  grn1718_interim[[2]] %>%
  clean_table_full() %>%
  mutate(Party = "Greens") %>%
  mutate(Year = "17/18") %>%
  select(Party, Year, Name, Value, Type, Value_num)

grn1718_debts <- 
  grn1718_interim[[3]] %>%
  clean_table_full() %>%
  mutate(Party = "Greens") %>%
  mutate(Year = "17/18") %>%
  select(Party, Year, Name, Value, Type, Value_num)

# One Nation 17/18 ----
#onenat1718_total_in <- extract_total_in('Disclosures/1718/YLQB3-OneNation1718.pdf')
onenat1718_total_in <- 1709611
onenat1718_total_out <- 1750142

onenat1718_interim <- 
  extract_tables('Disclosures/1718/YLQB3-OneNation1718.pdf',
                 output = 'data.frame',
                 header = FALSE)

onenat1718_donations <- 
  onenat1718_interim[[2]] %>%
  clean_table_full() %>%
  mutate(Name = ifelse(Name == "WAEC" & Value == "$138,667", "Pauline Hansons One Nation - West Australia Div",Name)) %>%
    add_row(Name = "Australian Taxation Office", Value = "$31,492",Type = "Other Receipt", Value_num = 31492) %>%
  filter(!Name == "Adani Mining") %>%
  filter(!Name == "APA Group") %>%
  mutate(Party = "One Nation") %>%
  mutate(Year = "17/18") %>%
  select(Party, Year, Name, Value, Type, Value_num)

onenat1718_debts <-
onenat1718_interim[[3]] %>%
  clean_table_full() %>%
  filter(!Value == "$9,375") %>%
  mutate(Party = "One Nation") %>%
  mutate(Year = "17/18") %>%
  select(Party, Year, Name, Value, Type, Value_num)

# Merged ----

totals_in_merged <- bind_rows(
  cbind.data.frame(Party = "Liberals", Total = lib1718_total_in, stringsAsFactors = FALSE),
  cbind.data.frame(Party = "Labor", Total = lab1718_total_in, stringsAsFactors = FALSE),
  cbind.data.frame(Party = "Nationals", Total = nat1718_total_in, stringsAsFactors = FALSE),
  cbind.data.frame(Party = "Greens", Total = grn1718_total_in, stringsAsFactors = FALSE),
  cbind.data.frame(Party = "One Nation", Total = onenat1718_total_in, stringsAsFactors = FALSE)
)
write.csv(totals_in_merged, "totals_in.csv", row.names = FALSE)

totals_out_merged <- bind_rows(
  cbind.data.frame(Party = "Liberals", Total = lib1718_total_out, stringsAsFactors = FALSE),
  cbind.data.frame(Party = "Labor", Total = lab1718_total_out, stringsAsFactors = FALSE),
  cbind.data.frame(Party = "Nationals", Total = nat1718_total_out, stringsAsFactors = FALSE),
  cbind.data.frame(Party = "Greens", Total = grn1718_total_out, stringsAsFactors = FALSE),
  cbind.data.frame(Party = "One Nation", Total = onenat1718_total_out, stringsAsFactors = FALSE)
)
write.csv(totals_out_merged, "totals_out.csv", row.names = FALSE)

donations_merged <- bind_rows(
lib1718_donations,
lab1718_donations,
nat1718_donations,
grn1718_donations,
onenat1718_donations
) %>%
filter(!is.na(Value_num))
write.csv(donations_merged, "donations.csv", row.names = FALSE)

debts_merged <- bind_rows(
lib1718_debts,
lab1718_debts,
#nat1718_debts,
grn1718_debts,
onenat1718_debts
)
write.csv(debts_merged, "debts.csv", row.names = FALSE)

# Charts ----

totals_in_merged %>%
  mutate(Group = ifelse(Party %in% c("Liberals", "Nationals"),"Coalition",Party)) %>%
  rename(PartyDollars = Total) %>%
  left_join(
    totals_in_merged %>%
      mutate(Group = ifelse(Party %in% c("Liberals", "Nationals"),"Coalition",Party)) %>%
      group_by(Group) %>%
      summarise(GroupDollars = sum(Total)) %>%
      ungroup(), 
    by = "Group"
              ) %>%
  mutate(Group = fct_reorder(Group, GroupDollars)) %>%
  mutate(Party = fct_reorder(Party, PartyDollars)) %>%
  ggplot(aes(x = Group, y = PartyDollars, fill = Party)) +
  geom_col() +
  scale_fill_manual(values = colour_set) +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "top") +
  labs(title = "Total receipts declared 17/18") +
  guides(fill = guide_legend(reverse = TRUE)) +
  NULL

donations_merged %>%
  filter(Year == "17/18") %>%
  filter(Value_num >= 13500) %>%
  mutate(Group = ifelse(Party %in% c("Liberals", "Nationals"),"Coalition",Party)) %>%
  group_by(Group, Party) %>%
  summarise(PartyIndividualDeclaration  = sum(Value_num)) %>%
  ungroup() %>%
  left_join(
    totals_in_merged %>%
      rename(PartyTotalDeclaration = Total),
    by = "Party"
  ) %>%
  left_join(
    donations_merged %>%
      filter(Value_num >= 13500) %>%
      mutate(Group = ifelse(Party %in% c("Liberals", "Nationals"),"Coalition",Party)) %>%
      group_by(Group) %>%
      summarise(GroupIndividualDeclaration = sum(Value_num)) %>%
      ungroup(),
    by = "Group"
  ) %>%
  mutate(Group = fct_reorder(Group, -GroupIndividualDeclaration)) %>%
  mutate(Party = fct_reorder(Party, PartyTotalDeclaration)) %>%
  gather(Type, Value, PartyIndividualDeclaration:PartyTotalDeclaration) %>%
  mutate(Type = ifelse(Type == "PartyIndividualDeclaration","Individual", "Total")) %>%
  mutate(Type = factor(Type, ordered = TRUE, levels = c("Total", "Individual"))) %>%
  mutate(Value_mil = Value / 10^6) %>%
  ggplot(aes(x = Type, y = Value_mil, fill = Party, alpha = Type)) +
  geom_col() +
  facet_grid(~Group) +
  scale_fill_manual(values = colour_set) +
  scale_y_continuous(labels = scales::dollar) +
  scale_alpha_manual(values = c(1,0.7)) +
  theme_bw() +
  labs(title = "Monies received in total vs total of transactions over $13500",
       x = "",
       y = "$ (millions)") +
  guides(fill = guide_legend(reverse = TRUE), alpha = FALSE) +
  NULL
  


donations_merged %>%
  filter(Year == "17/18") %>%
  filter(Value_num >= 13500) %>%
  mutate(Group = ifelse(Party %in% c("Liberals", "Nationals"),"Coalition",Party)) %>%
  group_by(Group, Party) %>%
  summarise(PartyDollars = sum(Value_num)) %>%
  ungroup() %>%
  left_join(
    donations_merged %>%
      filter(Value_num >= 13500) %>%
      mutate(Group = ifelse(Party %in% c("Liberals", "Nationals"),"Coalition",Party)) %>%
      group_by(Group) %>%
      summarise(GroupDollars = sum(Value_num)) %>%
      ungroup(),
     by = "Group"
  ) %>%
  mutate(Group = fct_reorder(Group, GroupDollars)) %>%
  mutate(Party = fct_reorder(Party, PartyDollars)) %>%
  ggplot(aes(x = Group, y = PartyDollars, fill = Party)) +
  geom_col() +
  scale_fill_manual(values = colour_set) +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "top") +
  labs(title = "Total receipts more than $13500 declared 17/18") +
  guides(fill = guide_legend(reverse = TRUE)) +
  NULL

donations_merged %>%
  filter(Type == "Donation") %>%
  filter(Year == "17/18") %>%
  filter(Value_num >= 13500) %>%
  mutate(Group = ifelse(Party %in% c("Liberals", "Nationals"),"Coalition",Party)) %>%
  group_by(Group, Party) %>%
  summarise(PartyDollars = sum(Value_num)) %>%
  ungroup() %>%
  left_join(
    
    donations_merged %>%
      filter(Type == "Donation") %>%
      filter(Value_num >= 13500) %>%
      mutate(Group = ifelse(Party %in% c("Liberals", "Nationals"),"Coalition",Party)) %>%
      group_by(Group) %>%
      summarise(GroupDollars = sum(Value_num)) %>%
      ungroup(), 
    by = "Group"
  ) %>%
  mutate(Group = fct_reorder(Group, GroupDollars)) %>%
  mutate(Party = fct_reorder(Party, PartyDollars)) %>%
  ggplot(aes(x = Group, y = PartyDollars, fill = Party)) +
  geom_col() +
  scale_fill_manual(values = colour_set) +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "top") +
  labs(title = "Total donations more than $13500 declared 17/18") +
  guides(fill = guide_legend(reverse = TRUE)) +
  NULL

donations_merged %>%
  filter(Year == "17/18") %>%
  filter(Value_num > 13500) %>%
  rename(ReceiptAmount = Value_num) %>%
  left_join(
    donations_merged %>%
      filter(Year == "17/18") %>%
      filter(Value_num > 13500) %>%
      group_by(Party) %>%
      summarise(PartyDollars = sum(Value_num)) %>%
      ungroup(),
    by = "Party"
  ) %>%
  mutate(Party = fct_reorder(Party, PartyDollars)) %>%
  mutate(Label = ifelse(ReceiptAmount >= 300000, paste0(Type, " from ", Name), NA)) %>%
  ggplot(aes(x = Party, y = ReceiptAmount, shape = Type)) +
  geom_jitter(aes(col = Party), height = 0) +
  #geom_label_repel(aes(label = Label)) +
  scale_color_manual(values = colour_set) +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "top",
        legend.box = "vertical") +
  labs(title = "Receipts more than $13500 declared 17/18") +
  guides(col = guide_legend(reverse = TRUE)) +
  NULL

donations_merged %>%
  filter(Year == "17/18") %>%
  filter(Value_num > 300000) %>%
  rename(ReceiptAmount = Value_num) %>%
  arrange(-ReceiptAmount) %>%
  select(Party, Year, Name, Type, Value) %>%
  View()

ggplotly(
  donations_merged %>%
    filter(Year == "17/18") %>%
    filter(Value_num > 13500) %>%
    filter(Type == "Donation") %>%
    rename(ReceiptAmount = Value_num) %>%
    left_join(
      donations_merged %>%
        filter(Year == "17/18") %>%
        filter(Value_num > 13500) %>%
        filter(Type == "Donation") %>%
        group_by(Party) %>%
        summarise(PartyDollars = sum(Value_num)) %>%
        ungroup(),
      by = "Party"
    ) %>%
    mutate(Party = fct_reorder(Party, PartyDollars)) %>%
    ggplot(aes(x = Party, text = paste0(Type, " from ", Name), y = ReceiptAmount)) +
    geom_jitter(aes(col = Party), height = 0) +
    scale_color_manual(values = colour_set) +
    theme_bw() +
    coord_flip() +
    scale_y_continuous(labels = scales::dollar) +
    theme(legend.position = "top") +
    labs(title = "Donations more than $13500 declared 17/18") +
    guides(fill = guide_legend(reverse = TRUE)) +
    NULL
)

debts_merged %>%
  filter(Year == "17/18") %>%
  group_by(Party) %>%
  summarise(PartyDollars = sum(Value_num)) %>%
  ungroup() %>%
  mutate(Party = fct_reorder(Party, PartyDollars)) %>%
  ggplot(aes(x = Party, y = PartyDollars, fill = Party)) +
  geom_col() +
  scale_fill_manual(values = colour_set) +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "top") +
  labs(title = "Debts declared 17/18") +
  guides(fill = guide_legend(reverse = TRUE)) +
  NULL

debts_merged %>%
  filter(Year == "17/18") %>%
  group_by(Party, Type) %>%
  summarise(PartyDollars = sum(Value_num)) %>%
  ungroup() %>%
  left_join(
    debts_merged %>%
      filter(Year == "17/18") %>%
      group_by(Party) %>%
      summarise(TotalDebt = sum(Value_num)) %>%
      ungroup()
  ) %>%
  mutate(Party = fct_reorder(Party, TotalDebt)) %>%
  ggplot(aes(x = Party, y = PartyDollars, fill = Party)) +
  geom_col() +
  scale_fill_manual(values = colour_set) +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "top") +
  facet_grid(Type~.) +
  labs(title = "Debts declared 17/18") +
  guides(fill = guide_legend(reverse = TRUE)) +
  NULL

debts_merged %>%
  filter(Year == "17/18") %>%
  mutate(PartyCreditor = paste0(Party, " - ", Name)) %>%
  rename(PartyDollars = Value_num) %>%
  left_join(
    debts_merged %>%
      filter(Year == "17/18") %>%
      group_by(Party) %>%
      summarise(TotalDebt = sum(Value_num)) %>%
      ungroup(),
    by = "Party"
  ) %>%
  mutate(Party = fct_reorder(Party, TotalDebt)) %>%
  mutate(PartyCreditor = fct_reorder(PartyCreditor, PartyDollars)) %>%
  filter(PartyDollars > 40000) %>%
  ggplot(aes(x = PartyCreditor, y = PartyDollars, fill = Party, alpha = Type)) +
  geom_col() +
  scale_fill_manual(values = colour_set) +
  scale_alpha_manual(values = c(1, 0.7)) +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "top") +
  facet_grid(Type~., scales = "free_y", space = "free_y") +
  labs(title = "Debts > $40,000 declared 17/18") +
  guides(fill = guide_legend(reverse = TRUE)) +
  NULL

debts_merged %>% 
  rename(PartyDollars = Value_num) %>%
  left_join(
    debts_merged %>%
      filter(Year == "17/18") %>%
      group_by(Party) %>%
      summarise(TotalDebt = sum(Value_num)) %>%
      ungroup(),
    by = "Party"
  ) %>%
  mutate(Party = fct_reorder(Party, TotalDebt)) %>%
  mutate(Label = ifelse(PartyDollars > 5 * 10 ^ 5, Name, NA)) %>%
  ggplot(aes(x = Party, y = PartyDollars, shape = Type, label = Label)) +
  geom_jitter(aes(col = Party), height = 0) +
  geom_label_repel() +
  scale_color_manual(values = colour_set) +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "top",
        legend.box = "vertical") +
  labs(title = "Debts declared 17/18") +
  guides(col = guide_legend(reverse = TRUE), label = FALSE)+
  NULL
