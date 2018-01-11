library(tidyverse)
library(readxl)


cm_info = read_excel("data/raw/cm_list.xlsx")
boot_result = read_csv("data/intermediate/boot_results_with_uu.csv")

df_merge = boot_result %>% 
    left_join(cm_info, by = c("cm_id" = "Id"))

df_merge %>% 
    write_excel_csv("data/output/boot_result.csv")


