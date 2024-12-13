#' @title to read files
#' @description \code{smc.convert_percentage_vars_2015}


smc.convert_percentage_vars_2015 <- function(sf_df){
  sf_df <- sf_df %>%
    mutate(
      # 性別割合
      smc.per.male = smc.population_male / smc.population,
      smc.per.female = smc.population_female / smc.population,
      # 年齢別人口割合
      smc.per.population0_4 = smc.population0_4 / smc.population,
      smc.per.population5_9 = smc.population5_9 / smc.population,
      smc.per.population10_14 = smc.population10_14 / smc.population,
      smc.per.population15_19 = smc.population15_19 / smc.population,
      smc.per.population20_24 = smc.population20_24 / smc.population,
      smc.per.population25_29 = smc.population25_29 / smc.population,
      smc.per.population30_34 = smc.population30_34 / smc.population,
      smc.per.population35_39 = smc.population35_39 / smc.population,
      smc.per.population40_44 = smc.population40_44 / smc.population,
      smc.per.population45_49 = smc.population45_49 / smc.population,
      smc.per.population50_54 = smc.population50_54 / smc.population,
      smc.per.population55_59 = smc.population55_59 / smc.population,
      smc.per.population60_64 = smc.population60_64 / smc.population,
      smc.per.population65_69 = smc.population65_69 / smc.population,
      smc.per.population70_74 = smc.population70_74 / smc.population,
      smc.per.population75_ = smc.population75_ / smc.population,
      # 男性年齢別人口割合
      smc.per.population_male0_4 = smc.population_male0_4 / smc.population,
      smc.per.population_male5_9 = smc.population_male5_9 / smc.population,
      smc.per.population_male10_14 = smc.population_male10_14 / smc.population,
      smc.per.population_male15_19 = smc.population_male15_19 / smc.population,
      smc.per.population_male20_24 = smc.population_male20_24 / smc.population,
      smc.per.population_male25_29 = smc.population_male25_29 / smc.population,
      smc.per.population_male30_34 = smc.population_male30_34 / smc.population,
      smc.per.population_male35_39 = smc.population_male35_39 / smc.population,
      smc.per.population_male40_44 = smc.population_male40_44 / smc.population,
      smc.per.population_male45_49 = smc.population_male45_49 / smc.population,
      smc.per.population_male50_54 = smc.population_male50_54 / smc.population,
      smc.per.population_male55_59 = smc.population_male55_59 / smc.population,
      smc.per.population_male60_64 = smc.population_male60_64 / smc.population,
      smc.per.population_male65_69 = smc.population_male65_69 / smc.population,
      smc.per.population_male70_74 = smc.population_male70_74 / smc.population,
      smc.per.population_male75_ = smc.population_male75_ / smc.population,
      # 世帯数割合
      smc.per.households1 = smc.households1 / smc.households,
      smc.per.households2 = smc.households2 / smc.households,
      smc.per.households3 = smc.households3 / smc.households,
      smc.per.households4 = smc.households4 / smc.households,
      smc.per.households5 = smc.households5 / smc.households,
      # 平均世帯人数
      smc.per.population_per_households = smc.population_per_households,
      # 種類別世帯割合
      smc.per.households_relatives = smc.households_relatives / smc.households,
      smc.per.households_core = smc.households_core / smc.households,
      smc.per.households_couple = smc.households_couple / smc.households,
      smc.per.households_couple_children = smc.households_couple_children / smc.households,
      smc.per.households_under_6 = smc.households_under_6 / smc.households,
      smc.per.households_under_18 = smc.households_under_18 / smc.households,
      smc.per.households_over_65 = smc.households_over_65 / smc.households,
      # 住宅種類別世帯割合
      smc.per.households_own = smc.households_own / smc.households,
      smc.per.households_rent = smc.households_rent / smc.households,
      # 住宅の建て方別世帯割合
      smc.per.households_detatched = smc.households_detatched / smc.households,
      smc.per.households_tenement = smc.households_tenement / smc.households,
      smc.per.households_communal_2 = smc.households_communal_2 / smc.households,
      smc.per.households_communal3_5 = smc.households_communal3_5 / smc.households,
      smc.per.households_communal6_10 = smc.households_communal6_10 / smc.households,
      smc.per.households_communal11_ = smc.households_communal11_ / smc.households,
      # 産業別人数
      smc.per.industry_A = smc.industry_A / smc.industry_all,
      smc.per.industry_B = smc.industry_B / smc.industry_all,
      smc.per.industry_C = smc.industry_C / smc.industry_all,
      smc.per.industry_D = smc.industry_D / smc.industry_all,
      smc.per.industry_E = smc.industry_E / smc.industry_all,
      smc.per.industry_F = smc.industry_F / smc.industry_all,
      smc.per.industry_G = smc.industry_G / smc.industry_all,
      smc.per.industry_H = smc.industry_H / smc.industry_all,
      smc.per.industry_I = smc.industry_I / smc.industry_all,
      smc.per.industry_J = smc.industry_J / smc.industry_all,
      smc.per.industry_K = smc.industry_K / smc.industry_all,
      smc.per.industry_L = smc.industry_L / smc.industry_all,
      smc.per.industry_M = smc.industry_M / smc.industry_all,
      smc.per.industry_N = smc.industry_N / smc.industry_all,
      smc.per.industry_O = smc.industry_O / smc.industry_all,
      smc.per.industry_P = smc.industry_P / smc.industry_all,
      smc.per.industry_Q = smc.industry_Q / smc.industry_all,
      smc.per.industry_R = smc.industry_R / smc.industry_all,
      smc.per.industry_S = smc.industry_S / smc.industry_all,
      smc.per.industry_T = smc.industry_T / smc.industry_all,
      smc.per.employer = smc.employer / smc.industry_all,
      smc.per.self_employed = smc.self_employed / smc.industry_all,
      smc.per.family_worker = smc.family_worker / smc.industry_all,
      # 職業別就業者数
      smc.per.sector_A = smc.sector_A / smc.sector_all,
      smc.per.sector_B = smc.sector_B / smc.sector_all,
      smc.per.sector_C = smc.sector_C / smc.sector_all,
      smc.per.sector_D = smc.sector_D / smc.sector_all,
      smc.per.sector_E = smc.sector_E / smc.sector_all,
      smc.per.sector_F = smc.sector_F / smc.sector_all,
      smc.per.sector_G = smc.sector_G / smc.sector_all,
      smc.per.sector_H = smc.sector_H / smc.sector_all,
      smc.per.sector_I = smc.sector_I / smc.sector_all,
      smc.per.sector_J = smc.sector_J / smc.sector_all,
      smc.per.sector_K = smc.sector_K / smc.sector_all,
      smc.per.sector_L = smc.sector_L / smc.sector_all,
      # 経済構成別世帯割合
      smc.per.primary = smc.primary / smc.households,
      smc.per.primary_other = smc.primary_other / smc.households,
      smc.per.not_primary = smc.not_primary / smc.households,
      smc.per.unemployed = smc.unemployed / smc.households
    )
  }
