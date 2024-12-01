#' @title to read files
#' @description \code{smc.create_variable_2020}

smc.create_variables_2020 <- function(sf_df){
  # 面積を計算できない地域をデータフレームから除外する機構を実装
  invalids <- sf_df[!st_is_valid(sf_df), ]
  KEY_CODE_to_eliminate <- invalids$KEY_CODE %>% as_vector()
  sf_df <- sf_df %>%
    filter(!KEY_CODE %in% KEY_CODE_to_eliminate)
  
  # ここで特徴量を作成
  processed_df <- sf_df %>%
    st_transform(crs = 4326) %>%
    mutate(
      # 面積
      smc.AREA = st_area(.),
      # 人口
      smc.population = as.numeric(T001081001),
      # 人口密度
      smc.population_density = as.numeric(T001081001) / as.numeric(smc.AREA),
      # 男性人口
      smc.population_male = as.numeric(T001081002),
      # 女性人口
      smc.population_female = as.numeric(T001081003),
      # 年齢別人口
      smc.population0_4 = as.numeric(replace(T001082002, T001082002 == "-", 0)),
      smc.population5_9 = as.numeric(replace(T001082003, T001082003 == "-", 0)),
      smc.population10_14 = as.numeric(replace(T001082004, T001082004 == "-", 0)),
      smc.population15_19 = as.numeric(replace(T001082005, T001082005 == "-", 0)),
      smc.population20_24 = as.numeric(replace(T001082006, T001082006 == "-", 0)),
      smc.population25_29 = as.numeric(replace(T001082007, T001082007 == "-", 0)),
      smc.population30_34 = as.numeric(replace(T001082008, T001082008 == "-", 0)),
      smc.population35_39 = as.numeric(replace(T001082009, T001082009 == "-", 0)),
      smc.population40_44 = as.numeric(replace(T001082010, T001082010 == "-", 0)),
      smc.population45_49 = as.numeric(replace(T001082011, T001082011 == "-", 0)),
      smc.population50_54 = as.numeric(replace(T001082012, T001082012 == "-", 0)),
      smc.population55_59 = as.numeric(replace(T001082013, T001082013 == "-", 0)),
      smc.population60_64 = as.numeric(replace(T001082014, T001082014 == "-", 0)),
      smc.population65_69 = as.numeric(replace(T001082015, T001082015 == "-", 0)),
      smc.population70_74 = as.numeric(replace(T001082016, T001082016 == "-", 0)),
      smc.population75_ = as.numeric(replace(T001082020, T001082020 == "-", 0)),
      # 男性年齢別人口
      smc.population_male0_4 = as.numeric(replace(T001082022, T001082022 == "-", 0)),
      smc.population_male5_9 = as.numeric(replace(T001082023, T001082023 == "-", 0)),
      smc.population_male10_14 = as.numeric(replace(T001082024, T001082024 == "-", 0)),
      smc.population_male15_19 = as.numeric(replace(T001082025, T001082025 == "-", 0)),
      smc.population_male20_24 = as.numeric(replace(T001082026, T001082026 == "-", 0)),
      smc.population_male25_29 = as.numeric(replace(T001082027, T001082027 == "-", 0)),
      smc.population_male30_34 = as.numeric(replace(T001082028, T001082028 == "-", 0)),
      smc.population_male35_39 = as.numeric(replace(T001082029, T001082029 == "-", 0)),
      smc.population_male40_44 = as.numeric(replace(T001082030, T001082030 == "-", 0)),
      smc.population_male45_49 = as.numeric(replace(T001082031, T001082031 == "-", 0)),
      smc.population_male50_54 = as.numeric(replace(T001082032, T001082032 == "-", 0)),
      smc.population_male55_59 = as.numeric(replace(T001082033, T001082033 == "-", 0)),
      smc.population_male60_64 = as.numeric(replace(T001082034, T001082034 == "-", 0)),
      smc.population_male65_69 = as.numeric(replace(T001082035, T001082035 == "-", 0)),
      smc.population_male70_74 = as.numeric(replace(T001082036, T001082036 == "-", 0)),
      smc.population_male75_ = as.numeric(replace(T001082040, T001082040 == "-", 0)),
      # 女性年齢別人口
      smc.population_female0_4 = as.numeric(replace(T001082042, T001082042 == "-", 0)),
      smc.population_female5_9 = as.numeric(replace(T001082043, T001082043 == "-", 0)),
      smc.population_female10_14 = as.numeric(replace(T001082044, T001082044 == "-", 0)),
      smc.population_female15_19 = as.numeric(replace(T001082045, T001082045 == "-", 0)),
      smc.population_female20_24 = as.numeric(replace(T001082046, T001082046 == "-", 0)),
      smc.population_female25_29 = as.numeric(replace(T001082047, T001082047 == "-", 0)),
      smc.population_female30_34 = as.numeric(replace(T001082048, T001082048 == "-", 0)),
      smc.population_female35_39 = as.numeric(replace(T001082049, T001082049 == "-", 0)),
      smc.population_female40_44 = as.numeric(replace(T001082050, T001082050 == "-", 0)),
      smc.population_female45_49 = as.numeric(replace(T001082051, T001082051 == "-", 0)),
      smc.population_female50_54 = as.numeric(replace(T001082052, T001082052 == "-", 0)),
      smc.population_female55_59 = as.numeric(replace(T001082053, T001082053 == "-", 0)),
      smc.population_female60_64 = as.numeric(replace(T001082054, T001082054 == "-", 0)),
      smc.population_female65_69 = as.numeric(replace(T001082055, T001082055 == "-", 0)),
      smc.population_female70_74 = as.numeric(replace(T001082056, T001082056 == "-", 0)),
      smc.population_female75_ = as.numeric(replace(T001082060, T001082060 == "-", 0)),
      # 世帯数に関して
      smc.households = as.numeric(replace(T001083001, T001083001 == "-", 0)),
      smc.households1 = as.numeric(replace(T001083002, T001083002 == "-", 0)),
      smc.households2 = as.numeric(replace(T001083003, T001083003 == "-", 0)),
      smc.households3 = as.numeric(replace(T001083004, T001083004 == "-", 0)),
      smc.households4 = as.numeric(replace(T001083005, T001083005 == "-", 0)),
      smc.households5 = as.numeric(replace(T001083006, T001083006 == "-", 0)),
      # 平均世帯人数
      smc.population_per_households = as.numeric(replace(T001083008, T001083008 == "-", 0)),
      # 種類別世帯数
      smc.households_relatives = as.numeric(replace(T001084002, T001084002 == "-", 0)),
      smc.households_core = as.numeric(replace(T001084003, T001084003 == "-", 0)),
      smc.households_couple = as.numeric(replace(T001084004, T001084004 == "-", 0)),
      smc.households_couple_children = as.numeric(replace(T001084005, T001084005 == "-", 0)),
      smc.households_under_6 = as.numeric(replace(T001084007, T001084007 == "-", 0)),
      smc.households_under_18 = as.numeric(replace(T001084008, T001084008 == "-", 0)),
      smc.households_over_65 = as.numeric(replace(T001084009, T001084009 == "-", 0)),
      # 住宅種類別世帯
      smc.households_own = as.numeric(replace(T001085002, T001085002 == "-", 0)),
      smc.households_rent = as.numeric(replace(T001085003, T001085003 == "-", 0)),
      # 住宅の建て方別世帯
      smc.households_detatched = as.numeric(replace(T001086002, T001086002 == "-", 0)),
      smc.households_tenement = as.numeric(replace(T001086003, T001086003 == "-", 0)),
      smc.households_communal_2 = as.numeric(replace(T001086005, T001086005 == "-", 0)),
      smc.households_communal3_5 = as.numeric(replace(T001086006, T001086006 == "-", 0)),
      smc.households_communal6_10 = as.numeric(replace(T001086007, T001086007 == "-", 0)),
      smc.households_communal11_ = as.numeric(replace(T001086008, T001086008 == "-", 0)),
      # 産業別人数
      smc.industry_all = as.numeric(replace(T001103001, T001103001 == "-", 0)),
      smc.industry_A = as.numeric(replace(T001103002, T001103002 == "-", 0)),
      smc.industry_B = as.numeric(replace(T001103004, T001103004 == "-", 0)),
      smc.industry_C = as.numeric(replace(T001103005, T001103005 == "-", 0)),
      smc.industry_D = as.numeric(replace(T001103006, T001103006 == "-", 0)),
      smc.industry_E = as.numeric(replace(T001103007, T001103007 == "-", 0)),
      smc.industry_F = as.numeric(replace(T001103008, T001103008 == "-", 0)),
      smc.industry_G = as.numeric(replace(T001103009, T001103009 == "-", 0)),
      smc.industry_H = as.numeric(replace(T001103010, T001103010 == "-", 0)),
      smc.industry_I = as.numeric(replace(T001103011, T001103011 == "-", 0)),
      smc.industry_J = as.numeric(replace(T001103012, T001103012 == "-", 0)),
      smc.industry_K = as.numeric(replace(T001103013, T001103013 == "-", 0)),
      smc.industry_L = as.numeric(replace(T001103014, T001103014 == "-", 0)),
      smc.industry_M = as.numeric(replace(T001103015, T001103015 == "-", 0)),
      smc.industry_N = as.numeric(replace(T001103016, T001103016 == "-", 0)),
      smc.industry_O = as.numeric(replace(T001103017, T001103017 == "-", 0)),
      smc.industry_P = as.numeric(replace(T001103018, T001103018 == "-", 0)),
      smc.industry_Q = as.numeric(replace(T001103019, T001103019 == "-", 0)),
      smc.industry_R = as.numeric(replace(T001103020, T001103020 == "-", 0)),
      smc.industry_S = as.numeric(replace(T001103021, T001103021 == "-", 0)),
      smc.industry_T = as.numeric(replace(T001103022, T001103022 == "-", 0)),
      smc.employer = as.numeric(replace(T001103024, T001103024 == "-", 0)),
      smc.self_employed = as.numeric(replace(T001103025, T001103025 == "-", 0)),
      smc.family_worker = as.numeric(replace(T001103026, T001103026 == "-", 0)),
      # 職業別就業者数
      smc.sector_all = as.numeric(replace(T001104001, T001104001 == "-", 0)),
      smc.sector_A = as.numeric(replace(T001104002, T001104002 == "-", 0)),
      smc.sector_B = as.numeric(replace(T001104003, T001104003 == "-", 0)),
      smc.sector_C = as.numeric(replace(T001104004, T001104004 == "-", 0)),
      smc.sector_D = as.numeric(replace(T001104005, T001104005 == "-", 0)),
      smc.sector_E = as.numeric(replace(T001104006, T001104006 == "-", 0)),
      smc.sector_F = as.numeric(replace(T001104007, T001104007 == "-", 0)),
      smc.sector_G = as.numeric(replace(T001104008, T001104008 == "-", 0)),
      smc.sector_H = as.numeric(replace(T001104009, T001104009 == "-", 0)),
      smc.sector_I = as.numeric(replace(T001104010, T001104010 == "-", 0)),
      smc.sector_J = as.numeric(replace(T001104011, T001104011 == "-", 0)),
      smc.sector_K = as.numeric(replace(T001104012, T001104012 == "-", 0)),
      smc.sector_L = as.numeric(replace(T001104013, T001104013 == "-", 0)),
      # 経済構成別世帯
      smc.primary = as.numeric(replace(T001106002, T001106002 == "-", 0)),
      smc.primary_other = as.numeric(replace(T001106003, T001106003 == "-", 0)),
      smc.not_primary = as.numeric(replace(T001106004, T001106004 == "-", 0)),
      smc.unemployed = as.numeric(replace(T001106005, T001106005 == "-", 0))
    )
  return(processed_df)
}
