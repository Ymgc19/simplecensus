#' @title to read files
#' @description \code{smc.create_variables_2015}


smc.create_variables_2015 <- function(sf_df){
  # 面積を計算できない地域をデータフレームから除外する機構を実装
#  invalids <- sf_df[!st_is_valid(sf_df), ]
#  KEY_CODE_to_eliminate <- invalids$KEY_CODE %>% as_vector()
#  sf_df <- sf_df %>%
#    filter(!KEY_CODE %in% KEY_CODE_to_eliminate)

  # ここで特徴量を作成
  processed_df <- sf_df %>%
    st_transform(crs = 4326) %>%
    mutate(
      # 面積
#      smc.AREA = st_area(.),
      # 人口
      smc.population = as.numeric(T000848001),
      # 人口密度
#      smc.population_density = as.numeric(T000848001) / as.numeric(smc.AREA),
      # 男性人口
      smc.population_male = as.numeric(T000848002),
      # 女性人口
      smc.population_female = as.numeric(T000848003),
      # 年齢別人口
      smc.population0_4 = as.numeric(replace(T000849002, T000849002 == "-", 0)),
      smc.population5_9 = as.numeric(replace(T000849003, T000849003 == "-", 0)),
      smc.population10_14 = as.numeric(replace(T000849004, T000849004 == "-", 0)),
      smc.population15_19 = as.numeric(replace(T000849005, T000849005 == "-", 0)),
      smc.population20_24 = as.numeric(replace(T000849006, T000849006 == "-", 0)),
      smc.population25_29 = as.numeric(replace(T000849007, T000849007 == "-", 0)),
      smc.population30_34 = as.numeric(replace(T000849008, T000849008 == "-", 0)),
      smc.population35_39 = as.numeric(replace(T000849009, T000849009 == "-", 0)),
      smc.population40_44 = as.numeric(replace(T000849010, T000849010 == "-", 0)),
      smc.population45_49 = as.numeric(replace(T000849011, T000849011 == "-", 0)),
      smc.population50_54 = as.numeric(replace(T000849012, T000849012 == "-", 0)),
      smc.population55_59 = as.numeric(replace(T000849013, T000849013 == "-", 0)),
      smc.population60_64 = as.numeric(replace(T000849014, T000849014 == "-", 0)),
      smc.population65_69 = as.numeric(replace(T000849015, T000849015 == "-", 0)),
      smc.population70_74 = as.numeric(replace(T000849016, T000849016 == "-", 0)),
      smc.population75_ = as.numeric(replace(T000849020, T000849020 == "-", 0)),
      # 男性年齢別人口
      smc.population_male0_4 = as.numeric(replace(T000849022, T000849022 == "-", 0)),
      smc.population_male5_9 = as.numeric(replace(T000849023, T000849023 == "-", 0)),
      smc.population_male10_14 = as.numeric(replace(T000849024, T000849024 == "-", 0)),
      smc.population_male15_19 = as.numeric(replace(T000849025, T000849025 == "-", 0)),
      smc.population_male20_24 = as.numeric(replace(T000849026, T000849026 == "-", 0)),
      smc.population_male25_29 = as.numeric(replace(T000849027, T000849027 == "-", 0)),
      smc.population_male30_34 = as.numeric(replace(T000849028, T000849028 == "-", 0)),
      smc.population_male35_39 = as.numeric(replace(T000849029, T000849029 == "-", 0)),
      smc.population_male40_44 = as.numeric(replace(T000849030, T000849030 == "-", 0)),
      smc.population_male45_49 = as.numeric(replace(T000849031, T000849031 == "-", 0)),
      smc.population_male50_54 = as.numeric(replace(T000849032, T000849032 == "-", 0)),
      smc.population_male55_59 = as.numeric(replace(T000849033, T000849033 == "-", 0)),
      smc.population_male60_64 = as.numeric(replace(T000849034, T000849034 == "-", 0)),
      smc.population_male65_69 = as.numeric(replace(T000849035, T000849035 == "-", 0)),
      smc.population_male70_74 = as.numeric(replace(T000849036, T000849036 == "-", 0)),
      smc.population_male75_ = as.numeric(replace(T000849040, T000849040 == "-", 0)),
      # 女性年齢別人口
      smc.population_female0_4 = as.numeric(replace(T000849042, T000849042 == "-", 0)),
      smc.population_female5_9 = as.numeric(replace(T000849043, T000849043 == "-", 0)),
      smc.population_female10_14 = as.numeric(replace(T000849044, T000849044 == "-", 0)),
      smc.population_female15_19 = as.numeric(replace(T000849045, T000849045 == "-", 0)),
      smc.population_female20_24 = as.numeric(replace(T000849046, T000849046 == "-", 0)),
      smc.population_female25_29 = as.numeric(replace(T000849047, T000849047 == "-", 0)),
      smc.population_female30_34 = as.numeric(replace(T000849048, T000849048 == "-", 0)),
      smc.population_female35_39 = as.numeric(replace(T000849049, T000849049 == "-", 0)),
      smc.population_female40_44 = as.numeric(replace(T000849050, T000849050 == "-", 0)),
      smc.population_female45_49 = as.numeric(replace(T000849051, T000849051 == "-", 0)),
      smc.population_female50_54 = as.numeric(replace(T000849052, T000849052 == "-", 0)),
      smc.population_female55_59 = as.numeric(replace(T000849053, T000849053 == "-", 0)),
      smc.population_female60_64 = as.numeric(replace(T000849054, T000849054 == "-", 0)),
      smc.population_female65_69 = as.numeric(replace(T000849055, T000849055 == "-", 0)),
      smc.population_female70_74 = as.numeric(replace(T000849056, T000849056 == "-", 0)),
      smc.population_female75_ = as.numeric(replace(T000849060, T000849060 == "-", 0)),
      # 世帯数に関して
      smc.households = as.numeric(replace(T000850001, T000850001 == "-", 0)),
      smc.households1 = as.numeric(replace(T000850002, T000850002 == "-", 0)),
      smc.households2 = as.numeric(replace(T000850003, T000850003 == "-", 0)),
      smc.households3 = as.numeric(replace(T000850004, T000850004 == "-", 0)),
      smc.households4 = as.numeric(replace(T000850005, T000850005 == "-", 0)),
      smc.households5 = as.numeric(replace(T000850006, T000850006 == "-", 0)),
      # 平均世帯人数
      smc.population_per_households = as.numeric(replace(T000850008, T000850008 == "-", 0)),
      # 種類別世帯数
      smc.households_relatives = as.numeric(replace(T000851002, T000851002 == "-", 0)),
      smc.households_core = as.numeric(replace(T000851003, T000851003 == "-", 0)),
      smc.households_couple = as.numeric(replace(T000851004, T000851004 == "-", 0)),
      smc.households_couple_children = as.numeric(replace(T000851005, T000851005 == "-", 0)),
      smc.households_under_6 = as.numeric(replace(T000851007, T000851007 == "-", 0)),
      smc.households_under_18 = as.numeric(replace(T000851008, T000851008 == "-", 0)),
      smc.households_over_65 = as.numeric(replace(T000851009, T000851009 == "-", 0)),
      # 住宅種類別世帯
      smc.households_own = as.numeric(replace(T000852002, T000852002 == "-", 0)),
      smc.households_rent = as.numeric(replace(T000852003, T000852003 == "-", 0)),
      # 住宅の建て方別世帯
      smc.households_detatched = as.numeric(replace(T000853002, T000853002 == "-", 0)),
      smc.households_tenement = as.numeric(replace(T000853003, T000853003 == "-", 0)),
      smc.households_communal_2 = as.numeric(replace(T000853005, T000853005 == "-", 0)),
      smc.households_communal3_5 = as.numeric(replace(T000853006, T000853006 == "-", 0)),
      smc.households_communal6_10 = as.numeric(replace(T000853007, T000853007 == "-", 0)),
      smc.households_communal11_ = as.numeric(replace(T000853008, T000853008 == "-", 0)),
      # 産業別人数
      smc.industry_all = as.numeric(replace(T000865001, T000865001 == "-", 0)),
      smc.industry_A = as.numeric(replace(T000865002, T000865002 == "-", 0)),
      smc.industry_B = as.numeric(replace(T000865004, T000865004 == "-", 0)),
      smc.industry_C = as.numeric(replace(T000865005, T000865005 == "-", 0)),
      smc.industry_D = as.numeric(replace(T000865006, T000865006 == "-", 0)),
      smc.industry_E = as.numeric(replace(T000865007, T000865007 == "-", 0)),
      smc.industry_F = as.numeric(replace(T000865008, T000865008 == "-", 0)),
      smc.industry_G = as.numeric(replace(T000865009, T000865009 == "-", 0)),
      smc.industry_H = as.numeric(replace(T000865010, T000865010 == "-", 0)),
      smc.industry_I = as.numeric(replace(T000865011, T000865011 == "-", 0)),
      smc.industry_J = as.numeric(replace(T000865012, T000865012 == "-", 0)),
      smc.industry_K = as.numeric(replace(T000865013, T000865013 == "-", 0)),
      smc.industry_L = as.numeric(replace(T000865014, T000865014 == "-", 0)),
      smc.industry_M = as.numeric(replace(T000865015, T000865015 == "-", 0)),
      smc.industry_N = as.numeric(replace(T000865016, T000865016 == "-", 0)),
      smc.industry_O = as.numeric(replace(T000865017, T000865017 == "-", 0)),
      smc.industry_P = as.numeric(replace(T000865018, T000865018 == "-", 0)),
      smc.industry_Q = as.numeric(replace(T000865019, T000865019 == "-", 0)),
      smc.industry_R = as.numeric(replace(T000865020, T000865020 == "-", 0)),
      smc.industry_S = as.numeric(replace(T000865021, T000865021 == "-", 0)),
      smc.industry_T = as.numeric(replace(T000865022, T000865022 == "-", 0)),
      smc.employer = as.numeric(replace(T000865023, T000865023 == "-", 0)),
      smc.self_employed = as.numeric(replace(T000865024, T000865024 == "-", 0)),
      smc.family_worker = as.numeric(replace(T000865025, T000865025 == "-", 0)),
      # 職業別就業者数
      smc.sector_all = as.numeric(replace(T000866001, T000866001 == "-", 0)),
      smc.sector_A = as.numeric(replace(T000866002, T000866002 == "-", 0)),
      smc.sector_B = as.numeric(replace(T000866003, T000866003 == "-", 0)),
      smc.sector_C = as.numeric(replace(T000866004, T000866004 == "-", 0)),
      smc.sector_D = as.numeric(replace(T000866005, T000866005 == "-", 0)),
      smc.sector_E = as.numeric(replace(T000866006, T000866006 == "-", 0)),
      smc.sector_F = as.numeric(replace(T000866007, T000866007 == "-", 0)),
      smc.sector_G = as.numeric(replace(T000866008, T000866008 == "-", 0)),
      smc.sector_H = as.numeric(replace(T000866009, T000866009 == "-", 0)),
      smc.sector_I = as.numeric(replace(T000866010, T000866010 == "-", 0)),
      smc.sector_J = as.numeric(replace(T000866011, T000866011 == "-", 0)),
      smc.sector_K = as.numeric(replace(T000866012, T000866012 == "-", 0)),
      smc.sector_L = as.numeric(replace(T000866013, T000866013 == "-", 0)),
      # 経済構成別世帯
      smc.primary = as.numeric(replace(T000875002, T000875002 == "-", 0)),
      smc.primary_other = as.numeric(replace(T000875003, T000875003 == "-", 0)),
      smc.not_primary = as.numeric(replace(T000875004, T000875004 == "-", 0)),
      smc.unemployed = as.numeric(replace(T000875005, T000875005 == "-", 0))
    )
  return(processed_df)
}

