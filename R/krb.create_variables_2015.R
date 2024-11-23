#' @title to read files
#' @description \code{krb.create_variables_2015}
#' @export

krb.create_variables_2015 <- function(sf_df){
  processed_df <- sf_df %>%
    st_transform(crs = 4326) %>%
    mutate(
      # 面積
      AREA = st_area(.),
      # 人口
      population = as.numeric(T000848001),
      # 人口密度
      population_density = as.numeric(T000848001) / as.numeric(AREA),
      # 男性人口
      population_male = as.numeric(T000848002),
      # 女性人口
      population_female = as.numeric(T000848003),
      # 年齢別人口
      population0_4 = as.numeric(replace(T000849002, T000849002 == "-", 0)),
      population5_10 = as.numeric(replace(T000849003, T000849003 == "-", 0)),
      population11_14 = as.numeric(replace(T000849004, T000849004 == "-", 0)),
      population15_19 = as.numeric(replace(T000849005, T000849005 == "-", 0)),
      population20_24 = as.numeric(replace(T000849006, T000849006 == "-", 0)),
      population25_29 = as.numeric(replace(T000849007, T000849007 == "-", 0)),
      population30_34 = as.numeric(replace(T000849008, T000849008 == "-", 0)),
      population35_39 = as.numeric(replace(T000849009, T000849009 == "-", 0)),
      population40_44 = as.numeric(replace(T000849010, T000849010 == "-", 0)),
      population45_49 = as.numeric(replace(T000849011, T000849011 == "-", 0)),
      population50_54 = as.numeric(replace(T000849012, T000849012 == "-", 0)),
      population55_59 = as.numeric(replace(T000849013, T000849013 == "-", 0)),
      population60_64 = as.numeric(replace(T000849014, T000849014 == "-", 0)),
      population65_69 = as.numeric(replace(T000849015, T000849015 == "-", 0)),
      population70_74 = as.numeric(replace(T000849016, T000849016 == "-", 0)),
      population75_ = as.numeric(replace(T000849020, T000849020 == "-", 0)),
      # 男性年齢別人口
      population_male0_4 = as.numeric(replace(T000849022, T000849022 == "-", 0)),
      population_male5_10 = as.numeric(replace(T000849023, T000849023 == "-", 0)),
      population_male11_14 = as.numeric(replace(T000849024, T000849024 == "-", 0)),
      population_male15_19 = as.numeric(replace(T000849025, T000849025 == "-", 0)),
      population_male20_24 = as.numeric(replace(T000849026, T000849026 == "-", 0)),
      population_male25_29 = as.numeric(replace(T000849027, T000849027 == "-", 0)),
      population_male30_34 = as.numeric(replace(T000849028, T000849028 == "-", 0)),
      population_male35_39 = as.numeric(replace(T000849029, T000849029 == "-", 0)),
      population_male40_44 = as.numeric(replace(T000849030, T000849030 == "-", 0)),
      population_male45_49 = as.numeric(replace(T000849031, T000849031 == "-", 0)),
      population_male50_54 = as.numeric(replace(T000849032, T000849032 == "-", 0)),
      population_male55_59 = as.numeric(replace(T000849033, T000849033 == "-", 0)),
      population_male60_64 = as.numeric(replace(T000849034, T000849034 == "-", 0)),
      population_male65_69 = as.numeric(replace(T000849035, T000849035 == "-", 0)),
      population_male70_74 = as.numeric(replace(T000849036, T000849036 == "-", 0)),
      population_male75_ = as.numeric(replace(T000849040, T000849040 == "-", 0)),
      # 女性年齢別人口
      population_female0_4 = as.numeric(replace(T000849042, T000849042 == "-", 0)),
      population_female5_10 = as.numeric(replace(T000849043, T000849043 == "-", 0)),
      population_female11_14 = as.numeric(replace(T000849044, T000849044 == "-", 0)),
      population_female15_19 = as.numeric(replace(T000849045, T000849045 == "-", 0)),
      population_female20_24 = as.numeric(replace(T000849046, T000849046 == "-", 0)),
      population_female25_29 = as.numeric(replace(T000849047, T000849047 == "-", 0)),
      population_female30_34 = as.numeric(replace(T000849048, T000849048 == "-", 0)),
      population_female35_39 = as.numeric(replace(T000849049, T000849049 == "-", 0)),
      population_female40_44 = as.numeric(replace(T000849050, T000849050 == "-", 0)),
      population_female45_49 = as.numeric(replace(T000849051, T000849051 == "-", 0)),
      population_female50_54 = as.numeric(replace(T000849052, T000849052 == "-", 0)),
      population_female55_59 = as.numeric(replace(T000849053, T000849053 == "-", 0)),
      population_female60_64 = as.numeric(replace(T000849054, T000849054 == "-", 0)),
      population_female65_69 = as.numeric(replace(T000849055, T000849055 == "-", 0)),
      population_female70_74 = as.numeric(replace(T000849056, T000849056 == "-", 0)),
      population_female75_ = as.numeric(replace(T000849060, T000849060 == "-", 0)),
      # 世帯数に関して
      households = as.numeric(replace(T000850001, T000850001 == "-", 0)),
      householda1 = as.numeric(replace(T000850002, T000850002 == "-", 0)),
      householda2 = as.numeric(replace(T000850003, T000850003 == "-", 0)),
      householda3 = as.numeric(replace(T000850004, T000850004 == "-", 0)),
      householda4 = as.numeric(replace(T000850005, T000850005 == "-", 0)),
      householda5 = as.numeric(replace(T000850006, T000850006 == "-", 0)),
      # 平均世帯人数
      population_per_householda = as.numeric(replace(T000850008, T000850008 == "-", 0)),
      # 種類別世帯数
      households_relatives = as.numeric(replace(T000851002, T000851002 == "-", 0)),
      households_core = as.numeric(replace(T000851003, T000851003 == "-", 0)),
      households_couple = as.numeric(replace(T000851004, T000851004 == "-", 0)),
      households_couple_children = as.numeric(replace(T000851005, T000851005 == "-", 0)),
      households_under_6 = as.numeric(replace(T000851007, T000851007 == "-", 0)),
      households_under_18 = as.numeric(replace(T000851008, T000851008 == "-", 0)),
      households_over_65 = as.numeric(replace(T000851009, T000851009 == "-", 0)),
      # 住宅種類別世帯
      households_own = as.numeric(replace(T000852002, T000852002 == "-", 0)),
      households_rent = as.numeric(replace(T000852003, T000852003 == "-", 0)),
      # 住宅の建て方別世帯
      households_detatched = as.numeric(replace(T000853002, T000853002 == "-", 0)),
      households_tenement = as.numeric(replace(T000853003, T000853003 == "-", 0)),
      households_communal_2 = as.numeric(replace(T000853005, T000853005 == "-", 0)),
      households_communal3_5 = as.numeric(replace(T000853006, T000853006 == "-", 0)),
      households_communal6_11 = as.numeric(replace(T000853007, T000853007 == "-", 0)),
      households_communal12_ = as.numeric(replace(T000853008, T000853008 == "-", 0)),
      # 産業別人数
      industry_A = as.numeric(replace(T000865002, T000865002 == "-", 0)),
      industry_B = as.numeric(replace(T000865004, T000865004 == "-", 0)),
      industry_C = as.numeric(replace(T000865005, T000865005 == "-", 0)),
      industry_D = as.numeric(replace(T000865006, T000865006 == "-", 0)),
      industry_E = as.numeric(replace(T000865007, T000865007 == "-", 0)),
      industry_F = as.numeric(replace(T000865008, T000865008 == "-", 0)),
      industry_G = as.numeric(replace(T000865009, T000865009 == "-", 0)),
      industry_H = as.numeric(replace(T000865010, T000865010 == "-", 0)),
      industry_I = as.numeric(replace(T000865011, T000865011 == "-", 0)),
      industry_J = as.numeric(replace(T000865012, T000865012 == "-", 0)),
      industry_K = as.numeric(replace(T000865013, T000865013 == "-", 0)),
      industry_L = as.numeric(replace(T000865014, T000865014 == "-", 0)),
      industry_M = as.numeric(replace(T000865015, T000865015 == "-", 0)),
      industry_N = as.numeric(replace(T000865016, T000865016 == "-", 0)),
      industry_O = as.numeric(replace(T000865017, T000865017 == "-", 0)),
      industry_P = as.numeric(replace(T000865018, T000865018 == "-", 0)),
      industry_Q = as.numeric(replace(T000865019, T000865019 == "-", 0)),
      industry_R = as.numeric(replace(T000865020, T000865020 == "-", 0)),
      industry_S = as.numeric(replace(T000865021, T000865021 == "-", 0)),
      industry_T = as.numeric(replace(T000865022, T000865022 == "-", 0)),
      employer = as.numeric(replace(T000865023, T000865023 == "-", 0)),
      self_employed = as.numeric(replace(T000865024, T000865024 == "-", 0)),
      family_worker = as.numeric(replace(T000865025, T000865025 == "-", 0)),
      # 職業別就業者数
      sector_A = as.numeric(replace(T000866002, T000866002 == "-", 0)),
      sector_B = as.numeric(replace(T000866003, T000866003 == "-", 0)),
      sector_C = as.numeric(replace(T000866004, T000866004 == "-", 0)),
      sector_D = as.numeric(replace(T000866005, T000866005 == "-", 0)),
      sector_E = as.numeric(replace(T000866006, T000866006 == "-", 0)),
      sector_F = as.numeric(replace(T000866007, T000866007 == "-", 0)),
      sector_G = as.numeric(replace(T000866008, T000866008 == "-", 0)),
      sector_H = as.numeric(replace(T000866009, T000866009 == "-", 0)),
      sector_I = as.numeric(replace(T000866010, T000866010 == "-", 0)),
      sector_J = as.numeric(replace(T000866011, T000866011 == "-", 0)),
      sector_K = as.numeric(replace(T000866012, T000866012 == "-", 0)),
      sector_L = as.numeric(replace(T000866013, T000866013 == "-", 0)),
      # 経済構成別世帯
      primary = as.numeric(replace(T000875002, T000875002 == "-", 0)),
      primary_other = as.numeric(replace(T000875003, T000875003 == "-", 0)),
      not_primary = as.numeric(replace(T000875004, T000875004 == "-", 0)),
      unemployed = as.numeric(replace(T000875005, T000875005 == "-", 0)),
    )
  return(processed_df)
}

