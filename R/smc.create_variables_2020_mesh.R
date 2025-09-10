#' @title to read files
#' @description \code{smc.create_variables_2020_mesh}

smc.create_variables_2020_mesh <- function(sf_df){
  mutate(
    # 人口統計に関するもの
    # 人口情報
    msh.per.male = as.numeric(T001102002) / as.numeric(T001102001), # 男性割合
    msh.per.female = as.numeric(T001102003) / as.numeric(T001102001), # 女性割合
    
    msh.per.0_14 = as.numeric(T001102004) / as.numeric(T001102001), # 14歳以下割合
    msh.per.0_14_male = as.numeric(T001102005) / as.numeric(T001102001), # 14歳以下割合 男性
    msh.per.0_14_female = as.numeric(T001102006) / as.numeric(T001102001), # 14歳以下割合 女性
    
    msh.per.15_ = as.numeric(T001102007) / as.numeric(T001102001), # 15歳以上割合
    msh.per.15_male = as.numeric(T001102008) / as.numeric(T001102001), # 15歳以上割合 男性
    msh.per.15_female = as.numeric(T001102009) / as.numeric(T001102001), # 15歳以上割合 女性
    
    msh.per.15_64 = as.numeric(T001102010) / as.numeric(T001102001), # 15歳以上64歳以下割合 
    msh.per.15_64_male = as.numeric(T001102011) / as.numeric(T001102001), # 15歳以上64歳以下割合 男性
    msh.per.15_64_female = as.numeric(T001102012) / as.numeric(T001102001), # 15歳以上64歳以下割合 女性
    
    msh.per.18 = as.numeric(T001102013) / as.numeric(T001102001), # 18歳以上割合
    msh.per.18_male = as.numeric(T001102014) / as.numeric(T001102001),
    msh.per.18_female = as.numeric(T001102015) / as.numeric(T001102001),
    
    msh.per.20 = as.numeric(T001102016) / as.numeric(T001102001), # 18歳以上割合
    msh.per.20_male = as.numeric(T001102017) / as.numeric(T001102001),
    msh.per.20_female = as.numeric(T001102018) / as.numeric(T001102001),
    
    msh.per.65 = as.numeric(T001102019) / as.numeric(T001102001), # 65歳以上割合
    msh.per.65_male = as.numeric(T001102020) / as.numeric(T001102001),
    msh.per.65_female = as.numeric(T001102021) / as.numeric(T001102001),
    
    msh.per.75 = as.numeric(T001102022) / as.numeric(T001102001), # 75歳以上割合
    msh.per.75_male = as.numeric(T001102023) / as.numeric(T001102001),
    msh.per.75_female = as.numeric(T001102024) / as.numeric(T001102001),
    
    msh.per.85 = as.numeric(T001102025) / as.numeric(T001102001), # 85歳以上割合
    msh.per.85_male = as.numeric(T001102026) / as.numeric(T001102001),
    msh.per.85_female = as.numeric(T001102027) / as.numeric(T001102001),
    
    msh.per.95 = as.numeric(T001102028) / as.numeric(T001102001), # 95歳以上割合
    msh.per.95_male = as.numeric(T001102029) / as.numeric(T001102001),
    msh.per.95_female = as.numeric(T001102030) / as.numeric(T001102001),
    
    # 外国人
    msh.per.foreigner = as.numeric(T001102031) / as.numeric(T001102001), # 外国人割合
    msh.per.foreigner_male = as.numeric(T001102032) / as.numeric(T001102001), # 外国人割合 男性
    msh.per.foreigner_female = as.numeric(T001102033) / as.numeric(T001102001), # 外国人割合 女性
    
    # 世帯
    msh.per.house1 = as.numeric(T001102036) / as.numeric(T001102034),
    msh.per.house2 = as.numeric(T001102037) / as.numeric(T001102034),
    msh.per.house3 = as.numeric(T001102038) / as.numeric(T001102034),
    msh.per.house4 = as.numeric(T001102039) / as.numeric(T001102034),
    msh.per.house5 = as.numeric(T001102040) / as.numeric(T001102034),
    msh.per.house6 = as.numeric(T001102041) / as.numeric(T001102034),
    msh.per.house7 = as.numeric(T001102042) / as.numeric(T001102034),
    
    msh.per.house_relative = as.numeric(T001102043) / as.numeric(T001102034),
    msh.per.house_core = as.numeric(T001102044) / as.numeric(T001102034),
    msh.per.house_not_core = as.numeric(T001102045) / as.numeric(T001102034),
    msh.per.house_6 = as.numeric(T001102046) / as.numeric(T001102034),
    msh.per.house_65 = as.numeric(T001102047) / as.numeric(T001102034),
    msh.per.house_alone_young = as.numeric(T001102048) / as.numeric(T001102034), # 20代の単身世帯割合
    msh.per.house_alone_old = as.numeric(T001102049) / as.numeric(T001102034), # 高齢単身世帯割合
    msh.per.house_couple_old = as.numeric(T001102050) / as.numeric(T001102034), # 高齢夫婦世帯割合
    # 雇用情報
    # 分母は15歳以上人口
    msh.per.employer = as.numeric(T001109001) / as.numeric(T001102007), # 雇用者割合
    msh.per.employer_male = as.numeric(T001109002) / as.numeric(T001102007), 
    msh.per.employer_female = as.numeric(T001109003) / as.numeric(T001102007), 
    msh.per.regular_employee = as.numeric(T001109004) / as.numeric(T001102007), # 正規職員
    msh.per.regular_employee_male = as.numeric(T001109005) / as.numeric(T001102007),
    msh.per.regular_employee_female = as.numeric(T001109006) / as.numeric(T001102007),
    msh.per.informal_employee = as.numeric(T001109007) / as.numeric(T001102007), # 労働者派遣事業所の派遣社員
    msh.per.informal_employee_male = as.numeric(T001109008) / as.numeric(T001102007),
    msh.per.informal_employee_female = as.numeric(T001109009) / as.numeric(T001102007),
    msh.per.part_employee = as.numeric(T001109010) / as.numeric(T001102007), # パート・アルバイト
    msh.per.part_employee_male = as.numeric(T001109011) / as.numeric(T001102007),
    msh.per.part_employee_female = as.numeric(T001109012) / as.numeric(T001102007),
    msh.per.self_employee = as.numeric(T001109013) / as.numeric(T001102007), # 自営
    msh.per.self_employee_male = as.numeric(T001109014) / as.numeric(T001102007),
    msh.per.self_employee_female = as.numeric(T001109015) / as.numeric(T001102007),
    msh.per.family_employee = as.numeric(T001109016) / as.numeric(T001102007), # 家族従業者
    msh.per.family_employee_male = as.numeric(T001109017) / as.numeric(T001102007),
    msh.per.family_employee_female = as.numeric(T001109018) / as.numeric(T001102007),
    
    # 就学状況
    msh.per.before_school = as.numeric(T001109019) / as.numeric(T001102001), # 未就学
    msh.per.before_school_male = as.numeric(T001109020) / as.numeric(T001102001),
    msh.per.before_school_female = as.numeric(T001109021) / as.numeric(T001102001),
    msh.per.before_school_youchien = as.numeric(T001109022) / as.numeric(T001102001), # 未就学 幼稚園
    msh.per.before_school_youchien_male = as.numeric(T001109023) / as.numeric(T001102001),
    msh.per.before_school_youchien_female = as.numeric(T001109024) / as.numeric(T001102001),
    msh.per.before_school_hoikuen = as.numeric(T001109025) / as.numeric(T001102001), # 未就学 保育園
    msh.per.before_school_hoikuen_male = as.numeric(T001109026) / as.numeric(T001102001),
    msh.per.before_school_hoikuen_female = as.numeric(T001109027) / as.numeric(T001102001),
    msh.per.before_school_nintei = as.numeric(T001109028) / as.numeric(T001102001), # 未就学 認定こども園
    msh.per.before_school_nintei_male = as.numeric(T001109029) / as.numeric(T001102001),
    msh.per.before_school_nintei_female = as.numeric(T001109030) / as.numeric(T001102001),
    msh.per.before_school_other = as.numeric(T001109031) / as.numeric(T001102001), # 未就学 その他
    msh.per.before_school_other_male = as.numeric(T001109032) / as.numeric(T001102001),
    msh.per.before_school_other_female = as.numeric(T001109033) / as.numeric(T001102001),
    msh.per.school = as.numeric(T001109034) / as.numeric(T001102001), # 在学者
    msh.per.school_male = as.numeric(T001109035) / as.numeric(T001102001), 
    msh.per.school_female = as.numeric(T001109036) / as.numeric(T001102001), 
    msh.per.school_compulsory = as.numeric(T001109037) / as.numeric(T001102001), # 在学者 義務教育
    msh.per.school_compulsory_male = as.numeric(T001109038) / as.numeric(T001102001), 
    msh.per.school_compulsory_female = as.numeric(T001109039) / as.numeric(T001102001), 
    msh.per.school_high = as.numeric(T001109040) / as.numeric(T001102001), # 在学者 高校
    msh.per.school_high_male = as.numeric(T001109041) / as.numeric(T001102001), 
    msh.per.school_high_female = as.numeric(T001109042) / as.numeric(T001102001), 
    msh.per.school_j_college = as.numeric(T001109043) / as.numeric(T001102001), # 在学者 短大高専
    msh.per.school_j_college_male = as.numeric(T001109044) / as.numeric(T001102001), 
    msh.per.school_j_college_female = as.numeric(T001109045) / as.numeric(T001102001), 
    msh.per.school_university = as.numeric(T001109046) / as.numeric(T001102001), # 在学者 大学
    msh.per.school_university_male = as.numeric(T001109047) / as.numeric(T001102001), 
    msh.per.school_university_female = as.numeric(T001109048) / as.numeric(T001102001), 
    # 学歴 15歳以上
    msh.per.school_grad = as.numeric(T001109049) / as.numeric(T001102007), # 最終学歴
    msh.per.school_grad_male = as.numeric(T001109050) / as.numeric(T001102007), 
    msh.per.school_grad_female = as.numeric(T001109051) / as.numeric(T001102007), 
    msh.per.school_grad_compulsory = as.numeric(T001109052) / as.numeric(T001102007), # 最終学歴 小中学
    msh.per.school_grad_compulsory_male = as.numeric(T001109053) / as.numeric(T001102007), 
    msh.per.school_grad_compulsory_female = as.numeric(T001109054) / as.numeric(T001102007), 
    msh.per.school_grad_high = as.numeric(T001109055) / as.numeric(T001102007), # 最終学歴 高校
    msh.per.school_grad_high_male = as.numeric(T001109056) / as.numeric(T001102007), 
    msh.per.school_grad_high_female = as.numeric(T001109057) / as.numeric(T001102007), 
    msh.per.school_grad_j_college = as.numeric(T001109058) / as.numeric(T001102007), # 最終学歴 短大高専
    msh.per.school_grad_j_college_male = as.numeric(T001109059) / as.numeric(T001102007), 
    msh.per.school_grad_j_college_female = as.numeric(T001109060) / as.numeric(T001102007), 
    msh.per.school_grad_university = as.numeric(T001109061) / as.numeric(T001102007), # 最終学歴 大学
    msh.per.school_grad_university_male = as.numeric(T001109062) / as.numeric(T001102007), 
    msh.per.school_grad_university_female = as.numeric(T001109063) / as.numeric(T001102007),
    # 居住期間
    msh.per.live_until_now = as.numeric(T001109064) / as.numeric(T001102001), # ずっと住んでいる
    msh.per.live_until_now_male = as.numeric(T001109065) / as.numeric(T001102001), 
    msh.per.live_until_now_female = as.numeric(T001109066) / as.numeric(T001102001), 
    msh.per.live_1 = as.numeric(T001109067) / as.numeric(T001102001), # 1年未満
    msh.per.live_1_male = as.numeric(T001109068) / as.numeric(T001102001), 
    msh.per.live_1_female = as.numeric(T001109069) / as.numeric(T001102001), 
    msh.per.live_1_5 = as.numeric(T001109070) / as.numeric(T001102001), # 1-5年
    msh.per.live_1_5_male = as.numeric(T001109071) / as.numeric(T001102001), 
    msh.per.live_1_5_female = as.numeric(T001109072) / as.numeric(T001102001), 
    msh.per.live_5_10 = as.numeric(T001109073) / as.numeric(T001102001), # 5-10年
    msh.per.live_5_10_male = as.numeric(T001109074) / as.numeric(T001102001), 
    msh.per.live_5_10_female = as.numeric(T001109075) / as.numeric(T001102001), 
    msh.per.live_10_20 = as.numeric(T001109076) / as.numeric(T001102001), # 10-20年
    msh.per.live_10_20_male = as.numeric(T001109077) / as.numeric(T001102001), 
    msh.per.live_10_20_female = as.numeric(T001109078) / as.numeric(T001102001), 
    msh.per.live_20 = as.numeric(T001109079) / as.numeric(T001102001), # 20年-
    msh.per.live_20_male = as.numeric(T001109080) / as.numeric(T001102001), 
    msh.per.live_20_female = as.numeric(T001109081) / as.numeric(T001102001), 
    
    
    # 新しい国勢調査
    # 5歳区切りの人口
    msh.per.population_0_4 = as.numeric(T001196004) / as.numeric(T001196001),
    msh.per.population_0_4_male = as.numeric(T001196005) / as.numeric(T001196004),
    msh.per.population_0_4_female = as.numeric(T001196006) / as.numeric(T001196004),
    
    msh.per.population_5_9 = as.numeric(T001196007) / as.numeric(T001196001),
    msh.per.population_5_9_male = as.numeric(T001196008) / as.numeric(T001196007),
    msh.per.population_5_9_female = as.numeric(T001196009) / as.numeric(T001196007),
    
    msh.per.population_10_14 = as.numeric(T001196010) / as.numeric(T001196001),
    msh.per.population_10_14_male = as.numeric(T001196011) / as.numeric(T001196010),
    msh.per.population_10_14_female = as.numeric(T001196012) / as.numeric(T001196010),
    
    msh.per.population_15_19 = as.numeric(T001196013) / as.numeric(T001196001),
    msh.per.population_15_19_male = as.numeric(T001196014) / as.numeric(T001196013),
    msh.per.population_15_19_female = as.numeric(T001196015) / as.numeric(T001196013),
    
    msh.per.population_20_24 = as.numeric(T001196016) / as.numeric(T001196001),
    msh.per.population_20_24_male = as.numeric(T001196017) / as.numeric(T001196016),
    msh.per.population_20_24_female = as.numeric(T001196018) / as.numeric(T001196016),
    
    msh.per.population_25_29 = as.numeric(T001196019) / as.numeric(T001196001),
    msh.per.population_25_29_male = as.numeric(T001196020) / as.numeric(T001196019),
    msh.per.population_25_29_female = as.numeric(T001196021) / as.numeric(T001196019),
    
    msh.per.population_30_34 = as.numeric(T001196022) / as.numeric(T001196001),
    msh.per.population_30_34_male = as.numeric(T001196023) / as.numeric(T001196022),
    msh.per.population_30_34_female = as.numeric(T001196024) / as.numeric(T001196022),
    
    msh.per.population_35_39 = as.numeric(T001196025) / as.numeric(T001196001),
    msh.per.population_35_39_male = as.numeric(T001196026) / as.numeric(T001196025),
    msh.per.population_35_39_female = as.numeric(T001196027) / as.numeric(T001196025),
    
    msh.per.population_40_44 = as.numeric(T001196028) / as.numeric(T001196001),
    msh.per.population_40_44_male = as.numeric(T001196029) / as.numeric(T001196028),
    msh.per.population_40_44_female = as.numeric(T001196030) / as.numeric(T001196028),
    
    msh.per.population_45_49 = as.numeric(T001196031) / as.numeric(T001196001),
    msh.per.population_45_49_male = as.numeric(T001196032) / as.numeric(T001196031),
    msh.per.population_45_49_female = as.numeric(T001196033) / as.numeric(T001196031),
    
    msh.per.population_50_54 = as.numeric(T001196034) / as.numeric(T001196001),
    msh.per.population_50_54_male = as.numeric(T001196035) / as.numeric(T001196034),
    msh.per.population_50_54_female = as.numeric(T001196036) / as.numeric(T001196034),
    
    msh.per.population_55_59 = as.numeric(T001196037) / as.numeric(T001196001),
    msh.per.population_55_59_male = as.numeric(T001196038) / as.numeric(T001196037),
    msh.per.population_55_59_female = as.numeric(T001196039) / as.numeric(T001196037),
    
    msh.per.population_60_64 = as.numeric(T001196040) / as.numeric(T001196001),
    msh.per.population_60_64_male = as.numeric(T001196041) / as.numeric(T001196040),
    msh.per.population_60_64_female = as.numeric(T001196042) / as.numeric(T001196040),
    
    msh.per.population_65_69 = as.numeric(T001196043) / as.numeric(T001196001),
    msh.per.population_65_69_male = as.numeric(T001196044) / as.numeric(T001196043),
    msh.per.population_65_69_female = as.numeric(T001196045) / as.numeric(T001196043),
    
    msh.per.population_70_74 = as.numeric(T001196046) / as.numeric(T001196001),
    msh.per.population_70_74_male = as.numeric(T001196047) / as.numeric(T001196046),
    msh.per.population_70_74_female = as.numeric(T001196048) / as.numeric(T001196046),
    
    msh.per.population_75_79 = as.numeric(T001196049) / as.numeric(T001196001),
    msh.per.population_75_79_male = as.numeric(T001196050) / as.numeric(T001196049),
    msh.per.population_75_79_female = as.numeric(T001196051) / as.numeric(T001196049),
    
    msh.per.population_80_84 = as.numeric(T001196052) / as.numeric(T001196001),
    msh.per.population_80_84_male = as.numeric(T001196053) / as.numeric(T001196052),
    msh.per.population_80_84_female = as.numeric(T001196054) / as.numeric(T001196052),
    
    msh.per.population_85_89 = as.numeric(T001196055) / as.numeric(T001196001),
    msh.per.population_85_89_male = as.numeric(T001196056) / as.numeric(T001196055),
    msh.per.population_85_89_female = as.numeric(T001196057) / as.numeric(T001196055),
    
    msh.per.population_90_94 = as.numeric(T001196058) / as.numeric(T001196001),
    msh.per.population_90_94_male = as.numeric(T001196059) / as.numeric(T001196058),
    msh.per.population_90_94_female = as.numeric(T001196060) / as.numeric(T001196058),
    
    msh.per.population_95over = as.numeric(T001196061) / as.numeric(T001196001),
    msh.per.population_95over_male = as.numeric(T001196062) / as.numeric(T001196061),
    msh.per.population_95over_female = as.numeric(T001196063) / as.numeric(T001196061),
    msh.mean_age = as.numeric(T001196064),
    msh.median_age = as.numeric(T001196065),
    
    
    # 労働力割合（分母は全人口）
    msh.per.labor_force = as.numeric(T001197001) / as.numeric(T001102001),
    # 性別ごとの労働力割合（分母は労働力人口）
    msh.per.labor_force_male = as.numeric(T001197002) / as.numeric(T001197001),
    msh.per.labor_force_female = as.numeric(T001197003) / as.numeric(T001197001),
    # 労働力人口に占める就業者割合
    msh.per.worker = as.numeric(T001197004) / as.numeric(T001197001),
    # 性別ごとの就業者割合（分母は就業者数）
    msh.per.worker_male = as.numeric(T001197005) / as.numeric(T001197004),
    msh.per.worker_female = as.numeric(T001197006) / as.numeric(T001197004),
    # 労働力人口に占める完全失業者割合
    msh.per.unemployed = as.numeric(T001197007) / as.numeric(T001197001),
    # 性別ごとの完全失業者割合（分母は完全失業者数）
    msh.per.unemployed_male = as.numeric(T001197008) / as.numeric(T001197007),
    msh.per.unemployed_female = as.numeric(T001197009) / as.numeric(T001197007),
    # 非労働者割合（分母は全人口）
    msh.per.non_labor_force = as.numeric(T001197010) / as.numeric(T001102001),
    # 性別ごとの完全失業者割合（分母は完全失業者数）
    msh.per.non_labor_force_male = as.numeric(T001197011) / as.numeric(T001197010),
    msh.per.non_labor_force_female = as.numeric(T001197012) / as.numeric(T001197010),
    
    # 第一次産業
    msh.per.industry1 = as.numeric(T001197013) / as.numeric(T001197004),
    msh.per.industry1_male = as.numeric(T001197014) / as.numeric(T001197013),
    msh.per.industry1_female = as.numeric(T001197015) / as.numeric(T001197013),
    # 農林業
    msh.per.agri_forestry = as.numeric(T001197016) / as.numeric(T001197004),
    msh.per.agri_forestry_male = as.numeric(T001197017) / as.numeric(T001197016),
    msh.per.agri_forestry_female = as.numeric(T001197018) / as.numeric(T001197016),
    # 漁業
    msh.per.fishery = as.numeric(T001197019) / as.numeric(T001197004),
    msh.per.fishery_male = as.numeric(T001197020) / as.numeric(T001197019),
    msh.per.fishery_female = as.numeric(T001197021) / as.numeric(T001197019),
    # 第二次産業
    msh.per.industry2 = as.numeric(T001197022) / as.numeric(T001197004),
    msh.per.industry2_male = as.numeric(T001197023) / as.numeric(T001197022),
    msh.per.industry2_female = as.numeric(T001197024) / as.numeric(T001197022),
    # 鉱業
    msh.per.mining = as.numeric(T001197025) / as.numeric(T001197004),
    msh.per.mining_male = as.numeric(T001197026) / as.numeric(T001197025),
    msh.per.mining_female = as.numeric(T001197027) / as.numeric(T001197025),
    # 建設業
    msh.per.construction = as.numeric(T001197028) / as.numeric(T001197004),
    msh.per.construction_male = as.numeric(T001197029) / as.numeric(T001197028),
    msh.per.construction_female = as.numeric(T001197030) / as.numeric(T001197028),
    # 製造業
    msh.per.manufacturing = as.numeric(T001197031) / as.numeric(T001197004),
    msh.per.manufacturing_male = as.numeric(T001197032) / as.numeric(T001197031),
    msh.per.manufacturing_female = as.numeric(T001197033) / as.numeric(T001197031),
    # 第三次産業
    msh.per.industry3 = as.numeric(T001197034) / as.numeric(T001197004),
    msh.per.industry3_male = as.numeric(T001197035) / as.numeric(T001197034),
    msh.per.industry3_female = as.numeric(T001197036) / as.numeric(T001197034),
    # 主要サービス業の例（他も同様に追加可能）
    msh.per.utilities = as.numeric(T001197037) / as.numeric(T001197004), # 電気・ガス・水道
    msh.per.utilities_male = as.numeric(T001197038) / as.numeric(T001197037),
    msh.per.utilities_female = as.numeric(T001197039) / as.numeric(T001197037),
    msh.per.info_comm = as.numeric(T001197040) / as.numeric(T001197004), # 情報通信業
    msh.per.info_comm_male = as.numeric(T001197041) / as.numeric(T001197040),
    msh.per.info_comm_female = as.numeric(T001197042) / as.numeric(T001197040),
    msh.per.transport = as.numeric(T001197043) / as.numeric(T001197004), # 運輸業・郵便業
    msh.per.transport_male = as.numeric(T001197044) / as.numeric(T001197043),
    msh.per.transport_female = as.numeric(T001197045) / as.numeric(T001197043),
    msh.per.retail = as.numeric(T001197046) / as.numeric(T001197004), # 卸売・小売業
    msh.per.retail_male = as.numeric(T001197047) / as.numeric(T001197046),
    msh.per.retail_female = as.numeric(T001197048) / as.numeric(T001197046),
    msh.per.finance = as.numeric(T001197049) / as.numeric(T001197004), # 金融・保険業
    msh.per.finance_male = as.numeric(T001197050) / as.numeric(T001197049),
    msh.per.finance_female = as.numeric(T001197051) / as.numeric(T001197049),
    msh.per.real_estate = as.numeric(T001197052) / as.numeric(T001197004), # 不動産業
    msh.per.real_estate_male = as.numeric(T001197053) / as.numeric(T001197052),
    msh.per.real_estate_female = as.numeric(T001197054) / as.numeric(T001197052),
    msh.per.professional_services = as.numeric(T001197055) / as.numeric(T001197004), # 学術研究，専門・技術サービス業
    msh.per.professional_services_male = as.numeric(T001197056) / as.numeric(T001197055),
    msh.per.professional_services_female = as.numeric(T001197057) / as.numeric(T001197055),
    msh.per.accommodation_food = as.numeric(T001197058) / as.numeric(T001197004), # 宿泊業，飲食サービス業
    msh.per.accommodation_food_male = as.numeric(T001197059) / as.numeric(T001197058),
    msh.per.accommodation_food_female = as.numeric(T001197060) / as.numeric(T001197058),
    msh.per.personal_services = as.numeric(T001197061) / as.numeric(T001197004), # 生活関連サービス業，娯楽業
    msh.per.personal_services_male = as.numeric(T001197062) / as.numeric(T001197061),
    msh.per.personal_services_female = as.numeric(T001197063) / as.numeric(T001197061),
    msh.per.education = as.numeric(T001197064) / as.numeric(T001197004), # 教育，学習支援業
    msh.per.education_male = as.numeric(T001197065) / as.numeric(T001197064),
    msh.per.education_female = as.numeric(T001197066) / as.numeric(T001197064),
    msh.per.healthcare = as.numeric(T001197067) / as.numeric(T001197004), # 医療・福祉
    msh.per.healthcare_male = as.numeric(T001197068) / as.numeric(T001197067),
    msh.per.healthcare_female = as.numeric(T001197069) / as.numeric(T001197067),
    msh.per.combined_services = as.numeric(T001197070) / as.numeric(T001197004), # 複合サービス事業
    msh.per.combined_services_male = as.numeric(T001197071) / as.numeric(T001197070),
    msh.per.combined_services_female = as.numeric(T001197072) / as.numeric(T001197070),
    msh.per.other_services = as.numeric(T001197073) / as.numeric(T001197004), # サービス業（他に分類されないもの）
    msh.per.other_services_male = as.numeric(T001197074) / as.numeric(T001197073),
    msh.per.other_services_female = as.numeric(T001197075) / as.numeric(T001197073),
    msh.per.public_admin = as.numeric(T001197076) / as.numeric(T001197004), # 公務（他に分類されるものを除く）
    msh.per.public_admin_male = as.numeric(T001197077) / as.numeric(T001197076),
    msh.per.public_admin_female = as.numeric(T001197078) / as.numeric(T001197076),
    # 就業者に占める従業上の地位
    msh.per.management = as.numeric(T001197079) / as.numeric(T001197004), # 管理的職業従事者
    msh.per.management_male = as.numeric(T001197080) / as.numeric(T001197079),
    msh.per.management_female = as.numeric(T001197081) / as.numeric(T001197079),
    msh.per.professional = as.numeric(T001197082) / as.numeric(T001197004), # 専門的・技術的職業従事者
    msh.per.professional_male = as.numeric(T001197083) / as.numeric(T001197082),
    msh.per.professional_female = as.numeric(T001197084) / as.numeric(T001197082),
    msh.per.clerical = as.numeric(T001197085) / as.numeric(T001197004), # 事務従事者
    msh.per.clerical_male = as.numeric(T001197086) / as.numeric(T001197085),
    msh.per.clerical_female = as.numeric(T001197087) / as.numeric(T001197085),
    msh.per.sales = as.numeric(T001197088) / as.numeric(T001197004), # 販売従事者
    msh.per.sales_male = as.numeric(T001197089) / as.numeric(T001197088),
    msh.per.sales_female = as.numeric(T001197090) / as.numeric(T001197088),
    msh.per.service = as.numeric(T001197091) / as.numeric(T001197004), # サービス職業従事者
    msh.per.service_male = as.numeric(T001197092) / as.numeric(T001197091),
    msh.per.service_female = as.numeric(T001197093) / as.numeric(T001197091),
    msh.per.security = as.numeric(T001197094) / as.numeric(T001197004), # 保安職業従事者
    msh.per.security_male = as.numeric(T001197095) / as.numeric(T001197094),
    msh.per.security_female = as.numeric(T001197096) / as.numeric(T001197094),
    msh.per.agriculture_worker = as.numeric(T001197097) / as.numeric(T001197004), # 農林漁業従事者
    msh.per.agriculture_worker_male = as.numeric(T001197098) / as.numeric(T001197097),
    msh.per.agriculture_worker_female = as.numeric(T001197099) / as.numeric(T001197097),
    msh.per.production = as.numeric(T001197100) / as.numeric(T001197004), # 生産工程従事者
    msh.per.production_male = as.numeric(T001197101) / as.numeric(T001197100),
    msh.per.production_female = as.numeric(T001197102) / as.numeric(T001197100),
    msh.per.transport_operator = as.numeric(T001197103) / as.numeric(T001197004), # 輸送・機械運転従事者
    msh.per.transport_operator_male = as.numeric(T001197104) / as.numeric(T001197103),
    msh.per.transport_operator_female = as.numeric(T001197105) / as.numeric(T001197103),
    msh.per.construction_miner = as.numeric(T001197106) / as.numeric(T001197004), # 建設・採掘従事者
    msh.per.construction_miner_male = as.numeric(T001197107) / as.numeric(T001197106),
    msh.per.construction_miner_female = as.numeric(T001197108) / as.numeric(T001197106),
    msh.per.transport_cleaning = as.numeric(T001197109) / as.numeric(T001197004), # 運搬・清掃・包装等従事者
    msh.per.transport_cleaning_male = as.numeric(T001197110) / as.numeric(T001197109),
    msh.per.transport_cleaning_female = as.numeric(T001197111) / as.numeric(T001197109),
    
    # 住宅の所有関係（分母：住宅に住む一般世帯数）
    msh.per.house_owner = as.numeric(T001198002) / as.numeric(T001198001), # 持ち家
    msh.per.house_public_rent = as.numeric(T001198003) / as.numeric(T001198001), # 公営・UR・公社
    msh.per.house_private_rent = as.numeric(T001198004) / as.numeric(T001198001), # 民営借家
    msh.per.house_company_rent = as.numeric(T001198005) / as.numeric(T001198001), # 給与住宅
    msh.per.house_lodger = as.numeric(T001198006) / as.numeric(T001198001), # 間借り
    # 住宅の建て方（分母：住宅に住む一般世帯数）
    msh.per.detached = as.numeric(T001198007) / as.numeric(T001198001), # 一戸建
    msh.per.row_house = as.numeric(T001198008) / as.numeric(T001198001), # 長屋建
    msh.per.apartment = as.numeric(T001198009) / as.numeric(T001198001), # 共同住宅
    msh.per.apartment_1_2 = as.numeric(T001198010) / as.numeric(T001198001), # 共同住宅 1-2階
    msh.per.apartment_3_5 = as.numeric(T001198011) / as.numeric(T001198001), # 共同住宅 3-5階
    msh.per.apartment_6_10 = as.numeric(T001198012) / as.numeric(T001198001), # 共同住宅 6-10階
    msh.per.apartment_11_14 = as.numeric(T001198013) / as.numeric(T001198001), # 共同住宅 11-14階
    msh.per.apartment_15over = as.numeric(T001198014) / as.numeric(T001198001), # 共同住宅 15階以上
    # 世帯が住んでいる階（分母：住宅に住む一般世帯数）
    msh.per.floor_1_2 = as.numeric(T001198015) / as.numeric(T001198001), # 1-2階に住む世帯
    msh.per.floor_3_5 = as.numeric(T001198016) / as.numeric(T001198001), # 3-5階に住む世帯
    msh.per.floor_6_10 = as.numeric(T001198017) / as.numeric(T001198001), # 6-10階に住む世帯
    msh.per.floor_11_14 = as.numeric(T001198018) / as.numeric(T001198001), # 11-14階に住む世帯
    msh.per.floor_15over = as.numeric(T001198019) / as.numeric(T001198001), # 15階以上に住む世帯
    
    msh.household_persons_total = as.numeric(T001198020) / as.numeric(T001198001),   # 一般世帯人員 総数
    msh.household_persons_in_housing = as.numeric(T001198021) / as.numeric(T001198001), # 住宅に住む一般世帯人員 総数
    
    
    # =============================================== #
    # 現住所に住んでいる
    msh.per.residence_5yrs_ago_stay = as.numeric(T001199004) / as.numeric(T001199001),
    msh.per.residence_5yrs_ago_stay_male = as.numeric(T001199005) / as.numeric(T001199004),
    msh.per.residence_5yrs_ago_stay_female = as.numeric(T001199006) / as.numeric(T001199004),
    # 移住した人
    msh.per.residence_5yrs_ago_moved = as.numeric(T001199007) / as.numeric(T001199001),
    msh.per.residence_5yrs_ago_moved_male = as.numeric(T001199008) / as.numeric(T001199007),
    msh.per.residence_5yrs_ago_moved_female = as.numeric(T001199009) / as.numeric(T001199007),
    # 移住した人 市区町村内　（分母は移動した人数）
    msh.per.residence_5yrs_ago_moved_inner = as.numeric(T001199010) / as.numeric(T001199007),
    msh.per.residence_5yrs_ago_moved_inner_male = as.numeric(T001199011) / as.numeric(T001199010),
    msh.per.residence_5yrs_ago_moved_inner_female = as.numeric(T001199012) / as.numeric(T001199010),
    # 移住した人　県内の他の市区町村から（分母は移動した人数）
    msh.per.residence_5yrs_ago_moved_outer = as.numeric(T001199013) / as.numeric(T001199007),
    msh.per.residence_5yrs_ago_moved_outer_male = as.numeric(T001199014) / as.numeric(T001199013),
    msh.per.residence_5yrs_ago_moved_outer_female = as.numeric(T001199015) / as.numeric(T001199013),
    # 移住した人　県外から（分母は移動した人数）
    msh.per.residence_5yrs_ago_moved_other_pref = as.numeric(T001199016) / as.numeric(T001199007),
    msh.per.residence_5yrs_ago_moved_other_pref_male = as.numeric(T001199017) / as.numeric(T001199016),
    msh.per.residence_5yrs_ago_moved_other_pref_female = as.numeric(T001199018) / as.numeric(T001199016),
    # 5歳以上の人口に関して
    # 現住所に住んでいる
    msh.per.residence_5yrs_ago_stay_age5_ = as.numeric(T001199022) / as.numeric(T001199019),
    msh.per.residence_5yrs_ago_stay_age5_male = as.numeric(T001199023) / as.numeric(T001199022),
    msh.per.residence_5yrs_ago_stay_age5_female = as.numeric(T001199024) / as.numeric(T001199022),
    # 移住した人
    msh.per.residence_5yrs_ago_moved_age5 = as.numeric(T001199025) / as.numeric(T001199019),
    msh.per.residence_5yrs_ago_moved_age5_male = as.numeric(T001199026) / as.numeric(T001199025),
    msh.per.residence_5yrs_ago_moved_age5_female = as.numeric(T001199027) / as.numeric(T001199025),
    # 移住した人 市区町村内
    msh.per.residence_5yrs_ago_moved_age5_inner = as.numeric(T001199028) / as.numeric(T001199019),
    msh.per.residence_5yrs_ago_moved_age5_inner_male = as.numeric(T001199029) / as.numeric(T001199028),
    msh.per.residence_5yrs_ago_moved_age5_inner_female = as.numeric(T001199030) / as.numeric(T001199028),
    # 移住した人 県内の他の市区町村から
    msh.per.residence_5yrs_ago_moved_age5_outer = as.numeric(T001199031) / as.numeric(T001199019),
    msh.per.residence_5yrs_ago_moved_age5_outer_male = as.numeric(T001199032) / as.numeric(T001199031),
    msh.per.residence_5yrs_ago_moved_age5_outer_female = as.numeric(T001199033) / as.numeric(T001199031),
    # 移住した人 他県から
    msh.per.residence_5yrs_ago_moved_age5_other_pref = as.numeric(T001199034) / as.numeric(T001199019),
    msh.per.residence_5yrs_ago_moved_age5_other_pref_male = as.numeric(T001199035) / as.numeric(T001199034),
    msh.per.residence_5yrs_ago_moved_age5_other_pref_female = as.numeric(T001199036) / as.numeric(T001199034),
    # 居住者のうちの就業者
    msh.per.worker = as.numeric(T001199038) / as.numeric(T001199037),
    # 居住者のうちの通学者
    msh.per.student = as.numeric(T001199039) / as.numeric(T001199037),
    # 自宅就業者
    msh.per.worker_home = as.numeric(T001199040) / as.numeric(T001199038),
    # 自宅街の自市区町村で就業・通学
    msh.per.worker_student_inner = as.numeric(T001199041) / as.numeric(T001199037),
    # 自宅街の自市区町村で就業
    msh.per.worker_inner = as.numeric(T001199042) / as.numeric(T001199038),
    # 自宅街の自市区町村で通学
    msh.per.student_inner = as.numeric(T001199043) / as.numeric(T001199039),
    # 他の市区町村就業・通学
    msh.per.worker_student_outer = as.numeric(T001199044) / as.numeric(T001199037),
    # 他の自市区町村で就業
    msh.per.worker_outer = as.numeric(T001199045) / as.numeric(T001199038),
    # 他の自市区町村で通学
    msh.per.student_outer = as.numeric(T001199046) / as.numeric(T001199039),
    # 他県で就業・通学
    msh.per.worker_student_other_pref = as.numeric(T001199047) / as.numeric(T001199037),
    # 他県で就業
    msh.per.worker_student_other_pref = as.numeric(T001199048) / as.numeric(T001199038),
    # 他県へ通学
    msh.per.worker_student_other_pref = as.numeric(T001199049) / as.numeric(T001199039),
    # 利用交通手段
    # 徒歩のみ
    msh.per.commute_walk = as.numeric(T001199050) / as.numeric(T001199037),
    # 電車
    msh.per.commute_train = as.numeric(T001199051) / as.numeric(T001199037),
    # 乗合バス
    msh.per.commute_bus = as.numeric(T001199052) / as.numeric(T001199037),
    # 自家用車
    msh.per.commute_car = as.numeric(T001199053) / as.numeric(T001199037),
    # オートバイ
    msh.per.commute_motorcycle = as.numeric(T001199054) / as.numeric(T001199037),
    # 自転車
    msh.per.commute_bicycle = as.numeric(T001199055) / as.numeric(T001199037),
    # 勤め先のバス
    msh.per.commute_commuter_bus = as.numeric(T001199056) / as.numeric(T001199037),
    # タクシー
    msh.per.commute_taxi = as.numeric(T001199057) / as.numeric(T001199037),
    # 教育に関して
    # 就学状況
    msh.per.before_school = as.numeric(T001199058) / as.numeric(T001102001), # 未就学
    msh.per.before_school_male = as.numeric(T001199059) / as.numeric(T001199058),
    msh.per.before_school_female = as.numeric(T001199060) / as.numeric(T001199058),
    msh.per.before_school_youchien = as.numeric(T001199061) / as.numeric(T001199058), # 未就学 幼稚園
    msh.per.before_school_youchien_male = as.numeric(T001199062) / as.numeric(T001199058),
    msh.per.before_school_youchien_female = as.numeric(T001199063) / as.numeric(T001199058),
    msh.per.before_school_hoikuen = as.numeric(T001199064) / as.numeric(T001199058), # 未就学 保育園
    msh.per.before_school_hoikuen_male = as.numeric(T001199065) / as.numeric(T001199058),
    msh.per.before_school_hoikuen_female = as.numeric(T001199066) / as.numeric(T001199058),
    msh.per.before_school_nintei = as.numeric(T001199067) / as.numeric(T001199058), # 未就学 認定こども園
    msh.per.before_school_nintei_male = as.numeric(T001199068) / as.numeric(T001199058),
    msh.per.before_school_nintei_female = as.numeric(T001199069) / as.numeric(T001199058),
    msh.per.before_school_other = as.numeric(T001199070) / as.numeric(T001199058), # 未就学 その他
    msh.per.before_school_other_male = as.numeric(T001199071) / as.numeric(T001199058),
    msh.per.before_school_other_female = as.numeric(T001199072) / as.numeric(T001199058),
    msh.per.school = as.numeric(T001199073) / as.numeric(T001102001), # 在学者
    msh.per.school_male = as.numeric(T001199074) / as.numeric(T001199073), 
    msh.per.school_female = as.numeric(T001199075) / as.numeric(T001199073), 
    msh.per.school_compulsory = as.numeric(T001199076) / as.numeric(T001199073), # 在学者 義務教育
    msh.per.school_compulsory_male = as.numeric(T001199077) / as.numeric(T001199073), 
    msh.per.school_compulsory_female = as.numeric(T001199078) / as.numeric(T001199073), 
    msh.per.school_high = as.numeric(T001199079) / as.numeric(T001199073), # 在学者 高校
    msh.per.school_high_male = as.numeric(T001199080) / as.numeric(T001199073), 
    msh.per.school_high_female = as.numeric(T001199081) / as.numeric(T001199073), 
    msh.per.school_j_college = as.numeric(T001199082) / as.numeric(T001199073), # 在学者 短大高専
    msh.per.school_j_college_male = as.numeric(T001199083) / as.numeric(T001199073), 
    msh.per.school_j_college_female = as.numeric(T001199084) / as.numeric(T001199073), 
    msh.per.school_university = as.numeric(T001199085) / as.numeric(T001199073), # 在学者 大学
    msh.per.school_university_male = as.numeric(T001199086) / as.numeric(T001199073), 
    msh.per.school_university_female = as.numeric(T001199087) / as.numeric(T001199073), 
    
    # 最終学歴
    msh.per.school_grad = as.numeric(T001199088) / as.numeric(T001102007), # 最終学歴
    msh.per.school_grad_male = as.numeric(T001199089) / as.numeric(T001199088), 
    msh.per.school_grad_female = as.numeric(T001199090) / as.numeric(T001199088), 
    msh.per.school_grad_compulsory = as.numeric(T001199091) / as.numeric(T001199088), # 最終学歴 小中学
    msh.per.school_grad_compulsory_male = as.numeric(T001199092) / as.numeric(T001199088), 
    msh.per.school_grad_compulsory_female = as.numeric(T001199093) / as.numeric(T001199088), 
    msh.per.school_grad_high = as.numeric(T001199094) / as.numeric(T001199088), # 最終学歴 高校
    msh.per.school_grad_high_male = as.numeric(T001199095) / as.numeric(T001199088), 
    msh.per.school_grad_high_female = as.numeric(T001199096) / as.numeric(T001199088), 
    msh.per.school_grad_j_college = as.numeric(T001199097) / as.numeric(T001199088), # 最終学歴 短大高専
    msh.per.school_grad_j_college_male = as.numeric(T001199098) / as.numeric(T001199088), 
    msh.per.school_grad_j_college_female = as.numeric(T001199099) / as.numeric(T001199088), 
    msh.per.school_grad_university = as.numeric(T001199100) / as.numeric(T001199088), # 最終学歴 大学
    msh.per.school_grad_university_male = as.numeric(T001199101) / as.numeric(T001199088), 
    msh.per.school_grad_university_female = as.numeric(T001199102) / as.numeric(T001199088),
    
    # 子供の年齢
    msh.per_children_0_2 = as.numeric(T001199103) / as.numeric(T001102001),
    msh.per_children_0_2_male = as.numeric(T001199104) / as.numeric(T001199103),
    msh.per_children_0_2_female = as.numeric(T001199105) / as.numeric(T001199103),
    msh.per_children_0_5 = as.numeric(T001199106) / as.numeric(T001102001),
    msh.per_children_0_5_male = as.numeric(T001199107) / as.numeric(T001199106),
    msh.per_children_0_5_female = as.numeric(T001199108) / as.numeric(T001199106),
    msh.per_children_3_5 = as.numeric(T001199109) / as.numeric(T001102001),
    msh.per_children_3_5_male = as.numeric(T001199110) / as.numeric(T001199109),
    msh.per_children_3_5_female = as.numeric(T001199111) / as.numeric(T001199109),
    msh.per_children_6_11 = as.numeric(T001199112) / as.numeric(T001102001),
    msh.per_children_6_11_male = as.numeric(T001199113) / as.numeric(T001199112),
    msh.per_children_6_11_female = as.numeric(T001199114) / as.numeric(T001199112),
    msh.per_children_12_14 = as.numeric(T001199115) / as.numeric(T001102001),
    msh.per_children_12_14_male = as.numeric(T001199116) / as.numeric(T001199115),
    msh.per_children_12_14_female = as.numeric(T001199117) / as.numeric(T001199115),
    msh.per_children_15_17 = as.numeric(T001199118) / as.numeric(T001102001),
    msh.per_children_15_17_male = as.numeric(T001199119) / as.numeric(T001199118),
    msh.per_children_15_17_female = as.numeric(T001199120) / as.numeric(T001199118)
  )
  return(processed_df)
}




























