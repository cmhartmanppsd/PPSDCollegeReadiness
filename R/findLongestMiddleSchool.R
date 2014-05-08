findLongestMiddleSchool <- function(df){
  # This mimics the select_hs() function but for middle schools.
  ms <- df %.%
    filter(grade %in% seq(7, 8, 1)) %.%
    group_by(sasid, schno, school) %.%
    summarize(total_adm = sum(adm))
  ms <- ungroup(ms) %.%
    group_by(sasid) %.%
    filter(total_adm == max(total_adm)) %.%
    select(sasid, schno, school)
  # At this point, MS should have the right school. However, there could be more
  # than one school with the same number of enrollment days. So the next step
  # merges in the school year and selects the max school year.
  ms <- left_join(ms,  df %.% select(sasid, schno, schoolyear))
  ms <- ms %.%
    group_by(sasid) %.%
    filter(row_number(desc(schoolyear))==1) %.%
    select(sasid, schno, school)
  names(ms)[-1] <- paste(names(ms)[-1], 'ms_long', sep = '_')
  ms
}

findFreshmanHS <- function(df, schoolyear_first){
  hs <- df %.%
        filter(grade == 9 & schoolyear==schoolyear_first) %.%
        group_by(sasid, schno, school) %.%
        summarize(total_adm = sum(adm))
  hs <- ungroup(hs) %.%
        group_by(sasid) %.%
        filter(total_adm == max(total_adm)) %.%
        select(sasid, schno, school, total_adm)
  # Resolve multiple schools with the same total_adm
  hs <- hs %.%
        group_by(sasid) %.%
        filter(rank(total_adm, ties.method="first")==1) %.%
        select(sasid, schno, school)
  # 'hs_f9_long' = high school first-time 9th grade longest
  names(hs)[-1] <- paste(names(hs[-1]), 'hs_f9_long', sep = '_')
  hs
}