

awk '
  BEGIN {
    OFS = "\t";
    print "year", "age", "male", "female";
  }
  get_head == 0 && /^PREKILL_CENSUS_AGES/ {
    j = 0;
    get_head = 1;
    for(i=5;i<=NF;i++) ages[++j] = $i;
    next;
  }
  /^PREKILL_CENSUS_COUNTS/ && $3 == "MALES" {
    j = 0;
    for(i=7;i<=NF;i++) males[++j] = $i;
    next;
  }
  /^PREKILL_CENSUS_COUNTS/ && $3 == "FEM" {
    year = $5;
    j = 0;
    for(i=7;i<=NF;i++) {
      ++j;
      print year, ages[j], males[j], $i;
    }
    next;
  }
' spip_out.txt
