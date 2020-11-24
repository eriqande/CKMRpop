

awk '
  BEGIN {
    OFS = "\t";
    print "year", "pop", "age", "male", "female" > "spip_prekill_census.tsv";
    print "year", "pop", "age", "male", "female" > "spip_postkill_census.tsv";
    print "year", "pop", "kid", "pa", "ma" > "spip_pedigree.tsv";
    print "ID", "syears", "pop" > "spip_samples.tsv";
    print "ID", "year", "age" > "spip_deaths.tsv"
  }

  ### Getting the prekill Census  ###
  get_head == 0 && /^PREKILL_CENSUS_AGES/ {
    j = 0;
    get_head = 1;
    for(i=7;i<=NF;i++) ages[++j] = $i;
    next;
  }
  /^PREKILL_CENSUS_COUNTS/ && $3 == "MALES" {
    j = 0;
    pop = $7;
    for(i=9;i<=NF;i++) males[++j] = $i;
    next;
  }
  /^PREKILL_CENSUS_COUNTS/ && $3 == "FEM" {
    year = $5;
    j = 0;
    for(i=9;i<=NF;i++) {
      ++j;
      print year, pop, ages[j], males[j], $i > "spip_prekill_census.tsv";
    }
    next;
  }

  ###  Get the postkill census
  get_post_head == 0 && /^POSTKILL_CENSUS_AGES/ {
    j = 0;
    get_post_head = 1;
    for(i=7;i<=NF;i++) post_ages[++j] = $i;
    next;
  }
  /^POSTKILL_CENSUS_COUNTS/ && $3 == "MALES" {
    j = 0;
    for(i=9;i<=NF;i++) post_males[++j] = $i;
    pop = $7;
    next;
  }
  /^POSTKILL_CENSUS_COUNTS/ && $3 == "FEM" {
    year = $5;
    j = 0;
    for(i=9;i<=NF;i++) {
      ++j;
      print year, pop, post_ages[j], post_males[j], $i > "spip_postkill_census.tsv";
    }
    next;
  }




  ### Getting the pedigree entries ###
  /^PEDIGREE/ {
    print $3, $5, $7, $8, $9 > "spip_pedigree.tsv";
    next;
  }


  ### Get the death reports  ###
  /^KILLING/ {
    print $6, $9, $(12) > "spip_deaths.tsv";
    next;
  }

  ### Getting the sample entries ###
  # we have to do something a little more special here to use the colon as
  # the field separator, but we can do that.
  /^GenotypesByAllelicType/ {
    line = $0;
    gsub(/ *: */, ":", line)
    dump_n = split(line, arr, /:/)
    print arr[2], arr[3], arr[4] > "spip_samples.tsv";
    next;
  }


' spip_out.txt
