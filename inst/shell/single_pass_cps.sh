

awk '
  BEGIN {
    OFS = "\t";
    print "year", "age", "male", "female" > "spip_prekill_census.tsv";
    print "year", "kid", "pa", "ma" > "spip_pedigree.tsv";
    print "ID", "syears" > "spip_samples.tsv";
  }

  ### Getting the prekill Census  ###
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
      print year, ages[j], males[j], $i > "spip_prekill_census.tsv";
    }
    next;
  }


  ### Getting the pedigree entries ###
  /^PEDIGREE/ {
    print $3, $5, $6, $7 > "spip_pedigree.tsv";
  }


  ### Getting the sample entries ###
  # we have to do something a little more special here to use the colon as
  # the field separator, but we can do that.
  /^GenotypesByAllelicType/ {
    line = $0;
    gsub(/ *: */, ":", line)
    dump_n = split(line, arr, /:/)
    print arr[2], arr[3] > "spip_samples.tsv";
  }


' spip_out.txt
