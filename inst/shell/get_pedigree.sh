

awk '
  BEGIN {
    OFS = "\t";
    print "year", "kid", "pa", "ma";
  }
  /^PEDIGREE/ {
    print $3, $5, $6, $7
  }' spip_out.txt
