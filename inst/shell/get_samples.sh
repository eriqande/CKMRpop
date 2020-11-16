

sed 's/ *:/:/g; s/: */:/g;' spip_out.txt | \
awk -F":" '
  BEGIN {
    OFS = "\t";
    print "ID", "syears"
  }
  /^GenotypesByAllelicType/ {
    print $2, $3
  }
'
