# simple script.  Give it a TAB delimited file $2 with the first column holding
# indexes.  This will pick out the line that matches $1, and return a command
# line assigning the column header names as shell variables whose values are the
# follows in the file $2.

if [ $# -ne 2 ]; then
  echo "Wrong number of arguments in $0 " > /dev/stderr
fi

awk -F"\t" -v LINE=$1 '
  $1 == "index" {for(i=1; i<=NF; i++) vars[i]=$i; next}
  $1 == LINE {for(i=1; i<=NF; i++) printf("%s=\"%s\"; ", vars[i], $i)}
' $2

