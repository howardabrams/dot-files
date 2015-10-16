#!/bin/${1:sh}
# ----------------------------------------------------------------------
#  `(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`: $0
# ----------------------------------------------------------------------

  OPT1="default value"   # Setup a default value
  OPT2=0

  while getopts "d:s" o
  do  case "$o" in
      d)  OPT1="$OPTARG";;
      s)  OPT2=1;;
      [?])    print >&2 "Usage: \$0 [-s] [-d seplist] file ..."
          exit 1;;
      esac
  done
  shift $(expr $OPTIND - 1)

# ----------------------------------------------------------------------
