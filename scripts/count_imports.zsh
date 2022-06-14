#!/usr/bin/env zsh

#####################################################################################
# Returns count of all unique classes/properties in OWL file by namespace
# (includes deprecated)
#
# Arguments:
# - The first argument should be the path to the doid-merged.owl file
# - The second argument (optional) specifies the format of namespaces. The
#   formats available are URI (default, requires no input or "uri") or
#   the last non-local element of the URI (~ CURIE; for any input other than
#   "uri")
#
# Output:
# - Count of unique classes/properties by namespace to stdout
#####################################################################################


if [ ${2:-uri} = "uri" ]; then
    # identify IRIs
    grep -E "<\!--.*-->" $1 |
    # remove duplicates (if any)
    sort | uniq |
    # identify namespace
    sed -E 's$.*(http.*/[^#_/]+[#/_]).*$\1$' |
    # count by namespace
    sort | uniq -c
else
    # alternate 'short' identification of namespace (shortened to last non-local element of URI)
    grep -E "<\!--.*-->" $1 |
    # remove duplicates (if any)
    sort | uniq |
    # identify namespace
    sed -E 's$.*/([^#_/]+)[#/_].*$\1$' |
    # count by namespace
    sort | uniq -c
fi
