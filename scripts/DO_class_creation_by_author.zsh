#!/bin/zsh

EDIT_FILE="${1}"
OUTDIR="${2}"
RANGE_OUTFILE="${OUTDIR:+$OUTDIR/}author_ranges.tsv"

# make sure file doesn't exist before appending to it
if [[ -f "$RANGE_OUTFILE" ]]; then
    echo "$RANGE_OUTFILE exists."
	exit 101
fi
echo "author\tclass_range" > "${RANGE_OUTFILE}"

# identify all authors
git shortlog -s | cut -f2 > "${OUTDIR}/authors.txt"
# echo "lschriml" > "${OUTDIR}/authors.txt"

# Custom function to convert comma-delimited list of numbers to ranges
# 	Copied from https://unix.stackexchange.com/a/562349; only name changed
to_range() {
	read input
	local i=1
	argv=(${(nus:,:)input})
	while ((i < $#)) {
    	if ((${argv[i]#*-} + 1 == ${argv[i+1]%-*})) {
			argv[i]=${argv[i]%-*}-${argv[i+1]#*-}
			argv[i+1]=()
		} else {
			((i++))
		}
	}
	print ${(j:,:)@}
}

# list classes added or removed by each author
# info on looping through a file: https://stackoverflow.com/questions/1521462/looping-through-the-content-of-a-file-in-bash
FILES=()
while read AUTHOR
do
	TMP="${AUTHOR//[ -\.\[\]]/_}_classes.txt"
    OUTFILE="${OUTDIR:+$OUTDIR/}${TMP//__/_}"

	git log -G"# Class:.*DOID" -p --author="${AUTHOR}" -- "${EDIT_FILE}" |
		grep -E "[-+]# Class:.*DOID" |
		awk '{
			if ($1 ~ /^ /) { class=""; next }
			if ($1 ~ /^-/) { class=$3; next }
			if ($1 ~ /^\+/) {
				if (class == $3) {
					class=""; next
				}
				else {
					class=""; print $3
				}
			}
		}' | sort > "${OUTFILE}"

    echo "${AUTHOR}"
    RANGE=$(sed -E 's/.*[_:]|>//g' "${OUTFILE}" | paste -s -d, - | to_range)
	echo "${AUTHOR}\t${RANGE}" >> "${RANGE_OUTFILE}"

    # list output files; not currently used
    # FILES+=("${OUTFILE}")
done <"${OUTDIR:+$OUTDIR/}authors.txt"


##### EXTRA STUFF #####
# echo "${FILES[@]}"
# for FILE in "${FILES[@]}"
# do
# 	AUTHOR=$(echo "${FILE}" | sed -E "s|${OUTDIR:+$OUTDIR/}(.*)_classes.txt|\1|")
#
# done

# awk 'NR==1{next} {class=$3; f=$NF=="minus"} $3 != class || $3 ~ "-#" && $NF=="plus"{print p,$3} ' sortedfile
#
# while read AUTHOR
# do
# 	git log -G"# Class:" --author="${AUTHOR}" -p -- "${EDIT_FILE}" |
# 		grep -E "[-+]# Class:" > "${OUTDIR}/${AUTHOR}_classes.txt"
# done <"${OUTDIR}/authors.txt"
