#!/bin/zsh
# Script identifies all DOID classes ever created in doid-edit.owl file and
# lists the author, commit, and date of their first edit (usually creation)

# USE: zsh <path_to_script> <path_to_doid-edit.owl> <path_to_dir_for_saving_output>

#### SETUP ####
# ensure variables are provided
if [[ -z ${1+x} || -z ${2+x} ]]
then
    echo "ERROR: Two arguments are required
  First: Path to doid-edit.owl
  Second: Path to directory for saving output
Example usage --> zsh $(basename $0) HumanDiseaseOntology/src/ontology/doid-edit.owl results/"
    exit 101
fi

# set internal variables
WD=$(pwd)
GIT_DIR=$(dirname "${1}")
EDIT_FILE=$(basename "${1}")
OUTDIR="${WD:+$WD/}${2}"
HISTORY_FILE="${OUTDIR:+$OUTDIR/}class_history.tsv"

# change to relevant git directory
cd "${GIT_DIR}"


#### IDENTIFY CLASSES ####
# FOR TESTING ONLY: A single class (LINE)
# echo "Class: obo:DOID_0080846" >  "${OUTDIR:+$OUTDIR/}raw_classes.txt"

# identify all classes ever created in file
git log -G"Class:.*DOID" -p -- "${EDIT_FILE}" |
    grep -Eo "Class:.*DOID[^ ]+" |
    sort | uniq > "${OUTDIR:+$OUTDIR/}raw_classes.txt"


#### GENERATE HISTORY FILE ####
# add header
echo "class\tauthor\tcommit\tdate" > "${HISTORY_FILE}"

# return first commit where each class was first edited
#   result will be the classes creation EXCEPT for classes created before the repository
while read LINE
do
    CLASS=$(echo "${LINE}" | sed -E 's/.*(DOID.[0-9]+).*/\1/')
    CREATION=$(git log -S"${LINE}" --pretty=format:"%an\t%h\t%as" -- "${EDIT_FILE}" | tail -1)
    echo "${CLASS}\t${CREATION}" >> "${HISTORY_FILE}"
done <"${OUTDIR:+$OUTDIR/}raw_classes.txt"


#### CLEANUP ####
cd "${WD}"
