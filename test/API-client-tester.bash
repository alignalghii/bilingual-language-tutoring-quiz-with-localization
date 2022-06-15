#!/bin/bash

TESTROOT=`dirname "$0"`;
PROJECTROOT="$TESTROOT/..";

cp $PROJECTROOT/var/session.data $PROJECTROOT/test/session.data.archive;
for tableName in practice question answer;
        do echo $tableName; cp $PROJECTROOT/var/$tableName.table $PROJECTROOT/test/$tableName.table.archive;
done;

curl -sS localhost:3000/practice/new -d word=on -d easy=on -d number_of_questions=1 --trace-ascii - | grep 'HTTP\|Location';
timestampAssignment="$(sed -n 's/.*\(maybePracticeStart[^,}]*\).*/\1/p' $PROJECTROOT/var/session.data)";
if diff $PROJECTROOT/test/practice.table.archive $PROJECTROOT/var/practice.table;
        then echo No change in practice table;
        else echo Practice table has just been changed;
fi;
echo "Timestamp assignment: ($timestampAssignment)"

cat << EOT > $PROJECTROOT/var/session.data;
Sssn {etalon = [LxcE {hu = "nulla", en = "zero", entity = LUWord, difficulty = Easy}], personal = [], $timestampAssignment}
EOT

curl -sS localhost:3000/question | grep input;
curl -sS localhost:3000/question -d hu=nulla -d en=zero --trace-ascii - | grep 'HTTP\|Location';
#curl -sS localhost:3000/question -d hu=nulla -d en=zero;
curl -sS localhost:3000/question | grep 'td class=\|[0-9]\{4\}\(-[0-9]\{2\}\)\{2\}';

for tableName in practice question answer;
    do
        if diff <(sed 's/},/},\n/g' $PROJECTROOT/test/$tableName.table.archive) <(sed 's/},/},\n/g' $PROJECTROOT/var/$tableName.table);
                then echo No change in $tableName table;
                else echo $tableName table has just been changed;
        fi;
done;

cp $PROJECTROOT/test/session.data.archive $PROJECTROOT/var/session.data;
for tableName in practice question answer;
        do cp $PROJECTROOT/test/$tableName.table.archive $PROJECTROOT/var/$tableName.table;
done;

if diff $PROJECTROOT/test/practice.table.archive $PROJECTROOT/var/practice.table;
        then echo No change in practice table;
        else echo Practice table has just been changed;
fi;
