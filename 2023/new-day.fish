#!/usr/bin/env fish

if test "$argv[1]" = ""
  echo 'Please provide a directory name:'
  read DIR_NAME
else
  set DIR_NAME $argv[1]
end

mkdir -p $DIR_NAME/src
touch $DIR_NAME/src/m.clj
touch $DIR_NAME/deps.edn
echo "{:deps {}}" >> $DIR_NAME/deps.edn

mv ~/Downloads/input.txt  $DIR_NAME