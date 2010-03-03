#!/bin/bash


MR_BEGIN=( `grep "Begin MR" log  | sed -e "s/^.*: //g" -e "s/s//g"` )
MR_END=( `grep "END MR" log  | sed -e "s/^.*: //g" -e "s/s//g"` )
BEGIN=( `grep "Begin Client :" log  | sed -e "s/^.*: //g" -e "s/s//g"` )
END=( `grep "End Client :" log  | sed -e "s/^.*: //g" -e "s/s//g"` )

CONF=( `egrep -v "(^$|^#.*$)" $1` )

I=0
while [ "$I" -lt "${#CONF[@]}" ]
do
  [ ${#END[@]} -eq ${#CONF[@]} ] || exit 324
  [ ${#MR_END[@]} -eq ${#CONF[@]} ] || exit 324
  [ ${#BEGIN[@]} -eq ${#CONF[@]} ] || exit 324
  [ ${#MR_BEGIN[@]} -eq ${#CONF[@]} ] || exit 324
  
  TT=`echo "${END[$I]} - ${BEGIN[$I]}" | bc | sed "s/\./,/g"`
  MRT=`echo "${MR_END[$I]} - ${MR_BEGIN[$I]}" | bc | sed "s/\./,/g"`
  ST=`echo "${MR_BEGIN[$I]} - ${BEGIN[$I]}" | bc | sed "s/\./,/g"`
  SUT=`echo "${END[$I]} - ${MR_END[$I]}" | bc | sed "s/\./,/g"`
  echo "${CONF[$I]} $TT $MRT $ST $SUT"
  I=`expr $I + 1`
done
