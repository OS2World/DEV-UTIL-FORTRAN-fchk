#!/bin/sh
#
# Bourne shell script to compare File in directory ./Vary with
# original in directory ./Okay and print differences if any.
# If no differences it echoes a dot to show progress.
# If differences, touches file CHECK_FAILED
#
# Usage: Compare.sh Okay Vary File

OKAY=$1
VARY=$2
FILE=$3

	if [ ! -d ${OKAY} ] ;
	then
	    echo "Creating directory ${OKAY}" ;
	    mkdir ${OKAY};
	fi
	if [ ! -f ${OKAY}/${FILE} ] ;
	then
	    echo "${FILE} is new" ;
	    cp ${FILE} ${OKAY}/${FILE} ;
	fi
	if cmp ${OKAY}/${FILE} ${FILE} ;
	then
	    /bin/rm -f ${FILE} ;
	    echo '.' | awk '{printf("%s",$1);}' ;
	else
	    touch CHECK_FAILED
	    echo "--------------------------------------------------" ;
	    echo "Differences in ${OKAY}/${FILE}  ${VARY}/${FILE} :" ;
	    if [ ! -d ${VARY} ] ;
	    then
		echo "Creating directory ${VARY}" ;
		mkdir ${VARY} ;
	    fi
	    /bin/mv ${FILE} ${VARY}/${FILE} ;
	    diff ${OKAY}/${FILE} ${VARY}/${FILE} ;
	fi
	exit 0
