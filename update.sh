#!/bin/sh
#
# Automatic update from other repository
# Origin is https://github.com/manojo/lamp-dp-mt
# The paper is in a private repository
#

cd `dirname $0`
BASE=`pwd`
ORIGIN='../lamp-dp-mt'
PAPER='../papers/manohar/icfp2013'

# Cleanup structure
rm -r docs src
mkdir -p docs src

# Documents
texdoc() { # $1=file
	texi2dvi --pdf "$1.tex" >/dev/null
	rm *.aux *.log 2>/dev/null
	mv "`basename $1`.pdf" docs
}
#texdoc $PAPER/paper
cp $PAPER/paper.pdf docs
cp $ORIGIN/docs/report/report.pdf docs

# Source code
cp -r $ORIGIN/src/librna src
cp -r $ORIGIN/src/main/scala/v4 src
rm -r src/v4/report src/librna/rfold src/librna/misc
