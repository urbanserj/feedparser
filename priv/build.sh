#!/bin/sh -e

for filename in /usr/bin/virtualenv-2.7 /usr/bin/virtualenv-2.6 /usr/bin/virtualenv-2.5; do
	if [ -f $filename ]; then
		VIRTUALENV=$filename
		break
	fi
done
if [ -z $VIRTUALENV ]; then
	VIRTUALENV=/usr/bin/virtualenv
fi

ENVDIR="`dirname $0`/env"
[ -d $ENVDIR ] && exit 0
$VIRTUALENV $ENVDIR
$ENVDIR/bin/easy_install erlport
$ENVDIR/bin/easy_install https://github.com/kurtmckee/feedparser/tarball/master
$ENVDIR/bin/easy_install chardet
rm -f $ENVDIR/local
