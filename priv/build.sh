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

chdir `dirname $0`
[ -d chardet -a \
  -d erlport -a \
  -f feedparser.py ] && exit 0
rm -rf env chardet erlport feedparser.py *.pyc

$VIRTUALENV env

env/bin/easy_install erlport
env/bin/easy_install https://github.com/kurtmckee/feedparser/tarball/master
env/bin/easy_install chardet

mv env/lib/*/site-packages/erlport-*/erlport .
mv env/lib/*/site-packages/feedparser-*/feedparser.py .
mv env/lib/*/site-packages/chardet-*/chardet .

rm -rf env

python -m compileall .
