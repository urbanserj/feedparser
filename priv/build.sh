#!/bin/sh -e

cd `dirname $0`
[ -d chardet -a \
  -d erlport -a \
  -f feedparser.py -a \
  ! -d env ] && exit 0
rm -rf env chardet erlport feedparser.py *.pyc

for version in 2.7 2.6 2.5; do
	VIRTUALENV=`which virtualenv-$version 2> /dev/null || echo`
	[ -n ""$VIRTUALENV ] && break
done
[ -z $VIRTUALENV ] && VIRTUALENV=`which virtualenv 2> /dev/null`
[ -z $VIRTUALENV ] && \
	(echo virtualenv: command not found >&2 && exit 1)
VER=`echo $VIRTUALENV | sed -re 's/.*virtualenv-?(.[.].)?/\1/'`

$VIRTUALENV env

env/bin/easy_install -Z erlport
env/bin/easy_install -Z https://github.com/kurtmckee/feedparser/tarball/dc3bd29
env/bin/easy_install -Z chardet

mv env/lib/*/site-packages/erlport-*/erlport .
mv env/lib/*/site-packages/feedparser-*/feedparser.py .
mv env/lib/*/site-packages/chardet-*/chardet .

patch feedparser.py << '_PATCH_'
--- feedparser.py.orig	2012-05-29 16:16:45.456262539 +0400
+++ feedparser.py	2012-05-29 16:16:51.676293399 +0400
@@ -3750,7 +3750,15 @@
             continue
         tried_encodings.append(proposed_encoding)
         try:
-            data = data.decode(proposed_encoding)
+            try:
+                data = unicode(data, proposed_encoding)
+            except UnicodeDecodeError:
+                idata = unicode(data, proposed_encoding, 'ignore')
+                rdata = unicode(data, proposed_encoding, 'replace')
+                if len(rdata) - len(idata) < 32:
+                    data = idata
+                else:
+                    raise
         except (UnicodeDecodeError, LookupError):
             pass
         else:
_PATCH_

sed -i -re '1s%#!.*%#!'`which python$VER`'%' feedparser-port.py

rm -rf env

python$VER -m compileall .
