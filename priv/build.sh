#!/bin/sh -e

for version in 2.7 2.6 2.5; do
	VIRTUALENV=`which virtualenv-$version 2> /dev/null || echo`
	[ -n ""$VIRTUALENV ] && break
done
[ -z $VIRTUALENV ] && VIRTUALENV=`which virtualenv`
VER=`echo $VIRTUALENV | sed -re 's/.*virtualenv-?(.[.].)?/\1/'`

cd `dirname $0`
[ -d chardet -a \
  -d erlport -a \
  -f feedparser.py -a \
  ! -d env ] && exit 0
rm -rf env chardet erlport feedparser.py *.pyc

$VIRTUALENV env

env/bin/easy_install -Z erlport
env/bin/easy_install -Z https://github.com/kurtmckee/feedparser/tarball/master
env/bin/easy_install -Z chardet

mv env/lib/*/site-packages/erlport-*/erlport .
mv env/lib/*/site-packages/feedparser-*/feedparser.py .
mv env/lib/*/site-packages/chardet-*/chardet .

patch feedparser.py << '_PATCH_'
--- feedparser.py.orig	2012-05-10 17:41:39.536888179 +0400
+++ feedparser.py	2012-05-10 17:39:06.364128637 +0400
@@ -3737,7 +3737,15 @@
     elif data[:4] == _l2bytes([0xff, 0xfe, 0x00, 0x00]):
         encoding = 'utf-32le'
         data = data[4:]
-    newdata = unicode(data, encoding)
+    try:
+        newdata = unicode(data, encoding)
+    except UnicodeDecodeError:
+        idata = unicode(data, encoding, 'ignore')
+        rdata = unicode(data, encoding, 'replace')
+        if len(rdata) - len(idata) < 32:
+            newdata = idata
+        else:
+            raise
     declmatch = re.compile('^<\?xml[^>]*?>')
     newdecl = '''<?xml version='1.0' encoding='utf-8'?>'''
     if declmatch.search(newdata):
_PATCH_

sed -i -re '1s%#!.*%#!'`which python$VER`'%' feedparser-port.py

rm -rf env

python$VER -m compileall .
