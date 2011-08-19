#!/bin/sh
ENVDIR="`dirname $0`/env"
[ -d $ENVDIR ] && exit 0
virtualenv $ENVDIR
$ENVDIR/bin/easy_install erlport
$ENVDIR/bin/easy_install feedparser
$ENVDIR/bin/easy_install chardet
