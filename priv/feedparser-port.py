#!/usr/bin/python

import sys
import time
import feedparser

from erlport import Port, Protocol, String, Atom

# "" -> <<>>
# u"" -> []
def conv(term):
	if isinstance(term, dict):
		return [(Atom(key), conv(value)) for key, value in term.items() if key[-6:] != 'detail']
	if isinstance(term, tuple):
		return (conv(t) for t in term)
	if isinstance(term, list):
		return [conv(t) for t in term]
	if term is True or term is False:
		return term
	if isinstance(term, str):
		return unicode(term)
	if isinstance(term, (unicode, int, long, float)):
		return term
	if isinstance(term, time.struct_time):
		return time.strftime("%a, %d %b %Y %H:%M:%S", term)
	return unicode(term.__repr__())

class FPProtocol(Protocol):
	def handle_parse(self, data, headers):
		feed = feedparser.parse(data)
		return conv(feed)

if __name__ == "__main__":
	# sys.stderr.close()
	proto = FPProtocol()
	proto.run(Port(use_stdio=True, packet=4))
