#!/usr/bin/python

import sys
import time
import feedparser

from erlport import Port, Protocol, String, Atom

# "" -> <<>>
# u"" -> []
def conv(term, binary):
	if isinstance(term, dict):
		return [ (Atom(key), conv(value, binary)) for key, value in term.items()
			if value and (key[-6:] not in ['detail', 'parsed'] or key in ['author_detail']) ]
	if isinstance(term, tuple):
		return (conv(t, binary) for t in term)
	if isinstance(term, list):
		return [conv(t, binary) for t in term]
	if term is True or term is False:
		return term
	if isinstance(term, str):
		if binary: return term
		return term.decode('utf8')
	if isinstance(term, unicode):
		if not binary: return term
		return term.encode('utf8')
	if isinstance(term, (int, long, float)):
		return term
	if isinstance(term, time.struct_time):
		return time.strftime("%a, %d %b %Y %H:%M:%S", term)
	term = term.__repr__()
	if binary: return term.encode('utf8')
	return term.decode('utf8')

class FPProtocol(Protocol):
	def handle_parse(self, data, headers, stype):
		feed = []
		try: feed = feedparser.parse(data, response_headers=dict(headers))
		except: return (Atom('error'), Atom('internal'))
		finally: return (Atom('ok'), conv(feed, stype == Atom('binary')))


if __name__ == "__main__":
	sys.stderr.close()
	proto = FPProtocol()
	proto.run(Port(use_stdio=True, packet=4))
