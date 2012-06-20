#!/usr/bin/python

import sys
import time
import feedparser

from erlport import Port, Protocol, String, Atom

# "" -> <<>>
# u"" -> []
def conv(term, root, binary):
	if isinstance(term, dict) and root in ['namespaces', 'headers']:
		return [ ( conv(key, root, binary), conv(value, root, binary) )
			for key, value in term.items() ]
	if isinstance(term, dict):
		return [ ( Atom(key), conv(value, key, binary) )
			for key, value in term.items()
			if value and key in [
				'author', 'author_detail', 'base', 'bozo',
				'bozo_exception', 'cloud', 'comments', 'content',
				'contributors', 'created', 'description', 'docs',
				'domain', 'email', 'enclosures', 'encoding',
				'entries', 'errorreportsto', 'etag', 'expired',
				'feed', 'generator', 'headers', 'height', 'href',
				'icon', 'id', 'image', 'info', 'label', 'language',
				'length', 'license', 'link', 'links', 'logo', 'name',
				'namespaces', 'path', 'port', 'protocol', 'published',
				'publisher', 'ref', 'registerProcedure', 'relationships',
				'rights', 'scheme', 'source', 'subtitle', 'summary',
				'tags', 'term', 'textinput', 'title', 'ttl', 'type',
				'updated', 'value', 'vcard', 'version', 'width', 'xfn'
			] ]
	if isinstance(term, tuple):
		return (conv(t, root, binary) for t in term)
	if isinstance(term, list):
		return [conv(t, root, binary) for t in term]
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
		finally: return (Atom('ok'), conv(feed, None, stype == Atom('binary')))


if __name__ == "__main__":
	sys.stderr.close()
	proto = FPProtocol()
	proto.run(Port(use_stdio=True, packet=4))
