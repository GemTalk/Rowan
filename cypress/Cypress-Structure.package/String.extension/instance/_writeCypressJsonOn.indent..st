"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-Structure
"
_writeCypressJsonOn: aStream indent: startIndent
	"Private method which may be removed in a future GemStone version."

	aStream nextPutAll: '"'.
	CypressUrl
		writeWithHttpEscapes: (CypressObject normalizeLineEndingsOf: self)
				encodeAsUTF8
		on: aStream.
	aStream nextPutAll: '"'