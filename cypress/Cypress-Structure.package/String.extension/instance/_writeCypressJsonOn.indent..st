"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-Structure
"
_writeCypressJsonOn: aStream indent: startIndent
	"Private method which may be removed in a future GemStone version."

	aStream
		nextPutAll: '"';
		nextPutAll: self withUnixLineEndings encodeAsUTF8 escapePercents;
		nextPutAll: '"'