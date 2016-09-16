"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-MesssageDigest
"
addToDigest: aMessageDigestStream

	aMessageDigestStream
		tab; tab; tab; tab; nextPutAll: self class name; cr;
		tab; tab; tab; tab; tab; nextPutAll: self selector; cr;
		tab; tab; tab; tab; tab; nextPutAll: 'properties:'; cr;
		tab; tab; tab; tab; tab; tab.
	self properties writeCypressJsonOn: aMessageDigestStream indent: 6.
	aMessageDigestStream
		cr;
		tab; tab; tab; tab; nextPutAll: 'source:'; nextPutAll: self source; cr.
