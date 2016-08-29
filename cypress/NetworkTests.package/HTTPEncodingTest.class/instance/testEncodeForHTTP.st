"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: as yet unclassified
"
testEncodeForHTTP

	self assert: 'aa aa éé aa aa' encodeForHTTP = 'aa%20aa%20%C3%A9%C3%A9%20aa%20aa'