test approximately greater than
testApproxVersion05

	self assert: '3.2.16' asRwGemStoneVersionNumber ~> '3.2.15' asRwGemStoneVersionNumber.
	self deny: '3.2.14' asRwGemStoneVersionNumber ~> '3.2.15' asRwGemStoneVersionNumber.
	self deny: '3.3' asRwGemStoneVersionNumber ~> '3.2.15' asRwGemStoneVersionNumber.
	self deny: '3.3.0' asRwGemStoneVersionNumber ~> '3.2.15' asRwGemStoneVersionNumber.

	self assert: '3.2.15.5' asRwGemStoneVersionNumber ~> '3.2.15.1' asRwGemStoneVersionNumber.
	self deny: '3.2.15.0' asRwGemStoneVersionNumber ~> '3.2.15.1' asRwGemStoneVersionNumber.
	self deny: '3.2.16' asRwGemStoneVersionNumber ~> '3.2.15.1' asRwGemStoneVersionNumber.
	self deny: '3.3' asRwGemStoneVersionNumber ~> '3.2.15.1' asRwGemStoneVersionNumber.
	self deny: '3.3.0' asRwGemStoneVersionNumber ~> '3.2.15.1' asRwGemStoneVersionNumber.

	self assert: '3.3' asRwGemStoneVersionNumber ~> '3.2' asRwGemStoneVersionNumber.
	self assert: '3.3.0' asRwGemStoneVersionNumber ~> '3.2' asRwGemStoneVersionNumber.
	self assert: '3.3.1' asRwGemStoneVersionNumber ~> '3.2' asRwGemStoneVersionNumber.
	self assert: '3.4' asRwGemStoneVersionNumber ~> '3.2' asRwGemStoneVersionNumber.

	self assert: '3.3.1' asRwGemStoneVersionNumber ~> '3.2.0' asRwGemStoneVersionNumber.
	self assert: '3.3.0' asRwGemStoneVersionNumber ~> '3.2.0' asRwGemStoneVersionNumber.
	self assert: '3.3' asRwGemStoneVersionNumber ~> '3.2.0' asRwGemStoneVersionNumber.
