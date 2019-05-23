test approximately greater than
testApproxVersion04

	self assert: '3.2.0' asRwGemStoneVersionNumber approximateBase asString = '4'.
	self assert: '3.2' asRwGemStoneVersionNumber approximateBase asString = '4'.
