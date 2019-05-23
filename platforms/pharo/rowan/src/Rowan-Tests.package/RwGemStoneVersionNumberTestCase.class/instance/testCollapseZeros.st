tests
testCollapseZeros

	self assert: (RwGemStoneVersionNumber fromString: '1.0') collapseZeros printString = '1'.
	self assert: (RwGemStoneVersionNumber fromString: '1.0') collapseZeros printString = '1'.
	self assert: (RwGemStoneVersionNumber fromString: '1.0.0') collapseZeros printString = '1'.
