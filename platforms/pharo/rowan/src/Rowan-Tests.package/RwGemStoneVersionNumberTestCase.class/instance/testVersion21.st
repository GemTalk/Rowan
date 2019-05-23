tests
testVersion21

	self deny: (RwGemStoneVersionNumber fromString: '') > (RwGemStoneVersionNumber fromString: '0').
	self assert: (RwGemStoneVersionNumber fromString: '') < (RwGemStoneVersionNumber fromString: '0').
	self assert: (RwGemStoneVersionNumber fromString: '') = (RwGemStoneVersionNumber fromString: '').

