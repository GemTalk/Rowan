tests
testVersion24

	self assert: (RwGemStoneVersionNumber fromString: '1.0.0.1.0.0') = (RwGemStoneVersionNumber fromString: '1.0.0.1').
	self assert: (RwGemStoneVersionNumber fromString: '1.0.0.1') ~= (RwGemStoneVersionNumber fromString: '1..1').
