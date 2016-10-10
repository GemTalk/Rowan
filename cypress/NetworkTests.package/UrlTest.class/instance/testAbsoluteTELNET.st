"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testAbsoluteTELNET
	
	url := 'telNet:chaos.resnet.gatech.edu#goo' asUrl.

	self assert: url schemeName = 'telnet'.
	self assert: url locator = 'chaos.resnet.gatech.edu'.
	self assert: url fragment = 'goo'.	
