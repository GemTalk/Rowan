"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testAsString
	| url |
	url := GenericUrl new schemeName: 'sip' locator: 'foo@bar'.
	self assert: url asString = 'sip:foo@bar'.