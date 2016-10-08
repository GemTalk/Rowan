"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Testing
"
isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #NetworkTestCase
