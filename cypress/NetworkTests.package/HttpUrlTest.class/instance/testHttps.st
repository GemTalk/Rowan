"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testHttps
	self assert: 'https://encrypted.google.com' asUrl scheme = 'https'.