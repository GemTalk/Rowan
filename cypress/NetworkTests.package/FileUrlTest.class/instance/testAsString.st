"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testAsString
	| target url |
	target := 'file://localhost/etc/rc.conf'.
	url := target asUrl.
	self assert: url asString = target.
		