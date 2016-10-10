"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testAbsoluteFILE
	
	url := Url absoluteFromText: 'file:/etc/passwd#foo'.

	self assert: url schemeName = 'file'.
	self assert: url path first = 'etc'.
	self assert: url path size = 2.	
	self assert: url fragment = 'foo'.