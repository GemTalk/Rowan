"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testAbsoluteFILE2
	
	url := 'fILE:/foo/bar//zookie/?fakequery/#fragger' asUrl.

	self assert: url schemeName = 'file'.
	self assert: url class = FileUrl.
	self assert: url path first ='foo'.
	self assert: url path size = 5.
	self assert: url fragment = 'fragger'.