"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testRelativeHTTP
	
	baseUrl := 'http://some.where/some/dir?query1#fragment1' asUrl.
	url := baseUrl newFromRelativeText: '../another/dir/?query2#fragment2'.

	self assert: url asString =  'http://some.where/another/dir/?query2#fragment2'.