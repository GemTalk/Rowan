"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testAsString
	| url |
	url := HierarchicalUrl new
		schemeName: 'ftp'
		authority: 'localhost'
		path: #('path' 'to' 'file')
		query: 'aQuery'.
	self assert: url asString = 'ftp://localhost/path/to/file?aQuery'.