run
| suite res strm |
suite := TestSuite named: 'Rowan tests'.
{RwLoadingTest.
	RwUrlTest.
	RwPackageReadWriteTest.
	RwSymbolDictionaryTest.
	RwProjectToolTest.
	RwEditToolTest.
	RwProjectSetTest.
	RwHybridBrowserToolTest.
	RwBrowserToolApiTest.
	RwRowanSample2Test.
	RwRowanProjectIssuesTest.
	RwCloneSymbolDictionaryTest.
	RwRowanIssue188Test.
	RwProjectConfigurationsTest} do: [ :cl | suite addTests: cl suite tests ].
	res := suite run.

strm := WriteStream on: String new.
strm nextPutAll: res printString; lf.
strm nextPutAll: '  errors'; lf.
res errors do: [:each |
	strm tab; nextPutAll: each printString; lf].
strm nextPutAll: '  failures'; lf.
res failures do: [:each |
	strm tab; nextPutAll: each printString; lf].
strm contents
%
