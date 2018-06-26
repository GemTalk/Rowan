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
	RwProjectConfigurationsTest.
	RwSemanticVersionNumber200TestCase.
	RwSemanticVersionNumberTestCase.
	RwGemStoneVersionNumberTestCase.
	RwRowanSample4Test.
	RwAdoptToolApiTest.
	RwProjectToolTest.
	RwDisownToolApiTest.} do: [ :cl | suite addTests: cl suite tests ].
	res := suite run.

strm := WriteStream on: String new.
strm nextPutAll: res printString; lf.
strm nextPutAll: '  errors'; lf.
(res errors collect: [:each | each printString ]) asArray sort do: [:each |
	strm tab; nextPutAll: each; lf].
strm nextPutAll: '  failures'; lf.
(res failures collect: [:each | each printString]) asArray sort do: [:each |
	strm tab; nextPutAll: each; lf].
strm contents
%
