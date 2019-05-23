smalltalk api
testSuiteForProjectsNamed: projectNames

	| suite strm |
	strm := WriteStream on: String new.
	projectNames do: [:projectName | strm nextPutAll: projectName; space ].
	strm nextPutAll:  'tests'.
	suite := TestSuite named: strm contents.
	projectNames do: [:projectName | self _addTestsForProjectNamed: projectName toTestSuite: suite ].
	^ suite
