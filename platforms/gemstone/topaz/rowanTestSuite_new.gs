run
	| suite strm res projectNames |
	projectNames := #( 'Rowan' 'STON' 'Cypress' 'Tonel' ).
	projectNames do: [:projectName |
		Rowan projectTools load
			loadProjectNamed: projectName
			withGroupNames: #('tests') ].
	suite := Rowan projectTools test testSuiteForProjectsNamed: projectNames.
	res := suite run.
	strm := WriteStream on: String new.
  strm nextPutAll: suite name; lf.
	strm nextPutAll: res printString; lf.
	strm nextPutAll: '  errors'; lf.
	(res errors collect: [:each | each printString ]) asArray sort do: [:each |
		strm tab; nextPutAll: each; lf].
	res failures size = 0
		ifTrue: [ ^ strm contents ].
	strm nextPutAll: '  failures'; lf.
		(res failures collect: [:each | each printString]) asArray sort do: [:each |
		strm tab; nextPutAll: each; lf].
	strm contents
%
