run
	| deprecationAction suite strm res resultCases systemUser |
	deprecationAction := Deprecated deprecatedAction.

	[
		suite := TestSuite named: 'DataCurator Rowan tests'.
		systemUser := AllUsers userWithId: 'SystemUser' ifAbsent: [].
		{
			systemUser objectNamed: 'RwProjectToolTest'.
		} do: [:testClass |
			suite addTests: testClass suite tests  ].
		res := suite run.

		strm := WriteStream on: String new.
		strm nextPutAll: suite name, ' for GemStone ', (System gemVersionAt: #gsVersion) printString; lf.
		strm nextPutAll: res printString; lf.
		strm nextPutAll: '  errors'; lf.
		(res errors collect: [:each | each printString ]) asArray sort do: [:each |
			strm tab; nextPutAll: each; lf].
		res failures size = 0
			ifTrue: [ ^ strm contents ].
		strm nextPutAll: '  failures'; lf.
		(res failures collect: [:each | each printString]) asArray sort do: [:each |
			strm tab; nextPutAll: each; lf].

		^ strm contents ] 
			ensure: [ 
				deprecationAction == #ignore
					ifTrue: [ Deprecated doNothingOnDeprecated ].
				deprecationAction == #log
					ifTrue: [ Deprecated doLogOnDeprecated ].
				deprecationAction == #logStack
					ifTrue: [ Deprecated doLogStackOnDeprecated ].
				deprecationAction == #error
					ifTrue: [ Deprecated doErrorOnDeprecated ].
			].
%
