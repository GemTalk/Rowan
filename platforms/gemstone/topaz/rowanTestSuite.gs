run
	| deprecationAction suite strm res projectNames |
	deprecationAction := Deprecated deprecatedAction.
	[
		Deprecated doErrorOnDeprecated.
		projectNames := #( 'Rowan' 'STON' 'Cypress' 'Tonel' ).
		"audit before load"
		projectNames do: [:projectName |
			| audit |
			audit := Rowan projectTools audit auditForProjectNamed: projectName.
			audit isEmpty ifFalse: [ self error: 'Pre load Rowan audit failed for project ', projectName printString ] ].
		projectNames do: [:projectName |
			"make sure test group is loaded ... include deprecated packages for now"
			Rowan projectTools load
				loadProjectNamed: projectName
				withGroupNames: #('tests' 'deprecated' 'jadeServer') ].
		"audit after load"
		projectNames do: [:projectName |
			| audit |
			audit := Rowan projectTools audit auditForProjectNamed: projectName.
			audit isEmpty ifFalse: [ self error: 'Post load Rowan audit failed for project ', projectName printString ] ].

		suite := Rowan projectTools test testSuiteForProjectsNamed: projectNames.

		false
			ifTrue: [ 
				[
					res := suite run.
				] on: Deprecated do: [:ex |
					ex resignalAs: (Error new messageText: ex description; yourself)
					] ]
			ifFalse: [ res := suite run ].

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
