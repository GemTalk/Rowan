run
	| deprecationAction suite strm res projectNames includeDeprecatedPackages warnings |

	includeDeprecatedPackages := false. "true means load deprecated packages"

	deprecationAction := Deprecated deprecatedAction.
	warnings := {}.
	[
		Deprecated doErrorOnDeprecated.
		projectNames := #( 'Rowan' 'STON' 'Cypress' 'Tonel' 'FileSystemGs' ).
		"audit before load"
		projectNames do: [:projectName |
			| audit |
			audit := Rowan projectTools audit auditForProjectNamed: projectName.
			audit isEmpty ifFalse: [ self error: 'Pre load Rowan audit failed for project ', projectName printString ] ].
		projectNames do: [:projectName |
			[			
			includeDeprecatedPackages
			ifTrue: [
				"make sure test group is loaded ... include deprecated packages"
				Rowan projectTools load
					loadProjectNamed: projectName
					withGroupNames: #('tests' 'deprecated' 'jadeServer' 'deprecated tests') ]
			ifFalse: [
				"make sure test group is loaded ... do NOT include deprecated packages"
				Rowan projectTools load
					loadProjectNamed: projectName
					withGroupNames: #('tests' 'jadeServer') ] ]
					on: CompileWarning do: [:ex |
						warnings add: ex asString printString.
						ex resume ] ].

 		warnings isEmpty ifFalse: [
			GsFile gciLogServer: 'COMPILE WARNINGS: '.
			warnings do: [:warning | GsFile gciLogServer: '	', warning ]. 
			self error: 'Warnings during project load' ]. 

		System commit. "do a commit immediately after test code is loaded, to avoid losing test classes if a test does an apport"

		"audit after load"
		projectNames do: [:projectName |
			| audit |
			audit := Rowan projectTools audit auditForProjectNamed: projectName.
			audit isEmpty ifFalse: [ self error: 'Post load Rowan audit failed for project ', projectName printString ].
 ].

		suite := Rowan projectTools test testSuiteForProjectsNamed: projectNames.

		true
			ifTrue: [ 
				"Deprection error will halt tests immediately"
				res := suite run ]
			ifFalse: [ 
				[ 
					res := suite run.
				] on: Deprecated do: [:ex |
					ex resignalAs: (Error new messageText: ex description; yourself)
					] ].

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
