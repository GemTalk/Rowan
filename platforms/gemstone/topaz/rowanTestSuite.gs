run
	| deprecationAction suite strm res projectNames warnings resultsDict resultCases includeDeprecatedPackages |

	includeDeprecatedPackages := (System stoneVersionReport at: 'gsVersion') = '3.2.15' 
		ifTrue: [
			"3.2.15 needs deprecated packages loaded to function"
			true ]
		ifFalse: [
			"3.6.1 needs deprecated packages loaded to function ... for now"
			true ].

	deprecationAction := Deprecated deprecatedAction.
	warnings := {}.
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
			[ 
			| groupNames |
			groupNames := includeDeprecatedPackages
				ifTrue: [ #('tests' 'deprecated' 'jadeServer') ]
				ifFalse: [ #('tests' 'jadeServer') ].
			Rowan projectTools load
				loadProjectNamed: projectName
				withGroupNames: groupNames ]
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
			audit isEmpty ifFalse: [ self error: 'Post load Rowan audit failed for project ', projectName printString ] ].

		suite := Rowan projectTools test testSuiteForProjectsNamed: projectNames.
		false
			ifTrue: [
				res := TestResult new.
				suite tests do: [:each | 
					((each class name asString = 'RwRowanProjectIssuesTest')
						and: [	#(testIssue72_addMethod testIssue72_removeMethod testIssue72_updateMethod) includes: each selector])
							ifTrue: [ each debug ]
							ifFalse: [ each run: res ] ]
				]
			ifFalse: [ res := suite run ].

		resultsDict := Dictionary new.
		resultCases := {}.
		resultsDict 
			at: #suiteName put: suite name;
			at: #timeStamp put: DateAndTime now printString;
			at: #properties put: Dictionary new;
			at: #notes put: '';
			at: #gsVersion put: (System gemVersionAt: #gsVersion);
			at: #testCases put: resultCases;
			at: #resultsSummary put: (Dictionary new
				at: #summary put: res printString;
				at: #failures put: res failureCount;
				at: #errors put: res errorCount;
				at: #passed put: res passedCount;
				yourself)
			yourself.

		strm := WriteStream on: String new.
		strm nextPutAll: suite name, ' for GemStone ', (System gemVersionAt: #gsVersion) printString; lf.
		res passed do: [:each |
			resultCases add: (Dictionary new
					at: #className put: each class asString;
					at: #selector put: each selector asString;
					at: #status put: 'passed';
					yourself) ].

		strm nextPutAll: res printString; lf.
		strm nextPutAll: '  errors'; lf.
		(res errors collect: [:each |  
			resultCases add: (Dictionary new
					at: #className put: each class asString;
					at: #selector put: each selector asString;
					at: #status put: 'errors';
					yourself). 
			each printString ]) asArray sort do: [:each |
			strm tab; nextPutAll: each; lf].
		res failures size = 0
			ifTrue: [ ^ strm contents ].
		strm nextPutAll: '  failures'; lf.
		(res failures collect: [:each | 
			resultCases add: (Dictionary new
					at: #className put: each class asString;
					at: #selector put: each selector asString;
					at: #status put: 'failures';
					yourself). 
			each printString]) asArray sort do: [:each |
			strm tab; nextPutAll: each; lf].

		Rowan fileUtilities
			writeStreamFor: 'testResults.json'
			in: '$PWD/'
			do: [:fileStream | STON put: resultsDict asJsonOnStreamPretty: fileStream ].

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
