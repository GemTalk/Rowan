run
	| deprecationAction suite strm res projectNames includeDeprecatedPackages warnings resultsDict resultCases deprecationErrors |

	includeDeprecatedPackages := (System stoneVersionReport at: 'gsVersion') = '3.2.15' 
		ifTrue: [
			"3.2.15 needs deprecated packages loaded to function"
			false ]
		ifFalse: [
			"3.5.0 can run with or without deprecated packges"
			false ].

	deprecationAction := Deprecated deprecatedAction.
	warnings := {}.
	[
		(Rowan projectNamed: 'Rowan') loadProjectSet.	"until RowanClientServices project is being loaded by 3.6.2/3.7.0"
		projectNames := #( 'Rowan' 'STON' 'Cypress' 'RowanClientServices' ).
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
				Rowan projectTools loadV2
					loadProjectNamed: projectName
					customConditionalAttributes: #('tests' 'deprecated') ]
			ifFalse: [
				"make sure test group is loaded ... do NOT include deprecated packages"
				Rowan projectTools loadV2
					loadProjectNamed: projectName
					customConditionalAttributes: #('tests' 'testsV2') ] ]
					on: CompileWarning do: [:ex |
						(ex description includesString: 'not optimized')
							ifFalse: [ warnings add: ex asString printString ].
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

		(true
			and: [(System stoneVersionReport at: 'gsVersion') >= '3.5.0']) 
				ifTrue: [ Deprecated doErrorOnDeprecated ].

		suite := Rowan projectTools test testSuiteForProjectsNamed: projectNames.

		false
			ifTrue: [ 
				"custom test debugging"
				false
					ifTrue:  [
						"bring up debugger on Deprecated exception"
						res := suite run ]
					ifFalse: [
						"debug selectors that only fail during full test run"
						res := TestResult new.
						suite tests do: [:each | 
							each selector == #'testIssue295_rename_package_move_newClassVersion_newProject_3'
								ifTrue: [ each debug ]
								ifFalse: [ 
									[ each run: res ] 
										on: Deprecated do: [:ex |
											ex resignalAs: (Error new messageText: ex description; yourself)
										] ] ] ] ]
			ifFalse: [ 
				"standard test suite run"
				deprecationErrors := 0.
				[ 
					res := suite run.
				] on: Deprecated do: [:ex |
					deprecationErrors := deprecationErrors + 1.
					ex resignalAs: (Error new messageText: ex description; yourself)
					] ].

		resultsDict := Dictionary new.
		resultCases := {}.
		resultsDict 
			at: #suiteName put: suite name;
			at: #timeStamp put: DateAndTime now printString;
			at: #properties put: (Dictionary new
				at: #includeDeprecatedPackages put: includeDeprecatedPackages;
				at: #deprecatedAction put: Deprecated deprecatedAction;
				yourself);
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

		strm nextPutAll: res printString; nextPutAll: ', ', deprecationErrors printString, ' deprecationErrors'; lf.
		strm nextPutAll: '  errors'; lf.
		(res errors collect: [:each |  
			resultCases add: (Dictionary new
					at: #className put: each class asString;
					at: #selector put: each selector asString;
					at: #status put: 'errors';
					yourself). 
			each printString ]) asArray sort do: [:each |
			strm tab; nextPutAll: each; lf].
		res failures size > 0
			ifTrue: [
				strm nextPutAll: '  failures'; lf.
				(res failures collect: [:each | 
					resultCases add: (Dictionary new
							at: #className put: each class asString;
							at: #selector put: each selector asString;
							at: #status put: 'failures';
							yourself). 
					each printString]) asArray sort do: [:each |
					strm tab; nextPutAll: each; lf ] ].

		(FileSystem workingDirectory / 'testResults', 'json')
			writeStreamDo: [:fileStream | STON put: resultsDict asJsonOnStreamPretty: fileStream ].

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

time
