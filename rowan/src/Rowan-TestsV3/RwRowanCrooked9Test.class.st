Class {
	#name : 'RwRowanCrooked9Test',
	#superclass : 'RwAbstractRowanSampleTest',
	#category : 'Rowan-TestsV3'
}

{ #category : 'private' }
RwRowanCrooked9Test class >> _branchSessionCacheKey [
	^ #'RowanCrooked9BranchesSet'
]

{ #category : 'private' }
RwRowanCrooked9Test class >> _checkoutBranchNamed: branchName [
	"
		self _clearLoadSpecSessionCache
	"

	"The RowanCrooked9 repository is expected to be 'permanently' located on disk ... 
		we'll use `fetch` and `pull` to update the repo and branches as needed"

	"this method will clone the project (if necessary) and checkout the given branch"

	| gitTools branchPulledDict gitRoot repositoryRoot |
	gitTools := Rowan gitTools.
	gitRoot := self _testRowanProjectsSandbox.
	repositoryRoot :=  gitRoot / 'RowanCrooked9'.
	(SessionTemps current at: self _branchSessionCacheKey ifAbsent: [  ])
		ifNotNil: [ 
			"repository has been cloned"
			 ]
		ifNil: [ 
			repositoryRoot exists
				ifFalse: [ 
					| gitUrl |
					gitUrl := gitTools currentUserHasGitHubSSHKey
						ifTrue: [  'git@github.com:dalehenrich/RowanCrooked9.git' ]
						ifFalse: [  'https://github.com/dalehenrich/RowanCrooked9.git' ].
					"clone the project"
					gitTools
						gitcloneIn: gitRoot pathString
						with: gitUrl ] ].
	branchPulledDict := SessionTemps current
		at: self _gitPullSessionCacheKey
		ifAbsentPut: [ Dictionary new ].
	gitTools gitcheckoutIn: repositoryRoot with: branchName.
	(branchPulledDict at: branchName ifAbsent: [  ])
		ifNil: [ 
			"Once per session fetch and pull the latest commits for the branch"
			gitTools
				gitfetchIn: repositoryRoot with: 'origin';
				gitpullIn: repositoryRoot pathString remote: 'origin' branch: branchName;
				yourself.
			branchPulledDict at: branchName put: true ].
	^ branchName
]

{ #category : 'private' }
RwRowanCrooked9Test class >> _gitPullSessionCacheKey [
	^ #'RowanCrooked9GitBranchDict'
]

{ #category : 'private' }
RwRowanCrooked9Test class >> _loadSpecSessionCacheKey [
	^ self _branchSessionCacheKey
]

{ #category : 'private' }
RwRowanCrooked9Test >> _checkoutBranchNamed: branchName [
	^ self class _checkoutBranchNamed: branchName
]

{ #category : 'tests' }
RwRowanCrooked9Test >> testIssue783_1 [
	"Traits support -- read/write a project using Traits ... export in topaz format"

	"https://github.com/GemTalk/Rowan/issues/783"

	| loadSpec crookedProjectName readProjects resolvedProject  |
	self _checkoutBranchNamed: 'spec_0013'.
	crookedProjectName := 'RowanCrooked9'.

	{crookedProjectName}
		do: [ :pName | 
			(Rowan image loadedProjectNamed: pName ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	loadSpec := RwSpecification
		fromUrl:
			'file:'
				,
					(self _testRowanProjectsSandbox / crookedProjectName / 'rowan' / 'specs'
						/ 'spec_0013.ston') fullName.
	readProjects := loadSpec resolve read.
	resolvedProject := readProjects projectNamed: crookedProjectName.
"now we export as .gs file and tonel file"
]

{ #category : 'tests' }
RwRowanCrooked9Test >> testIssue820 [
	"https://github.com/GemTalk/Rowan/issues/820"

	| loadSpec projectName rowanSample9Project crookedProjectName crookedSourceProjectName rowanProjectsSandbox |
	loadSpec := RwRowanSample9V3Test _loadSpecNamed: 'spec_0082'.
	projectName := loadSpec projectName.
	crookedProjectName := 'RowanCrooked9_gen'.
	crookedSourceProjectName := 'RowanCrooked9'.

	{projectName.
	(crookedProjectName , '_Rowan').
	(crookedProjectName , '_RowanHybrid').
	(crookedProjectName , '_Monticello')}
		do: [ :pName | 
			(Rowan image loadedProjectNamed: pName ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	rowanSample9Project := loadSpec resolve load.

	self _checkoutBranchNamed: 'spec_0004'.

	rowanProjectsSandbox := self _testRowanProjectsSandbox.
	{'Rowan'.
	'RowanHybrid'.
	'Monticello'}
		do: [ :packageConvention | 
			| resolvedProject definedProject theProjectName |
			theProjectName := crookedProjectName , '_' , packageConvention.
			(rowanProjectsSandbox / theProjectName) ensureDeleteAll.
			definedProject := (Rowan newProjectNamed: theProjectName)
				packageConvention: packageConvention;
				gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
				repoType: #'git';
				packageFormat: 'topaz';
				projectsHome: rowanProjectsSandbox;
				addLoadComponentNamed: 'Core';
				yourself.
			(Rowan globalNamed: 'GsFileinPackager')
				toPackagesForDefinedProject: definedProject
				fromServerPath:
					(rowanProjectsSandbox / crookedSourceProjectName / 'extensions.gs') pathString.
			resolvedProject := definedProject resolveProject.
			resolvedProject write ].

	{'Rowan'.
	'RowanHybrid'.
	'Monticello'}
		do: [ :packageConvention | 
			| theProjectName loadedProject testResult audit |
			"now load/test/unload each project in turn"
			theProjectName := crookedProjectName , '_' , packageConvention.
			loadedProject := (Rowan
				projectFromUrl:
					'file:'
						,
							(rowanProjectsSandbox / theProjectName / 'rowan' / 'specs' / theProjectName
								, 'ston') pathString
				projectsHome: rowanProjectsSandbox) load projectNamed: theProjectName.
			packageConvention ~= 'Monticello'
				ifTrue: [ 
					"audit for Monticello packaage convention not yet supported (https://github.com/GemTalk/Rowan/issues/829)"
					self assert: (audit := loadedProject audit) isEmpty ].
			testResult := loadedProject testSuite run.
			self deny: testResult hasErrors.
			self deny: testResult hasFailures.
			loadedProject unload ]
]

{ #category : 'tests' }
RwRowanCrooked9Test >> testIssue896_1 [
	"https://github.com/GemTalk/Rowan/issues/896 -- filetree duplicate method defs"

	| loadSpec crookedProject crookedProjectName errorHit |
	self _checkoutBranchNamed: 'spec_0005'.
	crookedProjectName := 'RowanCrooked9'.

	{crookedProjectName}
		do: [ :pName | 
			(Rowan image loadedProjectNamed: pName ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	loadSpec := RwSpecification
		fromUrl:
			'file:'
				,
					(self _testRowanProjectsSandbox / crookedProjectName / 'rowan' / 'specs'
						/ 'spec_0005.ston') fullName.
	errorHit := false.
	[ crookedProject := loadSpec resolve load ]
		on: Error
		do: [ :ex | 
			errorHit := true.
			self
				assert:
					(ex messageText
						beginsWith:
							'Duplicate definition of olview in RowanSample9V3Class1 [1] near line 2 in file') ].
	self assert: errorHit description: 'expected error not encountered'
]

{ #category : 'tests' }
RwRowanCrooked9Test >> testIssue896_2 [
	"https://github.com/GemTalk/Rowan/issues/896 -- tonel duplicate method defs"

	| loadSpec crookedProject crookedProjectName errorHit |
	self _checkoutBranchNamed: 'spec_0006'.
	crookedProjectName := 'RowanCrooked9'.

	{crookedProjectName}
		do: [ :pName | 
			(Rowan image loadedProjectNamed: pName ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	loadSpec := RwSpecification
		fromUrl:
			'file:'
				,
					(self _testRowanProjectsSandbox / crookedProjectName / 'rowan' / 'specs'
						/ 'spec_0006.ston') fullName.
	errorHit := false.
	[ crookedProject := loadSpec resolve load ]
		on: Error
		do: [ :ex | 
			errorHit := true.
			self
				assert:
					(ex messageText
						beginsWith:
							'Duplicate definition of foo in RowanSample9V3Class1 [1] near line 17') ].
	self assert: errorHit description: 'expected error not encountered'
]

{ #category : 'tests' }
RwRowanCrooked9Test >> testIssue896_3a [
	"Different error message is signalled if the class is added 2 or more times in the incoming package load (a), than is signaled when the class is already present in the image(b)"
	"https://github.com/GemTalk/Rowan/issues/896 -- filetree duplicate class defs"

	| loadSpec crookedProject crookedProjectName errorHit |
	self _checkoutBranchNamed: 'spec_0007'.
	crookedProjectName := 'RowanCrooked9'.

	{crookedProjectName}
		do: [ :pName | 
			(Rowan image loadedProjectNamed: pName ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	loadSpec := RwSpecification
		fromUrl:
			'file:'
				,
					(self _testRowanProjectsSandbox / crookedProjectName / 'rowan' / 'specs'
						/ 'spec_0007.ston') fullName.
	errorHit := false.
	[ crookedProject := loadSpec resolve load ]
		on: Error
		do: [ :ex | 
			| errorMessage|
			errorMessage := ex messageText.
			errorHit := true.
			self
				assert:
					((errorMessage
						beginsWith:
							'Duplicate new class name #''RowanSample9V3Class1'' in symbol dictionary #''RowanSample9_1''') and: [ errorMessage includesString: 'near line' ]) ].
	self assert: errorHit description: 'expected error not encountered'
]

{ #category : 'tests' }
RwRowanCrooked9Test >> testIssue896_3b [
	"Different error message is signalled if the class is added 2 or more times in the incoming package load (a), than is signaled when the class is already present in the image(b)"
	"https://github.com/GemTalk/Rowan/issues/896 -- filetree duplicate class defs"

	| loadSpecA loadSpecB crookedProject crookedProjectName errorHit |
	self _checkoutBranchNamed: 'spec_0007'.
	crookedProjectName := 'RowanCrooked9'.

	{crookedProjectName}
		do: [ :pName | 
			(Rowan image loadedProjectNamed: pName ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	loadSpecA := RwSpecification
		fromUrl:
			'file:'
				,
					(self _testRowanProjectsSandbox / crookedProjectName / 'rowan' / 'specs'
						/ 'spec_0007a.ston') fullName.
	loadSpecB := RwSpecification
		fromUrl:
			'file:'
				,
					(self _testRowanProjectsSandbox / crookedProjectName / 'rowan' / 'specs'
						/ 'spec_0007.ston') fullName.
	crookedProject := loadSpecA resolve load.
	errorHit := false.
	[ crookedProject := loadSpecB resolve load ]
		on: Error
		do: [ :ex | 
			| errorMessage|
			errorMessage := ex messageText.
			errorHit := true.
			self
				assert:
					((errorMessage
						beginsWith:
							'Encountered an existing association for a new class RowanSample9V3Class1') and: [ errorMessage includesString: 'near line' ]) ].
	self assert: errorHit description: 'expected error not encountered'
]

{ #category : 'tests' }
RwRowanCrooked9Test >> testIssue896_4 [
	"https://github.com/GemTalk/Rowan/issues/896 -- duplicate class class definition - tonel. Technically this is a tonel format error, since we only expect a single class definition per tonel file"

	| loadSpec crookedProject crookedProjectName errorHit |
	self _checkoutBranchNamed: 'spec_0008'.
	crookedProjectName := 'RowanCrooked9'.

	{crookedProjectName}
		do: [ :pName | 
			(Rowan image loadedProjectNamed: pName ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	loadSpec := RwSpecification
		fromUrl:
			'file:'
				,
					(self _testRowanProjectsSandbox / crookedProjectName / 'rowan' / 'specs'
						/ 'spec_0008.ston') fullName.
	errorHit := false.
	[ crookedProject := loadSpec resolve load ]
		on: Error
		do: [ :ex | 
			| errorMessage|
			errorMessage := ex printString.
			errorHit := true.
			self
				assert:
					(errorMessage
						beginsWith:
							'a Error occurred (error 2710), Missing method category or class properties. Expected ${, got $C,  near line 13 in file') ].
	self assert: errorHit description: 'expected error not encountered'
]

{ #category : 'tests' }
RwRowanCrooked9Test >> testIssue896_5 [
	"https://github.com/GemTalk/Rowan/issues/896 -- invalid class category - filetree"

	| loadSpec crookedProject crookedProjectName errorHit |
	self _checkoutBranchNamed: 'spec_0009'.
	crookedProjectName := 'RowanCrooked9'.

	{crookedProjectName}
		do: [ :pName | 
			(Rowan image loadedProjectNamed: pName ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	loadSpec := RwSpecification
		fromUrl:
			'file:'
				,
					(self _testRowanProjectsSandbox / crookedProjectName / 'rowan' / 'specs'
						/ 'spec_0009.ston') fullName.
	errorHit := false.
	[ crookedProject := loadSpec resolve load ]
		on: Error
		do: [ :ex | 
			| errorMessage|
			errorMessage := ex printString.
			errorHit := true.
			self
				assert:
					(errorMessage
						beginsWith:
							'a Error occurred (error 2710), The class category ''RowanSample9V3-Core'' for the class ''RowanSample9V3Class1'' violates the package convention ''RowanHybrid'',  near line 1 in file') ].
	self assert: errorHit description: 'expected error not encountered'
]

{ #category : 'tests' }
RwRowanCrooked9Test >> testIssue896_6 [
	"https://github.com/GemTalk/Rowan/issues/896 -- duplicate class definition, filetree"

	| loadSpec crookedProject crookedProjectName errorHit |
	self _checkoutBranchNamed: 'spec_0010'.
	crookedProjectName := 'RowanCrooked9'.

	{crookedProjectName}
		do: [ :pName | 
			(Rowan image loadedProjectNamed: pName ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	loadSpec := RwSpecification
		fromUrl:
			'file:'
				,
					(self _testRowanProjectsSandbox / crookedProjectName / 'rowan' / 'specs'
						/ 'spec_0010.ston') fullName.
	errorHit := false.
	[ crookedProject := loadSpec resolve load ]
		on: Error
		do: [ :ex | 
			| errorMessage |
			errorMessage := ex messageText.
			errorHit := true.
			self
				assert:
					(errorMessage
						beginsWith:
							'Duplicate definition of RowanSample9V3Class1 in RowanSample9V3-Core [1] near line 1 in file') ].
	self assert: errorHit description: 'expected error not encountered'
]

{ #category : 'tests' }
RwRowanCrooked9Test >> testIssue920_1 [
	"https://github.com/GemTalk/Rowan/issues/920 -- case insensitive filename convention, filetree"

	| loadSpec crookedProject crookedProjectName loadedProjects definedProject classDef methodDef  |
	self _checkoutBranchNamed: 'spec_0011'.
	crookedProjectName := 'RowanCrooked9'.

	{crookedProjectName}
		do: [ :pName | 
			(Rowan image loadedProjectNamed: pName ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	loadSpec := RwSpecification
		fromUrl:
			'file:'
				,
					(self _testRowanProjectsSandbox / crookedProjectName / 'rowan' / 'specs'
						/ 'spec_0011.ston') fullName.
	loadedProjects := loadSpec resolve load.
	crookedProject := loadedProjects projectNamed: crookedProjectName.
	definedProject := crookedProject defined.
"have to modify the methods to force a write"
	classDef := ((definedProject packageNamed:  'RowanSample9V3-Core')
		classDefinitionNamed:  'RowanSample9V3Class1').
	methodDef := classDef instanceMethodDefinitions at:  #'olView'.
	methodDef source: 'olView "different selector case - with mod"'.
	methodDef := classDef instanceMethodDefinitions at:  #'olview'.
	methodDef source: 'olview "different selector case - with mod"'.
	methodDef := classDef instanceMethodDefinitions at:  #'gsmfc'.
	methodDef source: 'gsmfc "different selector case - with mod"'.
	methodDef := classDef instanceMethodDefinitions at:  #'gsMfc'.
	methodDef source: 'gsMfc "different selector case - with mod"'.
	[
		"write project do disk"
		definedProject export.
		"now reread project ... should not get duplicate definition error"
		loadedProjects := loadSpec resolve load
	] ensure: [
		Rowan gitTools performGitCommand: 'restore' in: (self _testRowanProjectsSandbox / crookedProjectName ) fullName with: ' .'  ]
]

{ #category : 'tests' }
RwRowanCrooked9Test >> testIssue920_2 [
	"https://github.com/GemTalk/Rowan/issues/920 -- preserve commentStamp class property on write"

	| loadSpec crookedProjectName readProjects definedProject classDef  |
	self _checkoutBranchNamed: 'spec_0012'.
	crookedProjectName := 'RowanCrooked9'.

	{crookedProjectName}
		do: [ :pName | 
			(Rowan image loadedProjectNamed: pName ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	loadSpec := RwSpecification
		fromUrl:
			'file:'
				,
					(self _testRowanProjectsSandbox / crookedProjectName / 'rowan' / 'specs'
						/ 'spec_0012.ston') fullName.
	readProjects := loadSpec resolve read.
	definedProject := readProjects projectNamed: crookedProjectName.
	classDef := ((definedProject packageNamed:  'RowanSample9V3-Core')
		classDefinitionNamed:  'RowanSample9V3Class1').
	self assert: (classDef propertyAt: #'commentStamp') equals: 'Tbn 11/12/2010 23:15'.
	[
		"write project do disk"
		definedProject  export.
		"now reread project ... should still see the commentStamp property"
		readProjects := loadSpec resolve read.
		definedProject := readProjects projectNamed: crookedProjectName.
		classDef := ((definedProject packageNamed:  'RowanSample9V3-Core')
			classDefinitionNamed:  'RowanSample9V3Class1').
		self assert: (classDef propertyAt: #'commentStamp') equals: 'Tbn 11/12/2010 23:15'.	
	] ensure: [
		Rowan gitTools performGitCommand: 'restore' in: (self _testRowanProjectsSandbox / crookedProjectName ) fullName with: ' .'  ]
]
