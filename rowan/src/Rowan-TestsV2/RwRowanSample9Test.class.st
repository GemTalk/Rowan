Class {
	#name : 'RwRowanSample9Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwRowanSample9Test class >> _clearLoadSpecSessionCache [
	"self _clearLoadSpecSessionCache"

	SessionTemps current removeKey: self _loadSpecSessionCacheKey ifAbsent: [  ].
	SessionTemps current removeKey: self _gitPullSessionCacheKey ifAbsent: [  ]
]

{ #category : 'private' }
RwRowanSample9Test class >> _gitPullSessionCacheKey [
	^ #'RowanSample9GitBranchDict'
]

{ #category : 'private' }
RwRowanSample9Test class >> _loadSpecNamed: specName [
	"
		self _clearLoadSpecSessionCache
	"

	"The RowanSample9 repository is expected to be 'permanently' located on disk ... 
		we'll use `fetch` and `pull` to update the repo and branches as needed"

	"it is assumed that the load spec on the master branch and the load spec on the branch are the same"

	| loadSpecification resolvedProject resolvedRepository dict theSpec branchDict |
	loadSpecification := self _rowanSample9_0000_load_spec.
	theSpec := (SessionTemps current
		at: self _loadSpecSessionCacheKey
		ifAbsent: [  ])
		ifNotNil: [ :specsDict | specsDict at: specName ]
		ifNil: [ 
			"The first time we access the repository in this session, we'll refresh the 
				master branch and cache the session specs"
			loadSpecification revision: 'master'.
			resolvedProject := loadSpecification resolveStrict.

			resolvedRepository := resolvedProject repository.
			resolvedRepository
				fetch;
				pull: resolvedRepository remote branch: 'master';
				yourself.

			dict := SessionTemps current
				at: self _loadSpecSessionCacheKey
				ifAbsentPut: [ Dictionary new ].
			(resolvedProject repositoryRoot / 'specs') files
				do: [ :file | 
					file
						readStreamDo: [ :fileStream | 
							| stream spec |
							stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
							spec := STON fromStream: stream.
							dict at: spec specName put: spec ] ].
			dict at: specName ].
	theSpec := theSpec copy.
	theSpec
		projectsHome: self _testRowanProjectsSandbox;
		yourself.
	resolvedProject := theSpec resolveStrict.	"When we reference a spec, we'll checkout the branch"
	branchDict := SessionTemps current
		at: self _gitPullSessionCacheKey
		ifAbsentPut: [ Dictionary new ].
	(branchDict at: theSpec revision ifAbsent: [  ])
		ifNil: [ 
			"Once per session fetch and pull the latest commits for the branch"
			resolvedRepository := resolvedProject repository.
			resolvedRepository
				fetch;
				pull: resolvedRepository remote branch: theSpec revision;
				yourself.
			branchDict at: theSpec revision put: theSpec ].
	^ theSpec copy
]

{ #category : 'private' }
RwRowanSample9Test class >> _loadSpecSessionCacheKey [
	^ #'RowanSample9LoadSpecsDict'
]

{ #category : 'private' }
RwRowanSample9Test class >> _rowanSample9_0000_load_spec [
	| rowanProject fileUrl |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	fileUrl := 'file:' , rowanProject repositoryRootPath
		, '/test/specs/RowanSample9_0000.ston'.
	^ (RwSpecification fromUrl: fileUrl)
		projectsHome: self _testRowanProjectsSandbox;
		yourself
]

{ #category : 'private' }
RwRowanSample9Test >> _deleteProjectRepositoryDir [
	^ false
]

{ #category : 'private' }
RwRowanSample9Test >> _gitPullSessionCacheKey [
	^ self class _gitPullSessionCacheKey
]

{ #category : 'private' }
RwRowanSample9Test >> _issue_527_resolve_load_validate: projectSpec className: className expectedSymDictName: expectedSymDictName [
	| resolvedProject loadedProjects symDictName symDict |
	resolvedProject := projectSpec resolve.
	loadedProjects := resolvedProject load.
	self assert: loadedProjects size = 1.
	loadedProjects do: [ :project | self assert: project audit isEmpty ].

	symDictName := projectSpec
		gemstoneDefaultSymbolDictNameForUser: Rowan image currentUserId.

	self assert: symDictName = expectedSymDictName.

	(symDict := Rowan globalNamed: symDictName)
		ifNil: [ 
			self
				assert: false
				description:
					'Expected symbol dictionary ' , symDictName printString , ' not present' ].

	self assert: (symDict at: className asSymbol ifAbsent: [  ]) notNil.

	(loadedProjects at: 1) unload.

	self deny: (symDict at: className asSymbol ifAbsent: [  ]) notNil
]

{ #category : 'private' }
RwRowanSample9Test >> _loadSpecNamed: specName [
	^ self class _loadSpecNamed: specName
]

{ #category : 'private' }
RwRowanSample9Test >> _loadSpecSessionCacheKey [
	^ self class _loadSpecSessionCacheKey
]

{ #category : 'private' }
RwRowanSample9Test >> _rowanSample9_0000_load_spec [
	^ self class _rowanSample9_0000_load_spec
]

{ #category : 'private' }
RwRowanSample9Test >> _specListProjectNames [
	^ super _specListProjectNames
		,
			{'PrimerSample9'.
			'RowanSample9_testbed'}
]

{ #category : 'private' }
RwRowanSample9Test >> _standard_resolve_load_validate: projectSpec [
	| resolvedProject loadedProjects |

"resolve/load"
	resolvedProject := projectSpec resolve.
	loadedProjects := resolvedProject load.

	self _standard_validate: resolvedProject loadedProjects: loadedProjects
]

{ #category : 'private' }
RwRowanSample9Test >> _standard_validate: resolvedProject loadedProjects: loadedProjects [ 
	| testResult loadedProjectDefinition x |
	"validate"
	self assert: loadedProjects size = 1.
	loadedProjects
		do: [ :project | 
			self assert: (x := project audit) isEmpty.
			testResult := project testSuite run.
			self deny: testResult hasErrors.
			self deny: testResult hasFailures.

			loadedProjectDefinition := project asDefinition.
			self
				assert:
					loadedProjectDefinition class = resolvedProject projectDefinition class ]
]

{ #category : 'private' }
RwRowanSample9Test >> _standard_validate: resolvedProject loadedProjects: loadedProjects expectedProjectNames: expectedProjectNames [
	| testResult loadedProjectDefinition x |
	"validate"
	self assert: loadedProjects size = expectedProjectNames size.
	self
		assert: (loadedProjects collect: [ :each | each name ]) sort
		equals: expectedProjectNames sort.
	loadedProjects
		do: [ :project | 
			self assert: (x := project audit) isEmpty.
			testResult := project testSuite run.
			self deny: testResult hasErrors.
			self deny: testResult hasFailures.

			loadedProjectDefinition := project asDefinition.
			self
				assert:
					loadedProjectDefinition class = resolvedProject projectDefinition class ]
]

{ #category : 'private' }
RwRowanSample9Test >> _validateSpec_0017_componentStructure: resolvedProject [
	self
		_validateComponent: (resolvedProject componentNamed: 'Core')
			condition: {'common'}
			group: 'core'
			componentNames: {}
			packageNames:
				{'RowanSample9-Core'.
				'RowanSample9-Extensions'.
				'RowanSample9-Extensions1'}
			projectNames: {};
		_validateComponent: (resolvedProject componentNamed: 'Core')
			condition: {'common'}
			group: 'tests'
			componentNames: {}
			packageNames: {'RowanSample9-Tests'}
			projectNames: {};
		yourself
]

{ #category : 'private' }
RwRowanSample9Test >> _validateSpec_0021_componentStructure: resolvedProject [
	self
		_validateComponent: (resolvedProject componentNamed: 'Core')
			condition: {'common'}
			group: 'core'
			componentNames: {}
			packageNames:
				{'RowanSample9-Core'.
				'RowanSample9-Extensions'}
			projectNames: {};
		_validateComponent: (resolvedProject componentNamed: 'Core')
			condition: {'common'}
			group: 'tests'
			componentNames: {}
			packageNames: {'RowanSample9-Tests'}
			projectNames: {};
		yourself
]

{ #category : 'private' }
RwRowanSample9Test >> _validateSymbolDictMapping: classesToSymDictMap [
	| exceptions |
	exceptions := {}.
	classesToSymDictMap
		keysAndValuesDo: [ :classNames :symDictName | 
			| symDict |
			(symDict := Rowan globalNamed: symDictName)
				ifNil: [ 
					exceptions
						add:
							'Expected symbol dictionary ' , symDictName printString , ' not present'.
					classNames
						do: [ :className | 
							"if expected symbol dictionary is not present, confirm that the classes were loaded albeit, the wrong symbol dictionary"
							(Rowan globalNamed: className)
								ifNil: [ 
									exceptions
										add:
											'Class named ' , className printString , ' not loaded as expected' ] ] ]
				ifNotNil: [ 
					classNames
						do: [ :className | 
							(symDict at: className asSymbol ifAbsent: [  ])
								ifNil: [ 
									exceptions
										add:
											'Class named ' , className printString
												, ' not present in the expected symbol dictionary '
												, symDictName printString ] ] ] ].
	self assert: exceptions isEmpty
]

{ #category : 'tests' }
RwRowanSample9Test >> testIndexCards [
	| projectSpecification indexCards specsDir projectSpecs resolvedProjects |
	projectSpecification := self _rowanSample9_0000_load_spec.
	projectSpecification repositoryRoot ensureDeleteAll.
	projectSpecification revision: 'master'.
	projectSpecification resolveStrict.

	indexCards := (projectSpecification repositoryRoot / 'indexCards') files
		collect: [ :file | 
			file
				readStreamDo: [ :fileStream | 
					| stream |
					stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
					STON fromStream: stream ] ].

	indexCards := indexCards sort: [ :a :b | a index <= b index ].

	specsDir := projectSpecification repositoryRoot / 'specs'.
	projectSpecs := indexCards
		collect: [ :indexCard | 
			specsDir / indexCard specName , 'ston'
				readStreamDo: [ :fileStream | 
					| stream |
					stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
					STON fromStream: stream ] ].

	resolvedProjects := projectSpecs
		collect: [ :projectSpec | 
			projectSpec projectsHome: self _testRowanProjectsSandbox.
			STON toStringPretty: projectSpec resolve ]
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_254 [
	"https://github.com/dalehenrich/Rowan/issues/254"

	| loadSpec projectName resolvedProject loadedProjects project testResult 
		packageName1 packageName2 |

	loadSpec := self _loadSpecNamed: 'spec_0003'.	"primer -- spec_0001 with instancesInvariant"
	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed:projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"set up for issue #230 ... RowanSample9Class1 in one symbol dictionary (instancesInvariant);
		RowanSample4IdentityKeyValueDictionary in another symbol dictionary"
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.

"resolve primer project"
	resolvedProject := loadSpec resolve.
	resolvedProject
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 
			forPackageNamed: packageName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 
			forPackageNamed: packageName2;
		yourself.

"load primer project"
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: project audit isEmpty.
	testResult := project testSuite run.
	self deny: testResult hasErrors.
	self deny: testResult hasFailures.

	loadSpec := self _loadSpecNamed: 'spec_0002'.	"basic project (remove instancesInvariant option and move class to default symbol dictionary: self _sampleSymbolDictionaryName1)"

"resolve/load"
	resolvedProject := loadSpec resolve.
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: project audit isEmpty.
	testResult := project testSuite run.
	self deny: testResult hasErrors.
	self deny: testResult hasFailures.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_446 [
	"Not a real test ... used for V2.0 spec development"

	"https://github.com/GemTalk/Rowan/issues/446"

	"create a project that has 2 components, one component 
		requires the other component; and two load specs, each spec
		loads a different component ... needed to consolidate spec_004 
		creation"

	| project1 project2 projectName projectsHome resolvedProject stonString |
	projectName := 'RowanSample9_testbed'.
	projectsHome := self _testRowanProjectsSandbox.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

	project1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		repoType: #'git';
		yourself.

	resolvedProject := project1 resolve.

	(resolvedProject
		addComponentNamed: 'Core1'
		definedGroupNames:
			(Dictionary new
				add: 'core' -> {};
				add: 'tests' -> {'core'};
				yourself)
		comment: 'contains ' , projectName , '-Core1 package and Core2 component')
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addComponentNames: #('Core2');
		yourself.
	resolvedProject
		addComponentNamed: 'Core2'
		definedGroupNames:
			(Dictionary new
				add: 'core' -> {};
				add: 'tests' -> {'core'};
				yourself)
		comment: 'contains ' , projectName , '-Core2 package'.

	resolvedProject
		componentNames: #('Core1');
		yourself.

	project2 := RwResolvedProjectV2 new
		componentNames: #('Core2');
		yourself.

	stonString := STON toStringPretty: resolvedProject.
	false
		ifTrue: [ self halt ]
]

{ #category : 'issue 493' }
RwRowanSample9Test >> testIssue_493 [
	"https://github.com/dalehenrich/Rowan/issues/493"

	"duplicate of RwRowanSample2Test debug: #testIssue493 (no existing LoadedMethod found for the old compiledMethod.) ... "

	"Cannot duplicate RwRowanSample4Test>>#testLoadProjectNamed_493 (passing in 3.5.0) because: in v1.2 the default symbol 
		dictionary was a function of the component, so a component without a default
		explicitly defined, gave a different answer as to the expected symbol dict for 
		packages without explicitly defined symbol dicts ... in 3.2.15, the test passes, when deprecated methods are loaded"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0008'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project soec_0008"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

"spec_0010"
	loadSpec := self _loadSpecNamed: 'spec_0010'.
	resolvedProject := loadSpec resolve.

"load project soec_0010"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'issue 495' }
RwRowanSample9Test >> testIssue_495_1 [
	"https://github.com/dalehenrich/Rowan/issues/495"

	| loadSpec projectName resolvedProject loadedProjects errorHit |

	loadSpec := self _loadSpecNamed: 'spec_0011'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project spec_0011"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

"spec_0012"
	loadSpec := self _loadSpecNamed: 'spec_0012'.
	resolvedProject := loadSpec resolveStrict.

"load project spec_0012"
	errorHit := false.

	[ loadedProjects := resolvedProject load ]
		on: Error
		do: [:ex |
			| x |
			self assert: (x := ex description) = 'UserDefinedError: The extension class named ''RowanSample9Class1'' cannot be found in this symbol dictionary (#''RowanSample9_3''). Packages that extend a class must be loaded into the symbol dictionary in which the class is installed.'.
			errorHit := true ].

"validate"
	self assert: errorHit.
]

{ #category : 'issue 495' }
RwRowanSample9Test >> testIssue_495_2 [
	"https://github.com/dalehenrich/Rowan/issues/495"

	"in this case the class package and extension package are swapping symbol dictionaries"

	| loadSpec projectName resolvedProject loadedProjects errorHit |

	loadSpec := self _loadSpecNamed: 'spec_0011'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project spec_0011"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

"spec_0013"
	loadSpec := self _loadSpecNamed: 'spec_0013'.
	resolvedProject := loadSpec resolveStrict.

"load project spec_0013"
	errorHit := false.
	[ loadedProjects := resolvedProject load ]
		on: Error
		do: [:ex |
			| x |
			self assert: (x := ex description) = 'UserDefinedError: The extension class named ''RowanSample9Class1'' cannot be found in this symbol dictionary (#''RowanSample9_1''). Packages that extend a class must be loaded into the symbol dictionary in which the class is installed.'.
			errorHit := true ].

"validate"
	self assert: errorHit.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_504_1 [
	"https://github.com/dalehenrich/Rowan/issues/504"

	"V2.0 replacement for RowanSample4Test>>testIssue230 ... explicit test for issue #504"

	"use spec_0002 and modify resolved project to use correct symbol dictionary mappings"

	| loadSpec projectName resolvedProject loadedProjects |

	loadSpec := self _loadSpecNamed: 'spec_0005'.	"primer -- spec_0001 with instancesInvariant"

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed:projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve primer project"
	resolvedProject := loadSpec resolve.

"load primer project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

	loadSpec := self _loadSpecNamed: 'spec_0002'.

"resolve"
	resolvedProject := loadSpec resolve.

	resolvedProject
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName2 
			forPackageNamed: projectName , '-' , 'Core';
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName2 
			forPackageNamed: projectName , '-' , 'Extensions';
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName3 
			forPackageNamed: projectName , '-' , 'GemStone';
		yourself.

"load"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_504_2 [
	"https://github.com/dalehenrich/Rowan/issues/504"

	"V2.0 replacement for RowanSample4Test>>testIssue230 ... explicit test for issue #504"

	"use spec_0006 ... use component symbol dictionary mappings"

	| loadSpec projectName resolvedProject loadedProjects |

	loadSpec := self _loadSpecNamed: 'spec_0005'.	"primer -- spec_0001 with instancesInvariant"

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed:projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve primer project"
	resolvedProject := loadSpec resolve.

"load primer project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

	loadSpec := self _loadSpecNamed: 'spec_0006'.

"resolve"
	resolvedProject := loadSpec resolve.

"load"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_504_3 [
	"https://github.com/dalehenrich/Rowan/issues/504"

	"V2.0 replacement for RowanSample4Test>>testIssue230 ... explicit test for issue #504"

	"use spec_0007 ... with instance variables to force the new class version"

	| loadSpec projectName resolvedProject loadedProjects |

	loadSpec := self _loadSpecNamed: 'spec_0007'.	"primer -- spec_0005 with instance variables"

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed:projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve primer project"
	resolvedProject := loadSpec resolve.

"load primer project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

	loadSpec := self _loadSpecNamed: 'spec_0006'.

"resolve"
	resolvedProject := loadSpec resolve.

"load"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_504_4 [
	"https://github.com/dalehenrich/Rowan/issues/504"

	"V2.0 replacement for RowanSample4Test>>testIssue230 ... explicit test for issue #504"

	"reverse the use of spec_0007 and spec_0006"

	| loadSpec projectName resolvedProject loadedProjects |

	loadSpec := self _loadSpecNamed: 'spec_0006'.	

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed:projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve primer project"
	resolvedProject := loadSpec resolve.

"load primer project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

	loadSpec := self _loadSpecNamed: 'spec_0007'.	"primer -- spec_0005 with instance variables"

"resolve"
	resolvedProject := loadSpec resolve.

"load"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_527 [
	"
		self _clearLoadSpecSessionCache
	"

	| loadSpec symDictName className projectName |
	loadSpec := self _loadSpecNamed: 'spec_0001'.

	(Rowan image loadedProjectNamed: loadSpec projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

	projectName := loadSpec projectName.
	className := projectName , 'Class1'.

	self
		_issue_527_resolve_load_validate: loadSpec
		className: className
		expectedSymDictName: self _sampleSymbolDictionaryName1.

	symDictName := self _sampleSymbolDictionaryName2.
	loadSpec
		gemstoneSetDefaultSymbolDictNameForUser: Rowan image currentUserId
		to: symDictName.

	self
		_issue_527_resolve_load_validate: loadSpec
		className: className
		expectedSymDictName: self _sampleSymbolDictionaryName2
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_531_baseline [
	"load spec_0004 and validate that symbol dictionary specs are followed"

	"https://github.com/GemTalk/Rowan/issues/531"

	| loadSpec projectName classesToSymDictMap |
	loadSpec := self _loadSpecNamed: 'spec_0004'.

	projectName := loadSpec projectName.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve/load/validate"
	self _standard_resolve_load_validate: loadSpec.

"validate that classes are in the correct symbolDictionary"
	classesToSymDictMap := Dictionary new
		at: {projectName , 'Class1'} put: self _sampleSymbolDictionaryName2;
		at: {projectName , 'Class2'} put: self _sampleSymbolDictionaryName3;
		at: {projectName , 'TestCase'} put: self _sampleSymbolDictionaryName1;
		yourself.
	self _validateSymbolDictMapping: classesToSymDictMap.

"unload project"
	(Rowan projectNamed: projectName) unload.

"validate"
	classesToSymDictMap keysDo: [:classNames |
		classNames do: [:className |
			self assert: (Rowan globalNamed: className) isNil ] ].
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_532 [
	"
		self _clearLoadSpecSessionCache
	"

	"https://github.com/GemTalk/Rowan/issues/532"

	| loadSpec hitError |
	hitError := false.
	[ loadSpec := self _loadSpecNamed: 'spec_0004_b' ]
		on: Error
		do: [ :ex | 
			| x |
			self assert: (x := ex description) = 'UserDefinedError: Duplicate package'.
			hitError := true ].
	self assert: hitError description: 'Expected an error'
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_549 [
	"RwProjectDefinitionV2>>removePackageNamed: needs to clean up the component references to the removed package name"

	"https://github.com/dalehenrich/Rowan/issues/549"

	| loadSpec projectName  resolvedProject1 resolvedProject2 loadedProjects project |
	loadSpec := self _loadSpecNamed: 'spec_0014'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject1 := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject1 load.

"validate"
	self _standard_validate: resolvedProject1 loadedProjects: loadedProjects.

"remove package RowanSample9-Extensions"
	project := loadedProjects at: 1.
	resolvedProject2 := project asDefinition.
	resolvedProject2 removePackageNamed: 'RowanSample9-Extensions'.

"validation should pass"
	self assert: resolvedProject2 _validate
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_549_errorCondition [
	"Verify that when component packages and package definitions are inconsistent we generate an error"

	"https://github.com/dalehenrich/Rowan/issues/549"

	| loadSpec projectName  resolvedProject1 errorHit |
	loadSpec := self _loadSpecNamed: 'spec_0015'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject1 := loadSpec resolve.
	resolvedProject1 repository checkout: 'rowan_issue_549'.	"tag pointing to inconsistent checkout"
	resolvedProject1 read.

"load project"
	errorHit := false.
	[resolvedProject1 _validate ] 
		on: Error
		do: [:ex | 
			| x |
			self assert: (x := ex description) = 'UserDefinedError: Component references package(s) that are not defined
	The following packages are defined, but not referenced in a component:
		RowanSample9-Extensions
'.
			errorHit := true].
	self assert: errorHit
]

{ #category : 'issue 557' }
RwRowanSample9Test >> testIssue_557 [
	"https://github.com/dalehenrich/Rowan/issues/557"

	| loadSpec projectName resolvedProject loadedProjects |

	loadSpec := self _loadSpecNamed: 'spec_0016'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project spec_0016"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

"spec_0017"
	loadSpec := self _loadSpecNamed: 'spec_0017'.
	resolvedProject := loadSpec resolveStrict.

"adjust project definition to force the class method #bar into extendedMethods"

"load modified project spec_0017"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_568 [
	"load spec_0021, then spec_0022 to validate that pre load doit was run"

	"https://github.com/dalehenrich/Rowan/issues/568"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0021'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
	self _validateSpec_0021_componentStructure: resolvedProject.

"load spec_0022"
	loadSpec := self _loadSpecNamed: 'spec_0022'.

"resolve project"
	resolvedProject := loadSpec resolve.

"load project should trigger preload doit"
	loadedProjects := resolvedProject load.

"validate test will validate that preload doit executed"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'issue 495' }
RwRowanSample9Test >> testIssue495_move_class_and_extension_method_to_new_symbol_dictV2_1 [
	"https://github.com/dalehenrich/Rowan/issues/495"

	"unsuccessful attempt to duplicate of RwRowanProjectIssuesTest debug: #testIssue495_move_class_and_extension_method_to_new_symbol_dict 
		(during audit: UndefinedObject does not understand  #'name') ... in this test we're loading from RowanSample9 repo and passing in  
		RwRowanProjectIssuesTestV2 debug: #testIssue215_move_class_and_extension_method_to_new_symbol_dict we're using original code which modifies 
		project definition in memory and it fails with the expected error"

	"in this case the class package and extension package are swapping symbol dictionaries"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0011'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project spec_0011"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

"spec_0014"
	loadSpec := self _loadSpecNamed: 'spec_0014'.
	resolvedProject := loadSpec resolveStrict.

"load project spec_0014"
	loadedProjects := resolvedProject load .

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'issue 495' }
RwRowanSample9Test >> testIssue495_move_class_and_extension_method_to_new_symbol_dictV2_2 [
	"https://github.com/dalehenrich/Rowan/issues/495"

	"this time the test results should matchRwRowanProjectIssuesTestV2 testIssue495_move_class_and_extension_method_to_new_symbol_dict"

	"should involve a movePackage and not a classProperty change as in testIssue495_move_class_and_extension_method_to_new_symbol_dictV2_1"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0011'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project spec_0011"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

"spec_0015"
	loadSpec := self _loadSpecNamed: 'spec_0015'.
	resolvedProject := loadSpec resolveStrict.

"load project spec_0014"
	loadedProjects := resolvedProject load .

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'issue 493' }
RwRowanSample9Test >> testMoveClassBetweenSymDicts_changeDefaulSymDict_2_493 [
	"https://github.com/dalehenrich/Rowan/issues/493"

	"duplicate of RwRowanIssue493Test debug: #testMoveClassBetweenSymDicts_changeDefaulSymDict_2
		(audit failure: Issue493Class>>Missing loaded method>>foo) 
		original test fails with (during audit: UndefinedObject does not understand  #'name')"

	"move a class with a method from one sym dict to another using defaultSymbolDictName"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0008'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project soec_008"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

"spec_009"
	loadSpec := self _loadSpecNamed: 'spec_0009'.
	resolvedProject := loadSpec resolve.

"load project soec_009"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'tests' }
RwRowanSample9Test >> testRowanSample4_basic_504 [

	| loadSpec projectName resolvedProject  loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0002'.

	projectName := loadSpec projectName.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve/load"
	resolvedProject := loadSpec resolve.
	loadedProjects := resolvedProject load.

	resolvedProject
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName2 
			forPackageNamed: projectName , '-' , 'Core';
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName2 
			forPackageNamed: projectName , '-' , 'Extensions';
		gemstoneSetSymbolDictName: RwRowanSample9Test _sampleSymbolDictionaryName3 
			forPackageNamed: projectName , '-' , 'GemStone';
		yourself.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

"unload project"
	(Rowan projectNamed: projectName) unload
]

{ #category : 'tests' }
RwRowanSample9Test >> testRowanSample4_primer_504 [

	| loadSpec projectName resolvedProject packageName1 packageName2 loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0005'.

	projectName := loadSpec projectName.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

	packageName1 := projectName , '-' , 'Core1'.
	packageName2 := projectName , '-' , 'Core2'.

"resolve/load"
	resolvedProject := loadSpec resolve.
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

"unload project"
	(Rowan projectNamed: projectName) unload
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0001_A [
	"a UserDefinedError occurred (error 2318), reason:halt, The component 'Core' is undefined"

	"functional variant of testSpec_0001_B"

	| rowanProject fileUrl loadSpecification resolvedProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	fileUrl := 'file:' , rowanProject repositoryRootPath
		, '/test/specs/RowanSample9_0000.ston'.
	loadSpecification := (RwSpecification fromUrl: fileUrl)
		projectsHome: self _testRowanProjectsSandbox;
		yourself.

	(self _testRowanProjectsSandbox / 'RowanSample9') ensureDeleteAll.

	resolvedProject := loadSpecification resolve. "project is not being loaded from disk on resolve"
	resolvedProject
		addPackagesNamed: {(loadSpecification projectName , '-Core')}
		toComponentNamed: 'Core'
		withConditions: {'common'}
		andGroupName: 'core'.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0001_B [
	"a UserDefinedError occurred (error 2318), reason:halt, The component 'Core' is undefined"

	"testSpec_0001_A behavies correctly"

	| rowanProject fileUrl loadSpecification resolvedProject resolvedRepository oldPolicy |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	fileUrl := 'file:' , rowanProject repositoryRootPath
		, '/test/specs/RowanSample9_0000.ston'.
	loadSpecification := (RwSpecification fromUrl: fileUrl)
		projectsHome: self _testRowanProjectsSandbox;
		repositoryResolutionPolicy: #'strict';
		yourself.	"force #strict policy to ensure that the revision is checkout out in the repository"

	(self _testRowanProjectsSandbox / 'RowanSample9') ensureDeleteAll.


	resolvedProject := loadSpecification resolve.

	resolvedRepository := resolvedProject repository.
	resolvedRepository checkout: 'master'.

	oldPolicy := loadSpecification repositoryResolutionPolicy.
	[ 
	"force #strict policy to ensure that the revision is checkout out in the repolicy"
	loadSpecification repositoryResolutionPolicy: #'strict'.
	resolvedProject := loadSpecification resolve ]
		ensure: [ loadSpecification repositoryResolutionPolicy: oldPolicy ].

	resolvedProject
		addPackagesNamed: {(loadSpecification projectName , '-Core')}
		toComponentNamed: 'Core'
		withConditions: {'common'}
		andGroupName: 'core'
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0002_to_0003 [
	"https://github.com/dalehenrich/Rowan/issues/230"

	"Passing variation"

	| loadSpec projectName resolvedProject loadedProjects project testResult 
		packageName1 packageName2 x |
	loadSpec := self _loadSpecNamed: 'spec_0002'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed:projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"set up for issue #230 ... RowanSample9Class1 in one symbol dictionary (instancesInvariant);
		RowanSample4IdentityKeyValueDictionary in another symbol dictionary"
	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.

"resolve primer project"
	resolvedProject := loadSpec resolve.

"load primer project"
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: project audit isEmpty.
	testResult := project testSuite run.
	self deny: testResult hasErrors.
	self deny: testResult hasFailures.

	loadSpec := self _loadSpecNamed: 'spec_0003'.	"primer -- spec_0001 with instancesInvariant"

"resolve"
	resolvedProject := loadSpec resolve.
	resolvedProject
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 
			forPackageNamed: packageName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 
			forPackageNamed: packageName2;
		yourself.

"load"
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: (x := project audit) isEmpty.
	testResult := project testSuite run.
	self deny: testResult hasErrors.
	self deny: testResult hasFailures.
]

{ #category : 'issue 493' }
RwRowanSample9Test >> testSpec_0008 [
	"https://github.com/dalehenrich/Rowan/issues/493"

	| loadSpec projectName resolvedProject loadedProjects |
UserGlobals at: #ConditionalHalt put: false.
	loadSpec := self _loadSpecNamed: 'spec_0008'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
UserGlobals at: #ConditionalHalt put: false.
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'issue 493' }
RwRowanSample9Test >> testSpec_0009 [
	"https://github.com/dalehenrich/Rowan/issues/493"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0009'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'issue 493' }
RwRowanSample9Test >> testSpec_0010 [
	"https://github.com/dalehenrich/Rowan/issues/493"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0010'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'issue 495' }
RwRowanSample9Test >> testSpec_0011 [
	"https://github.com/dalehenrich/Rowan/issues/495"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0011'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'issue 495' }
RwRowanSample9Test >> testSpec_0012 [
	"spec_0012 should result in an error when loaded"

	"https://github.com/dalehenrich/Rowan/issues/495"

	| loadSpec projectName resolvedProject loadedProjects errorHit |
	loadSpec := self _loadSpecNamed: 'spec_0012'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project -- expect error"
	errorHit := false.
	[ loadedProjects := resolvedProject load ] 
		on: Error 
		do: [:ex | 
			| x |
			self assert: ( x:=  ex description) = 'UserDefinedError: The extension class named ''RowanSample9Class1'' cannot be found in this symbol dictionary (#''RowanSample9_3''). Packages that extend a class must be loaded into the symbol dictionary in which the class is installed.'.
			errorHit := true ].
	self assert: errorHit.
]

{ #category : 'issue 495' }
RwRowanSample9Test >> testSpec_0013 [
	"spec_0013 should result in an error when loaded"

	"https://github.com/dalehenrich/Rowan/issues/495"

	| loadSpec projectName resolvedProject loadedProjects errorHit |
	loadSpec := self _loadSpecNamed: 'spec_0013'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project -- expect error"
	errorHit := false.
	[ loadedProjects := resolvedProject load ] 
		on: Error 
		do: [:ex | 
			| x |
			self assert: ( x:=  ex description) = 'UserDefinedError: The extension class named ''RowanSample9Class1'' cannot be found in this symbol dictionary (#''RowanSample9_1''). Packages that extend a class must be loaded into the symbol dictionary in which the class is installed.'.
			errorHit := true ].
	self assert: errorHit.
]

{ #category : 'issue 495' }
RwRowanSample9Test >> testSpec_0014 [
	"spec_0014 should load cleanly"

	"https://github.com/dalehenrich/Rowan/issues/495"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0014'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'issue 495' }
RwRowanSample9Test >> testSpec_0015 [
	"spec_0015 should load cleanly"

	"https://github.com/dalehenrich/Rowan/issues/495"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0015'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'issue 557' }
RwRowanSample9Test >> testSpec_0015_to_0016 [
	"https://github.com/dalehenrich/Rowan/issues/557"

	"passing test case ... symbol dictionary of packages involved don't match"

	| loadSpec projectName resolvedProject loadedProjects |

	loadSpec := self _loadSpecNamed: 'spec_0015'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project spec_0015"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

"spec_0016"
	loadSpec := self _loadSpecNamed: 'spec_0016'.
	resolvedProject := loadSpec resolveStrict.

"load project spec_0016"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects
]

{ #category : 'issue 557' }
RwRowanSample9Test >> testSpec_0016 [
	"spec_0016 should load cleanly"

	"https://github.com/dalehenrich/Rowan/issues/557"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0016'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'issue 557' }
RwRowanSample9Test >> testSpec_0016_to_0017 [
	"https://github.com/dalehenrich/Rowan/issues/557"

	"passing test case ... symbol dictionaries match, but no methods put into extendedMethods -- put into movedMethods instead"

	| loadSpec projectName resolvedProject loadedProjects |

	loadSpec := self _loadSpecNamed: 'spec_0016'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project spec_0016"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

"spec_0017"
	loadSpec := self _loadSpecNamed: 'spec_0017'.
	resolvedProject := loadSpec resolveStrict.

"load project spec_0017"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects
]

{ #category : 'issue 557' }
RwRowanSample9Test >> testSpec_0017 [
	"spec_0017 should load cleanly"

	"https://github.com/dalehenrich/Rowan/issues/557"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0017'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].	"resolve project"
	resolvedProject := loadSpec resolve.	"load project"
	loadedProjects := resolvedProject load.	"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
	self _validateSpec_0017_componentStructure: resolvedProject
]

{ #category : 'issue_554' }
RwRowanSample9Test >> testSpec_0019 [
	"spec_0019 should load cleanly"

	"https://github.com/dalehenrich/Rowan/issues/554"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0019'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].	"resolve project"
	resolvedProject := loadSpec resolve.	"load project"
	loadedProjects := resolvedProject load.	"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
	self
		_validateComponent: (resolvedProject componentNamed: 'Core')
			condition: {'common'}
			group: 'core'
			componentNames:
				{'subs/Core1'.
				'subs/Core2'}
			packageNames: {'RowanSample9-Core'}
			projectNames: {};
		_validateComponent: (resolvedProject componentNamed: 'Core')
			condition: {'common'}
			group: 'tests'
			componentNames: {}
			packageNames: {'RowanSample9-Tests'}
			projectNames: {};
		yourself
]

{ #category : 'issue_554' }
RwRowanSample9Test >> testSpec_0020 [
	"spec_0020 should load cleanly and match spec_0017"

	"https://github.com/dalehenrich/Rowan/issues/554"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0020'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].	"resolve project"
	resolvedProject := loadSpec resolve.	"load project"
	loadedProjects := resolvedProject load.	"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
	self _validateSpec_0017_componentStructure: resolvedProject
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0021 [
	"spec_0021 should load cleanly"

	"https://github.com/dalehenrich/Rowan/issues/568"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0021'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
	self _validateSpec_0021_componentStructure: resolvedProject
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0023_1 [
	"spec_0023 should fail with undefined symbol error while executing a preload doit, 
		since at the time that preloadDoits are executed, not all of the classes are defined.

		When RwAtomicProjectLoadComponentV2 is implemented and used, this test should pass. "

	"https://github.com/dalehenrich/Rowan/issues/568"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0023'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0023_2 [
	"spec_0023 should load cleanly since the components will be loaded in order and preload doits should succeed ... 
		tests for pre and post load doits run as part of #_standard_validate:loadedProjects:"

	"https://github.com/dalehenrich/Rowan/issues/568"

	| loadSpec projectName resolvedProject loadedProjects componentNames |
	loadSpec := self _loadSpecNamed: 'spec_0023'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

	componentNames := {}.
	loadSpec groupNames: {'core'}.
	{'Core1' . 'Core2' . 'Core3' . 'Core4' }
		do: [:componentName |
"load all of the components in proper sequence"
		componentNames add: componentName.
"resolve project"
		loadSpec componentNames: componentNames.
		resolvedProject := loadSpec resolve.

"load project"
		loadedProjects := resolvedProject load.

"validate"
		self _standard_validate: resolvedProject loadedProjects: loadedProjects ].
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0024 [
	"spec_0024 should load cleanly"

	"https://github.com/dalehenrich/Rowan/issues/553"

	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0024'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0025 [
	"spec_0025 should load cleanly"

	"https://github.com/dalehenrich/Rowan/issues/571"

	| loadSpec projectName resolvedProject loadedProjects projectNames |
	loadSpec := self _loadSpecNamed: 'spec_0025'.

	projectName := loadSpec projectName.
	projectNames := {projectName. projectName, '_1' . projectName, '_2' .  projectName, '_3' . }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
]
