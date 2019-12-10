Class {
	#name : 'RwRowanSample9Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwRowanSample9Test class >> _clearLoadSpecSessionCache [
	"self _clearLoadSpecSessionCache"

	SessionTemps current removeKey: self _loadSpecSessionCacheKey ifAbsent: [  ]
]

{ #category : 'private' }
RwRowanSample9Test class >> _loadSpecSessionCacheKey [
	^ #'RowanSample9LoadSpecsDict'
]

{ #category : 'private' }
RwRowanSample9Test >> _deleteProjectRepositoryDir [
	^ false
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
	"
		self _clearLoadSpecSessionCache
	"

	"The RowanSample9 repository is expected to be 'permanently' located on disk ... 
		we'll use `fetch` and `pull` to update the repo and branches as needed"

	"it is assumed that the load spec on the master branch and the load spec on the branch are the same"

	| loadSpecification resolvedProject resolvedRepository dict theSpec |
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
	resolvedProject := theSpec resolveStrict.	"When we reference a spec, we'll checkout the branch and refresh from remote, as a courtesy"
	resolvedRepository := resolvedProject repository.
	resolvedRepository
		fetch;
		pull: resolvedRepository remote branch: theSpec revision;
		yourself.
	^ theSpec copy
]

{ #category : 'private' }
RwRowanSample9Test >> _loadSpecSessionCacheKey [
	^ self class _loadSpecSessionCacheKey
]

{ #category : 'private' }
RwRowanSample9Test >> _projectSpecNamed: specName [
	"
		self _clearLoadSpecSessionCache
	"

	"The RowanSample9 repository is expected to be 'permanently' located on disk ... 
		we'll use `fetch` and `pull` to update the repo and branches as needed"

	"it is assumed that the load spec on the master branch and the load spec on the branch are the same"

	| projectSpecification repositoryDefinition dict theSpec |
	projectSpecification := self _rowanSample9_0000_load_spec.
	theSpec := (SessionTemps current
		at: self _loadSpecSessionCacheKey
		ifAbsent: [  ])
		ifNotNil: [ :specsDict | specsDict at: specName ]
		ifNil: [ 
			"The first time we access the repository in this session, we'll refresh the 
				master branch and cache the session specs"
			projectSpecification revision: 'master'.
			projectSpecification resolveStrict.

			repositoryDefinition := projectSpecification repositoryDefinition.
			repositoryDefinition
				fetch;
				pull: repositoryDefinition remote branch: 'master';
				yourself.

			dict := SessionTemps current
				at: self _loadSpecSessionCacheKey
				ifAbsentPut: [ Dictionary new ].
			(projectSpecification repositoryRoot / 'specs') files
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
	theSpec resolveStrict.	"When we reference a spec, we'll checkout the branch and refresh from remote, as a courtesy"
	repositoryDefinition := theSpec repositoryDefinition.
	repositoryDefinition
		fetch;
		pull: repositoryDefinition remote branch: theSpec revision;
		yourself.
	^ theSpec copy
]

{ #category : 'private' }
RwRowanSample9Test >> _rowanSample9_0000_load_spec [

	| rowanProject fileUrl |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	fileUrl := 'file:' , rowanProject repositoryRootPath
		, '/test/specs/RowanSample9_0000.ston'.
	^(RwSpecification fromUrl: fileUrl)
		projectsHome: self _testRowanProjectsSandbox;
		yourself.
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
	| resolvedProject loadedProjects project testResult loadedProjectDefinition |

"resolve/load"
	resolvedProject := projectSpec resolve.
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: project audit isEmpty.
	testResult := project testSuite run.
	self deny: testResult hasErrors.
	self deny: testResult hasFailures.

	loadedProjectDefinition := project asDefinition.
	self assert: loadedProjectDefinition class = resolvedProject projectDefinition class.
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
RwRowanSample9Test >> testIssue_230 [
	"https://github.com/dalehenrich/Rowan/issues/230"

	| loadSpec projectName resolvedProject loadedProjects project testResult 
		packageName1 packageName2 |

	loadSpec := self _loadSpecNamed: 'spec_0003'.	"primer -- spec_0001 with instancesInvariant"
	projectName := loadSpec projectAlias.

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
		componentNames: #('Core2');
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
		yourself.	"????"

	project2 := RwResolvedProjectV2 new
		componentNames:  #('Core2');
		yourself.	"????"

	stonString := STON toStringPretty: resolvedProject.
	false
		ifTrue: [ self halt ]
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_527 [
	"
		self _clearLoadSpecSessionCache
	"

	| loadSpec symDictName className projectName |
	loadSpec := self _loadSpecNamed: 'spec_0001'.

	(Rowan image loadedProjectNamed: loadSpec projectAlias ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

	projectName := loadSpec projectAlias.
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

	projectName := loadSpec projectAlias.
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
RwRowanSample9Test >> testRowanSample4_basic [

	| loadSpec projectName  |
	loadSpec := self _loadSpecNamed: 'spec_0002'.

	projectName := loadSpec projectAlias.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve/load/validate"
	self _standard_resolve_load_validate: loadSpec.

"unload project"
	(Rowan projectNamed: projectName) unload
]

{ #category : 'tests' }
RwRowanSample9Test >> testRowanSample4_primer [

	| loadSpec projectName |
	loadSpec := self _loadSpecNamed: 'spec_0003'.

	projectName := loadSpec projectAlias.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve/load/validate"
	self _standard_resolve_load_validate: loadSpec.

"unload project"
	(Rowan projectNamed: projectName) unload
]
