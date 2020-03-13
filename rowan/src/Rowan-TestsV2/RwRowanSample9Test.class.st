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
RwRowanSample9Test class >> _symbolDictionaryNames [
	^ super _symbolDictionaryNames , {'RowanSample9Class1'}
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
""
	^ self class _loadSpecNamed: specName
]

{ #category : 'private' }
RwRowanSample9Test >> _loadSpecSessionCacheKey [
	^ self class _loadSpecSessionCacheKey
]

{ #category : 'private' }
RwRowanSample9Test >> _migrationClassMap [

	^ {#(#'RowanSample9Class' #(#'ivar0')).
	#(#'RowanSample9ClassA' #(#'ivar0' #'a' #'ivar1')).
	#(#'RowanSample9ClassC' #(#'ivar0' #'a' #'ivar1' #'c' #'ivar2')).
	#(#'RowanSample9ClassD' #(#'ivar0' #'a' #'ivar1' #'d' #'ivar2')).
	#(#'RowanSample9ClassB' #(#'ivar0' #'b' #'ivar1')).
	#(#'RowanSample9ClassE' #(#'ivar0' #'b' #'ivar1' #'e' #'ivar2')).
	#(#'RowanSample9ClassF' #(#'ivar0' #'b' #'ivar1' #'f' #'ivar2'))}
]

{ #category : 'migration tests' }
RwRowanSample9Test >> _migrationLoadSpecNamed: specName projectName: projectName projectAlias: projectAlias gitUrl: gitUrl [
	"fabricate a load spec to be used for cloning a rowanSample9 project from a specific remote git repository"

	^ RwLoadSpecificationV2 new
		specName: specName;
		projectName: projectName;
		projectAlias: projectAlias;
		projectsHome: self _testRowanProjectsSandbox;
		gitUrl: gitUrl;
		revision: specName;
		projectSpecFile: 'rowan/project.ston';
		componentNames: {'Core'};
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		comment: 'Generated loadSpec for RowanSample9 migration tests';
		yourself
]

{ #category : 'private' }
RwRowanSample9Test >> _rowanSample1LoadSpecificationUrl_515 [
	"https://github.com/GemTalk/Rowan/issues/515"

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/samples/RowanSample1_v2.ston'
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
			| testSuite |
			self assert: (x := project audit) isEmpty.
			testSuite := project testSuite.
			self deny: testSuite tests isEmpty.
			testResult := testSuite run.
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

{ #category : 'migration tests' }
RwRowanSample9Test >> _testMigration: loadSpec instanceMigrator: instanceMigrator [

	"this test is used for testing non-github remote repositories; so the gitUrl will be used to control which remote the repository is cloned from.
	instanceMap is returned, so caller can properly validate the migration results"

	"load migration_1, set all of the instance variables (a-f, ivar0-ivar2), then load migration_2. with no migration all of the instance variables (a-f, ivar0-ivar2) should be niled out"

	| projectName resolvedProject loadedProjects project audit instanceMap |

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"we want to clone the repository using the specified gitUrl"
	loadSpec repositoryRoot ensureDeleteAll.

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project - migration_step_0"
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: (audit := project audit) isEmpty.

"checkout migration_step_1 branch; read and load"
	project checkout: 'spec_0035'.
	resolvedProject read; load.

"build instance map; for validation in next step"
	instanceMap := IdentityKeyValueDictionary new.
	self _migrationClassMap
		do: [ :ar | 
			| className ivs class instance |
			className := ar at: 1.
			class := Rowan globalNamed: className.
			instance := class new.
			ivs := ar at: 2.
			ivs do: [ :ivName | instance perform: ivName , ':' with: ivName ].
			instanceMap at: instance put: ar -> class ].

"checkout migration_step_2 branch; read and load"
	project checkout: 'spec_0036'.
	project asDefinition
		read;
		load: instanceMigrator.

	self assert: (audit := project audit) isEmpty.

	^ instanceMap
]

{ #category : 'private' }
RwRowanSample9Test >> _validate: resolvedProject loadedProjects: loadedProjects expectedTestErrors: expectedTestErrors [
	| testResult loadedProjectDefinition x |
	"validate"
	self assert: loadedProjects size = 1.
	loadedProjects
		do: [ :project | 
			| testSuite |
			self assert: (x := project audit) isEmpty.
			testSuite := project testSuite.
			self deny: testSuite tests isEmpty.
			testResult := testSuite run.
			self assert: testResult hasErrors.
			self deny: testResult hasFailures.
			self assert: testResult errors size = expectedTestErrors size.
			self assert: (testResult errors collect: [:case | case selector]) asArray sort = expectedTestErrors sort.
			loadedProjectDefinition := project asDefinition.
			self
				assert:
					loadedProjectDefinition class = resolvedProject projectDefinition class ]
]

{ #category : 'private' }
RwRowanSample9Test >> _validateSpec_0017_componentStructure: resolvedProject [
	self
		_validateComponent: (resolvedProject componentNamed: 'Core')
			componentNames: { 'tests/Tests'}
			packageNames:
				{'RowanSample9-Core'.
				'RowanSample9-Extensions'.
				'RowanSample9-Extensions1'}
			projectNames: {};
		_validateComponent: (resolvedProject componentNamed: 'tests/Tests')
			componentNames: {}
			packageNames: {'RowanSample9-Tests'}
			projectNames: {};
		yourself
]

{ #category : 'private' }
RwRowanSample9Test >> _validateSpec_0021_componentStructure: resolvedProject [
	self
		_validateComponent: (resolvedProject componentNamed: 'Core')
			componentNames: {'tests/Tests'}
			packageNames:
				{'RowanSample9-Core'.
				'RowanSample9-Extensions'}
			projectNames: {};
		_validateComponent: (resolvedProject componentNamed: 'tests/Tests')
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

{ #category : 'migration tests' }
RwRowanSample9Test >> testAutomaticMigration_gitolite [

	"load migration_1, set all of the instance variables (a-f, ivar0-ivar2), then load migration_2. after automtic migration the permanent instance variables (a-f) should retain the values, while the moved instance variables (ivar0-ivar2) should be niled out"

	| loadSpec instanceMap |
	loadSpec := self 
		_migrationLoadSpecNamed:  'spec_0034' 
		projectName: 'RowanSample9' 
		projectAlias: 'RowanSample9_Automatic_Migration' 
		gitUrl: 'git@git.gemtalksystems.com:RowanSample9.git'.

	instanceMap := self 
		_testMigration: loadSpec 
		instanceMigrator:RwGsInstanceMigrator aggressiveMigration.

"validate migration"
	instanceMap
		keysAndValuesDo: [ :instance :assoc | 
			| ivNames ar oldClass className class |
			ar := assoc key.
			oldClass := assoc value.
			className := ar at: 1.
			class := Rowan globalNamed: className.
			self assert: oldClass ~= class.
			self assert: instance class == class.
			ivNames := ar at: 2.
			1 to: ivNames size do: [ :index | 
				| ivName |
				ivName := ivNames at: index.
				self assert: (instance perform: ivName) == ivName ] ]
]

{ #category : 'tests' }
RwRowanSample9Test >> testCategoryComponents [
	"based on spec_0029"

	"a category component should be able to answer the list of packages and classes loaded in image"

	"https://github.com/dalehenrich/Rowan/issues/573"

	| loadSpec projectName projectNames resolvedProject loadedProjects project components categoryComponents |
	loadSpec := self _loadSpecNamed: 'spec_0029'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

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
		expectedProjectNames: projectNames.

"validate that list of packages and classes from the category components is correct"
	project := loadedProjects at: 1.

"using the category components, calculate the list of package names and class names in each component"
	components := project loadedComponents.
	categoryComponents := components categoryComponentsFor: project topLevelComponentNames.
"I think I want a component visitor to extract the list of packages from the loaded components"
	false ifTrue: [ self halt]
]

{ #category : 'migration tests' }
RwRowanSample9Test >> testDeferredMigration_gitlab [

	"load migration_1, set all of the instance variables (a-f, ivar0-ivar2), then load migration_2. after deferred migration ..."

	| loadSpec instanceMap instanceMigrator classesToMigrate expectedClassesToMigrate |
	loadSpec := self 
		_migrationLoadSpecNamed:  'spec_0034' 
		projectName: 'RowanSample9' 
		projectAlias: 'RowanSample9_Deferred_Migration' 
		gitUrl: 'git@gitlab.com:dalehenrichs/rowansample9.git'.

	instanceMigrator := RwGsDeferredInstanceMigrator noMigration.
	instanceMap := self 
		_testMigration: loadSpec 
		instanceMigrator: instanceMigrator.

"validate deferred migration"
	classesToMigrate := (instanceMigrator classesToMigrate collect: [:each | each name ]) sort.
	expectedClassesToMigrate := (self _migrationClassMap collect: [:each | each at: 1 ]) sort.
	self assert: classesToMigrate = expectedClassesToMigrate
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
	resolvedProject1 repository checkout: 'issue_549'.	"tag pointing to inconsistent checkout"
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

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_571_1 [
	"load project with a different projectName and projectAlias"

	"https://github.com/dalehenrich/Rowan/issues/571"

	| loadSpec projectName resolvedProject loadedProjects projectNames newProjectName projectAlias expectedProjectNames |
	loadSpec := self _loadSpecNamed: 'spec_0025'.

	projectName := loadSpec projectName.
	projectAlias := projectName, '_alias'.
	newProjectName := projectName, '_new'.
	projectNames := {newProjectName. projectName, '_1' . projectName, '_2' .  projectName, '_3' . }.
	expectedProjectNames := {newProjectName. projectName, '_1' . projectName, '_2' .  projectName, '_3' . }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	projectName := newProjectName.

	loadSpec
		projectName: projectName;
		projectAlias: projectAlias.

	(self _testRowanProjectsSandbox / projectAlias) ensureDeleteAll.
	self deny: (self _testRowanProjectsSandbox / projectAlias) exists.

"resolve project"
	resolvedProject := loadSpec resolve.

"validate"
	self assert: (self _testRowanProjectsSandbox / projectAlias) exists.
	self assert: resolvedProject repositoryRoot basename = projectAlias.

"load project"
	loadedProjects := resolvedProject loadProjectSet.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: expectedProjectNames
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue14 [
	"ported from RowanSample4>>testIssue14, start with issue_231_0 (spec_0037)"

	"https://github.com/GemTalk/Rowan/issues/14"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0037'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

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
		expectedProjectNames: projectNames.
]

{ #category : 'issue 185' }
RwRowanSample9Test >> testIssue185_move_class_to_symbolDict_1 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"move RowanSample9Class from symbol dictionary RowanSample9_1 to _sampleSymbolDictionaryName2"

	| loadSpec projectName projectNames resolvedProject loadedProjects project ar className class x |
	loadSpec := self _loadSpecNamed: 'spec_0002'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project using symbol dict  _sampleSymbolDictionaryName1"
	resolvedProject gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1.
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.
	className := 'RowanSample9Class1'.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class := Rowan globalNamed: className.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: (ar first at: 1) name = self _sampleSymbolDictionaryName1.


"load project using symbol dict  _sampleSymbolDictionaryName2"
	resolvedProject := project asDefinition.
	resolvedProject gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class := Rowan globalNamed: className.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName2.
]

{ #category : 'issue 185' }
RwRowanSample9Test >> testIssue185_move_class_to_symbolDict_2 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"move RowanSample9Class from symbol dictionary RowanSample9_1 to _sampleSymbolDictionaryName2"

	| loadSpec projectName projectNames resolvedProject loadedProjects project ar className class x packageName |
	loadSpec := self _loadSpecNamed: 'spec_0002'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	packageName := 'RowanSample9-Core'.

"resolve project"
	resolvedProject := loadSpec resolve.

"load project using symbol dict  _sampleSymbolDictionaryName1"
	resolvedProject gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName1 forPackageNamed: packageName.
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.
	className := 'RowanSample9Class1'.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class := Rowan globalNamed: className.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: (ar first at: 1) name = self _sampleSymbolDictionaryName1.


"load project using symbol dict  _sampleSymbolDictionaryName2"
	resolvedProject := project asDefinition.
	resolvedProject gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class := Rowan globalNamed: className.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName2.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue345 [

	"ported from RwRowanSample1Test>>testIssue345"

	"https://github.com/GemTalk/Rowan/issues/514"

	"This test (for 3.5.0) is actually about being able to change the component
		properties before loading ... post clone ... Right now I am not completely
		sure what sematics should be applied:
			- disallow changing component properteis - if you want different properties
				edit the component --- NOT COOL
			- somehow arrange for changes made to the ""loaded project"" to survive
				the read that ??must happend?? before being able to load the project 
				from disk
			- require that the project be read from disk before making changes and
				then somehow arranging to do a load of the project in memory and avoid
				re-reading from disk ... THIS PROBABLY THE RIGHT ANSWER ... need to 
				get second and third opinions on this one ... and expand test coverage,
				I assume that there are existing tests (the extension methods in this 
				package are good clues) that have hacked some sort of behavior in this
				area, but we need a REAL solution with some logic behind it more than
				""this is how it works:)"""

	"testIssue514, thoroughly tests that the change attributs are being preserved"

	| loadSpec projectName resolvedProject loadedProjects symDict registry |
	loadSpec := self _loadSpecNamed: 'spec_0011'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.
	resolvedProject 
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName; "different than originally specified"
		yourself.

	resolvedProject repository checkout: 'spec_0033'.

"explicitly read project, since the disk version has changed. then load project"
	loadedProjects := resolvedProject
		read;
		load.

"validate"
	self _validate: resolvedProject loadedProjects: loadedProjects expectedTestErrors: #(#'testSymbolDicts').

"issue 345 validation"
	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) notNil.

	symDict := Rowan globalNamed: self _symbolDictionaryName.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 3.
	self assert: registry classRegistry size = 2.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 6.

"unload (delete) project"
	(Rowan projectNamed: projectName) unload.

"issue 345 validation"
	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) isNil.

	symDict := Rowan globalNamed: self _symbolDictionaryName.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.
]

{ #category : 'issue 493' }
RwRowanSample9Test >> testIssue493_RowanSample2 [

	"https://github.com/dalehenrich/Rowan/issues/493"

	"duplicate of RwRowanSample2Test debug: #testIssue493 (no existing LoadedMethod found for the old compiledMethod.) ... "

	"https://github.com/GemTalk/Rowan/issues/254  (no existing LoadedMethod found for the old compiledMethod.) ... "

	"using the migration tests set of definitions"

	| loadSpec projectName resolvedProject loadedProjects project  audit |
	loadSpec := self 
		_migrationLoadSpecNamed:  'spec_0035' 
		projectName: 'RowanSample9' 
		projectAlias: 'RowanSample9_Issue_493' 
		gitUrl: 'git@github.com:dalehenrich/RowanSample9.git'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"we want to clone the repository using the specified gitUrl"
	loadSpec repositoryRoot ensureDeleteAll.

"Issue #493: use _symbolDictionaryName for initial load"
	loadSpec gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName.

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project - migration_step_1"
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: (audit := project audit) isEmpty.

"checkout and load - migration_step_2 -- Issue #493: switch symbol dicts and use _symbolDictionaryName_1 for second load"
	resolvedProject := project asDefinition.
	resolvedProject 
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		checkout: 'spec_0036';
		read;
		load.

	self assert: (audit := project audit) isEmpty.
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

{ #category : 'tests' }
RwRowanSample9Test >> testIssue514 [

	"ported from RwRowanSample1Test>>testIssue514"

	"https://github.com/GemTalk/Rowan/issues/514"

	"Explicitly confirm that the property change was made and propogated to loaded project ... 
		hmmm and loaded project needs to be dirty because it doesn't match disk"

	| loadSpec projectName resolvedProject loadedProjects project loadedProjectDefinition diskProjectSet
		imageProjectSet projectSetModification propertyModValidationBlock |
	loadSpec := self _loadSpecNamed: 'spec_0011'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.
	resolvedProject 
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName; "different than originally specified"
		yourself.

	resolvedProject repository checkout: 'spec_0033'.

"explicitly read project, since the disk version has changed. then load project"
	loadedProjects := resolvedProject
		read;
		load.

"validate"
	self _validate: resolvedProject loadedProjects: loadedProjects expectedTestErrors: #(#'testSymbolDicts').

"issue 514 validation"
	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) notNil.

	self assert: (Rowan globalNamed: self _symbolDictionaryName) notNil.

	project := Rowan projectNamed: projectName.
	self deny: project isDirty.	"see https://github.com/GemTalk/Rowan/issues/583 for more details on dirty state"

"compare"
	loadedProjectDefinition := project asDefinition.
	diskProjectSet :=  loadedProjectDefinition copy readProjectSet.
	imageProjectSet := RwProjectSetDefinition new
		addProject: loadedProjectDefinition;
		yourself.
	projectSetModification :=  imageProjectSet compareAgainstBase: diskProjectSet. "new vs. old"
	self deny: projectSetModification isEmpty.

"validate that property changes are correct"
	propertyModValidationBlock := [ :propertiesModification | 
		"property modification should have a new value for gs_SymbolDictionary equal to <self _symbolDictionaryName>"
		| propertyElementsModified propertyModification |
		self deny: propertiesModification isEmpty.
		propertyElementsModified := propertiesModification elementsModified.
		self assert: propertyElementsModified size = 1.
		propertyModification := propertyElementsModified at: 'gs_SymbolDictionary'.
		self assert: propertyModification oldValue isNil.
		self assert: propertyModification newValue = self _symbolDictionaryName asString ].
	(RwSample1TestValidationVisitor new)
		defaultNoopBlock: [ self assert: false description: 'unexpected modification' ];
		changedProjectBlock: [:projectModification | self assert: projectModification propertiesModification isEmpty ];
		changedPackageBlock: [:packageModification | 
			| packageName |
			packageName := packageModification key.
			(#('RowanSample1-Extensions' 'RowanSample1-Core' 'RowanSample1-Tests' ) includes: packageName)
				ifTrue: [ 
					packageName = 'RowanSample1-Extensions'
						ifTrue: [ self assert: packageModification classesModification isEmpty ]
						ifFalse: [ self deny: packageModification classesModification isEmpty ].
					self assert: packageModification classExtensionsModification isEmpty.
					propertyModValidationBlock value: packageModification propertiesModification ]
				ifFalse: [
					self assert: packageModification propertiesModification isEmpty ] ];
		changedClassBlock: [ :classModification :visitor |
			| packageName |
			packageName := visitor currentPackageDefinition key.
			self assert: (#('RowanSample9-Core' 'RowanSample9-Tests' ) includes: packageName).
			self assert: classModification instanceMethodsModification isEmpty.
			self assert: classModification classMethodsModification isEmpty.
			propertyModValidationBlock value: classModification propertiesModification ];
		visit: projectSetModification.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue515_RowanSample2 [

	"https://github.com/GemTalk/Rowan/issues/515"

	"duplicate of RwRowanSample2Test debug: #testIssue515"

	"using the migration tests set of definitions"

	| loadSpec projectName resolvedProject loadedProjects project  audit loadSpec2 resolvedProject2 |
	loadSpec := self 
		_migrationLoadSpecNamed:  'spec_0035' 
		projectName: 'RowanSample9' 
		projectAlias: 'RowanSample9'
		gitUrl: 'git@github.com:dalehenrich/RowanSample9.git'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"we want to clone the repository using the specified gitUrl"
	loadSpec repositoryRoot ensureDeleteAll.

"Issue #493: use _symbolDictionaryName for initial load"
	loadSpec gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName.

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project - migration_step_1"
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: (audit := project audit) isEmpty.

"checkout and load - migration_step_2"
	resolvedProject := project asDefinition.
	resolvedProject 
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		checkout: 'spec_0036';
		read;
		load.

	self assert: (audit := project audit) isEmpty.

"load spec_0011 using RowanSample1 project name and RowanSample9Class1 dictionary"
	loadSpec2 := loadSpec copy;
		projectName: 'RowanSample1' ;
		projectAlias: 'RowanSample9';
		gemstoneSetDefaultSymbolDictNameTo: 'RowanSample9Class1';
		revision: 'spec_0011';
		yourself.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

	resolvedProject2 := loadSpec2 resolveStrict.
	loadedProjects := resolvedProject2 load.
"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: (audit := project audit) isEmpty.

"watch out ... jadeite won't bhave well with class and symbol dictionary of same name ... but that's okay"
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

{ #category : 'migration tests' }
RwRowanSample9Test >> testNoMigration_bitbucket [

	"load migration_1, set all of the instance variables (a-f, ivar0-ivar2), then load migration_2. with no migration all of the instance variables (a-f, ivar0-ivar2) should be niled out"

	| loadSpec instanceMap |
	loadSpec := self 
		_migrationLoadSpecNamed:  'spec_0034' 
		projectName: 'RowanSample9' 
		projectAlias: 'RowanSample9_No_Migration' 
		gitUrl: 'git@bitbucket.org:dalehenrich/rowansample9.git'.

	instanceMap := self 
		_testMigration: loadSpec 
		instanceMigrator: RwGsInstanceMigrator noMigration.

"validate migration"
	instanceMap
		keysAndValuesDo: [ :instance :assoc | 
			| ivNames ar oldClass className class |
			ar := assoc key.
			oldClass := assoc value.
			className := ar at: 1.
			class := Rowan globalNamed: className.
			self assert: oldClass ~= class.
			self assert: instance class == oldClass.
			ivNames := ar at: 2.
			1 to: ivNames size do: [ :index | 
				| ivName |
				ivName := ivNames at: index.
				self assert: (instance perform: ivName) == ivName ] ]
]

{ #category : 'tests' }
RwRowanSample9Test >> testReferenceRepositoryDirectory [
	"Clone RowanSample9, then reference the directory as a plain disk repository, ignoring git"

	| loadSpec1 loadSpec2 projectNames resolvedProject |
	loadSpec1 := self _loadSpecNamed: 'spec_0002'.

	projectNames := {loadSpec1 projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project --- cloning it to disk"
	resolvedProject := loadSpec1 resolve.

"now set the diskUrl for loadeSpec2 to the repository root of loadSpec1"
	loadSpec2 := loadSpec1 copy.
	loadSpec2 
		gitUrl: nil;
		diskUrl: 'file://', resolvedProject repositoryRoot pathString.

	loadSpec2 resolve.
]

{ #category : 'tests' }
RwRowanSample9Test >> testRemoveSubclassOfClassWithNewVersion [
	"duplicate of RwRowanSample2Test >> testRemoveSubclassOfClassWithNewVersion"

	"load migration_step_1 (spec_0035), then load migration_step_0 (spec_0034) new version of 
		RowanSample9Class and no subclasses"

	"using the migration tests set of definitions"

	| loadSpec projectName resolvedProject loadedProjects project audit |
	loadSpec := self 
		_migrationLoadSpecNamed:  'spec_0035' 
		projectName: 'RowanSample9' 
		projectAlias: 'RowanSample9'
		gitUrl: 'git@github.com:dalehenrich/RowanSample9.git'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"we want to clone the repository using the specified gitUrl"
	loadSpec repositoryRoot ensureDeleteAll.

"Issue #493: use _symbolDictionaryName for initial load"
	loadSpec gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName.

"resolve project and ensure desired branch is checked out"
	resolvedProject := loadSpec resolveStrict.

"load project - migration_step_1"
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: (audit := project audit) isEmpty.

"checkout and load - migration_step_0"
	resolvedProject := project asDefinition.
	resolvedProject 
		checkout: 'spec_0034';
		read;
		load.

"validate"
	self assert: (audit := project audit) isEmpty.
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
		toComponentNamed: 'Core'.
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
			componentNames:
				{'subs/Core1'.
				'subs/Core2'.
				'tests/Tests'}
			packageNames: {'RowanSample9-Core'}
			projectNames: {};
		_validateComponent: (resolvedProject componentNamed: 'tests/Tests')
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
	loadSpec customConditionalAttributes: {}.
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
	loadedProjects := resolvedProject loadProjectSet.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0026 [
	"spec_0026 should load cleanly"

	"start with a single component, single package project."

	"https://github.com/dalehenrich/Rowan/issues/573"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0026'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

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

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0027 [
	"spec_0027 should load cleanly"

	"split pacakges into core and tests and use move class api to move class definitions from package to package"

	"https://github.com/dalehenrich/Rowan/issues/573"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0027'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

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

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0028 [
	"spec_0028 should load cleanly"

	"add conditional Tests component"

	"https://github.com/dalehenrich/Rowan/issues/573"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0028'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

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
		expectedProjectNames: projectNames.

"remove tests from the conditional attributes and resolve -- RowanSample9TestCase1 should not be loaded"
	loadSpec customConditionalAttributes: #().
	resolvedProject := loadSpec resolve.

"load project"
	self assert: (Rowan globalNamed: 'RowanSample9TestCase1') notNil.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self assert: (Rowan globalNamed: 'RowanSample9TestCase1') isNil.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0029 [
	"spec_0029 should load cleanly"

	"add 2 more classes, tests and packages (one class/package) and create category components: Core, Definitions, Specs, Tests"

	"https://github.com/dalehenrich/Rowan/issues/573"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0029'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

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
		expectedProjectNames: projectNames.

"remove tests from the conditional attributes and resolve -- RowanSample9TestCase1 should not be loaded"
	loadSpec customConditionalAttributes: #().
	resolvedProject := loadSpec resolve.

"load project"
	#('RowanSample9TestCase1'  'RowanSample9TestCase2'  'RowanSample9TestCase3') do: [:className |
		self assert: (Rowan globalNamed: className) notNil ].

	loadedProjects := resolvedProject load.

	#('RowanSample9TestCase1'  'RowanSample9TestCase2'  'RowanSample9TestCase3') do: [:className |
		self assert: (Rowan globalNamed: className) isNil ].
"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0030 [
	"spec_0030 should load cleanly"

	"add platform conditional extension methods and classes plus tests - gemstone pharo gs3.5-"

	"https://github.com/dalehenrich/Rowan/issues/573"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0030'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

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
		expectedProjectNames: projectNames.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0031 [
	"spec_0031 should load cleanly"

	"Add platform conditional extension methods and classes plus tests - pharo, gemstone and gs3.[5-]. Use platform components with alias."

	"https://github.com/dalehenrich/Rowan/issues/573"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0031'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

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
		expectedProjectNames: projectNames.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0032 [
	"spec_0032 should load cleanly"

	"Explore the use of shared directory for sharing code between two conditions, like v1 and v2 ... first create v1 and v2 packages (that can be independently loaded or loaded together in GemStone and use the class in shared component for code that is common to both -- not controlled by attributes"

	"https://github.com/dalehenrich/Rowan/issues/573"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0032'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	#('RowanSample9SpecsSharedClass4' 'RowanSample9Specsv1Class5' 'RowanSample9Specsv2Class5') do: [:className |
		self assert: (Rowan globalNamed: className) notNil ].

	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"remove v1 from the conditional attributes and resolve -- RowanSample9Specsv1Class5 should not be loaded"
	loadSpec customConditionalAttributes remove: 'v1'.
	resolvedProject := loadSpec resolve.
"load project"
	loadedProjects := resolvedProject load.
"validate"
	#('RowanSample9Specsv1Class5' ) do: [:className |
		self assert: (Rowan globalNamed: className) isNil ].
	#('RowanSample9SpecsSharedClass4' 'RowanSample9Specsv2Class5') do: [:className |
		self assert: (Rowan globalNamed: className) notNil ].
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"remove v2 from the conditional attributes, add v1 back  and resolve -- RowanSample9Specsv1Class5 should not be loaded"
	loadSpec customConditionalAttributes remove: 'v2'.
	loadSpec customConditionalAttributes add: 'v1'.
	resolvedProject := loadSpec resolve.
"load project"
	loadedProjects := resolvedProject load.
"validate"
	#('RowanSample9Specsv2Class5' ) do: [:className |
		self assert: (Rowan globalNamed: className) isNil ].
	#('RowanSample9SpecsSharedClass4' 'RowanSample9Specsv1Class5') do: [:className |
		self assert: (Rowan globalNamed: className) notNil ].
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0033 [
	"spec_0033 should load cleanly"

	"https://github.com/dalehenrich/Rowan/issues/345; https://github.com/dalehenrich/Rowan/issues/514"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0033'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

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
		expectedProjectNames: projectNames.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0034 [
	"spec_0034 should load cleanly"

	"migration step 0"

	| loadSpec projectName projectNames resolvedProject loadedProjects project audit |
	loadSpec := self _loadSpecNamed: 'spec_0034'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: (audit := project audit) isEmpty.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0035 [
	"spec_0035 should load cleanly"

	"migration step 1"

	| loadSpec projectName projectNames resolvedProject loadedProjects project audit |
	loadSpec := self _loadSpecNamed: 'spec_0035'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: (audit := project audit) isEmpty.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0036 [
	"spec_0036 should load cleanly"

	"migration step 2"

	| loadSpec projectName projectNames resolvedProject loadedProjects project audit |
	loadSpec := self _loadSpecNamed: 'spec_0036'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: (audit := project audit) isEmpty.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSwitchRepository [
	"Clone RowanSample9 in two different directory locations, each checked out on a different branch. 
		Confirm that projectsHome and repositoryRoot stay in sync as we excercise the various APIs for 
		manipulating projectsHome and repositoryRoot."

	| loadSpec1 loadSpec2 projectNames resolvedProject1 resolvedProject2 project1 loadedProjects |
	loadSpec1 := (self _loadSpecNamed: 'spec_0002')
		projectAlias: 'RowanSample9_0002';
		yourself.
	loadSpec2 := (self _loadSpecNamed: 'spec_0003')
		projectAlias: 'RowanSample9_0003';
		yourself.

	projectNames := {loadSpec1 projectName. loadSpec2 projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve projects"
	resolvedProject1 := loadSpec1 resolve.
	resolvedProject2 := loadSpec2 resolve.

"validate"
	self deny: resolvedProject2 repositoryRoot = resolvedProject1 repositoryRoot.

"load project1"
	loadedProjects := resolvedProject1 load.
	project1 := Rowan projectNamed: loadSpec1 projectName.

"change repository root of the loaded project"
	project1 repositoryRoot: resolvedProject2 repositoryRoot.

"validate"
	self _standard_validate: resolvedProject1 loadedProjects: loadedProjects.
	self assert: project1 asDefinition repositoryRoot = project1 repositoryRoot.
	self assert: resolvedProject2 repositoryRoot = project1 repositoryRoot.
	self deny: resolvedProject2 repositoryRoot = resolvedProject1 repositoryRoot.

"load project1 from spec_0002 checkout"
	loadedProjects := project1 load.

"Validate"
	self _standard_validate: resolvedProject1 loadedProjects: loadedProjects.
	self assert: (Rowan globalNamed: 'RowanSample9Class1') bar = 'bar'.	"bar method not present in spec_0001"
	self assert: resolvedProject2 repositoryRoot = project1 repositoryRoot.
]
