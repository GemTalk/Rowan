Extension { #name : 'RwRowanSample2Test' }

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwRowanSample2Test >> _migration_0_branchName [
	^   'masterV2.0'
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwRowanSample2Test >> _migration_1_branchName [
	^ 'migration_1_v2'
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwRowanSample2Test >> _migration_2_branchName [
	^ 'migration_2_v2'
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwRowanSample2Test >> _rowanSample1LoadSpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/samples/RowanSample1_v2.ston'
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwRowanSample2Test >> _rowanSample2_bitbucketSpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath
		, '/test/specs/RowanSample2_bitbucket_v2.ston'
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwRowanSample2Test >> _rowanSample2_gitlabSpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath
		, '/test/specs/RowanSample2_gitlab_v2.ston'
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwRowanSample2Test >> _rowanSample2_gitoliteSpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath
		, '/test/specs/RowanSample2_gitolite_v2.ston'
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwRowanSample2Test >> _rowanSample2SpecificationUrl [
	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath
		, '/test/specs/RowanSample2_masterV2.ston'
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwRowanSample2Test >> testIssue493 [

	"https://github.com/GemTalk/Rowan/issues/493"

	"started as Issue #515 test, but turned into issue #493 test, once #515 fixed"

	"load migration_1, set all of the instance variables (a-f, ivar0-ivar2), then load migration_2. after automtic migration the permanent instance variables (a-f) should retain the values, while the moved instance variables (ivar0-ivar2) should be niled out"

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName instanceMap projectDef |
	projectName := 'RowanSample2'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

  	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	projectDef := self 
		_cloneProjectFromSpecUrl: specUrlString 
		projectsHome: self _testRowanProjectsSandbox
		registerProject: false.

	projectDef defaultSymbolDictName: self _symbolDictionaryName.

	gitRootPath := projectDef repositoryRoot.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: self _migration_1_branchName.
	projectDef read; load.

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

	gitTool gitcheckoutIn: gitRootPath with: self _migration_2_branchName.

	projectDef := (Rowan projectNamed: projectName) asDefinition.
	projectDef
		defaultSymbolDictName: self _symbolDictionaryName1;
		read;
		load.
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwRowanSample2Test >> testIssue515 [

	"https://github.com/GemTalk/Rowan/issues/515"

	"load migration_1, set all of the instance variables (a-f, ivar0-ivar2), then load migration_2. after automtic migration the permanent instance variables (a-f) should retain the values, while the moved instance variables (ivar0-ivar2) should be niled out"

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName instanceMap projectDef |
	projectName := 'RowanSample2'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

  	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample2SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	projectDef := self 
		_cloneProjectFromSpecUrl: specUrlString 
		projectsHome: self _testRowanProjectsSandbox
		registerProject: false.

	projectDef defaultSymbolDictName: self _symbolDictionaryName.

	gitRootPath := projectDef repositoryRoot.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: self _migration_1_branchName.
	projectDef read; load.

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

	gitTool gitcheckoutIn: gitRootPath with: self _migration_2_branchName.

	projectDef := (Rowan projectNamed: projectName) asDefinition.
	projectDef
		defaultSymbolDictName: self _symbolDictionaryName;
		read;
		load.

"load RowanSample1 project into RowanSample2 dictionary"
	projectName := 'RowanSample1'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].
	specUrlString := self _rowanSample1LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	projectDef := self 
		_cloneProjectFromSpecUrl: specUrlString 
		projectsHome: gitRootPath
		registerProject: false.

	projectDef
		defaultSymbolDictName: 'RowanSample2';
		yourself.

"load project"
	projectDef load.
"watch out ... jadeite won't bhave well with class and symbol dictionary of same name ... but that's okay"
]
