Class {
	#name : 'RwProjectSpecificationV2Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testBasic [
	"excercise basic functionality"

	| projectName projectSpecification stonString specName |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.

	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultMethodEnvTo: 0;
		gemstoneSetDefaultMethodEnvForUser: 'PharoGs' to: 2;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultSymbolDictNameForUser: 'DataCurator'
			to: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: false;
		gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: 'DataCurator'
			to: true;
		yourself.

	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	self assert: projectSpecification  _validate
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testBasicResolve [
	"excercise basic functionality"

	| project projectName projectAlias stonString projectsHome expectedRepositoryRoot
		 resolvedProjectSpecification projectSpecification|
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	projectsHome := self _testRowanProjectsSandbox.
	expectedRepositoryRoot := projectsHome / projectAlias.

	self deny: expectedRepositoryRoot exists.

	project := RwResolvedProjectV2 new
		projectName: projectName;
		projectAlias: projectAlias;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		yourself.
projectSpecification := project _projectSpecification.
	self assert: project _validate.

	project resolve.

	self assert: expectedRepositoryRoot exists.
	stonString := STON toStringPretty: project.	"useful in case of error"

	resolvedProjectSpecification := project _projectSpecification.
	self assert: project packageNames isEmpty.
	self assert: resolvedProjectSpecification = projectSpecification.

]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testBasicResolve_git [
	"Create git repository on resolve"

	| loadSpec1 projectName projectsHome projectDefinition projectAlias expectedRepositoryRoot stonString |
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	projectsHome := self _testRowanProjectsSandbox.
	expectedRepositoryRoot := projectsHome / projectAlias.

	self deny: expectedRepositoryRoot exists.

	loadSpec1 := RwProjectSpecificationV2 new
		projectName: projectName;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		repoType: #'git';
		yourself.

	projectDefinition := loadSpec1 resolve.
	stonString := STON toStringPretty: projectDefinition.

	self assert: expectedRepositoryRoot exists.
	self assert: (expectedRepositoryRoot / '.git') exists
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testComparison_1 [
	| projectName projectSpecification specName projectSpecCopy stonString stonStringCopy |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.
	stonString := STON toStringPretty: projectSpecification.

	projectSpecCopy := projectSpecification copy.

	stonStringCopy := STON toStringPretty: projectSpecCopy.

	self assert: stonString = stonStringCopy.
	self assert: projectSpecCopy = projectSpecification
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testComparison_2 [
	"compare equal even if lazy initialization has taken place"

	| projectName projectSpecification specName projectSpecCopy stonString stonStringCopy stonStringLazy |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.
	stonString := STON toStringPretty: projectSpecification.

	projectSpecCopy := projectSpecification copy.

	stonStringCopy := STON toStringPretty: projectSpecCopy.

	projectSpecCopy
		repoType;
		platformProperties.	"trigger the selectors that cause lazy initialization"

	stonStringLazy := STON toStringPretty: projectSpecCopy.

	self assert: stonString = stonStringCopy.
	self assert: projectSpecCopy = projectSpecification.
	self deny: stonStringLazy = stonStringCopy
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testInvalidPropertyValue [
	"error coverage for invalid load specs"

	| projectName projectSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultMethodEnvTo: 'boom';
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: true;
		yourself.

	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	hitError := false.
	[ projectSpecification validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: Value of property (#''defaultMethodEnv''->''boom'') is expected to be class ''SmallInteger'' not class ''String'''.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testInvalidPropertyValueForUser [
	"error coverage for invalid load specs"

	| projectName projectSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultMethodEnvForUser: 'PharoGs' to: 'boom';
		gemstoneSetDefaultSymbolDictNameForUser: 'DataCurator'
			to: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: 'DataCurator'
			to: true;
		yourself.

	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	hitError := false.
	[ projectSpecification validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: Value of property (#''defaultMethodEnv''->''boom'') is expected to be class ''SmallInteger'' not class ''String'''.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testInvalidRevision [
	"error coverage for invalid load specs"

	| projectName projectSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		revision: 'boom';
		yourself.

	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	hitError := false.
	[ projectSpecification validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: Invalid revision ''boom''. Should be nil for disk-based repository'.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testIssue_530_1 [
	"https://github.com/GemTalk/Rowan/issues/530"

	"state changes to a copy of a loadSpec should not affect oriinal load spec"

	"original has no predefined platformSpec dictionary"

	| projectName projectSpecification stonStrings specName projectSpecCopy stonString stonStringCopy |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.
	stonString := STON toStringPretty: projectSpecification.

	projectSpecCopy := projectSpecification copy.
	stonStringCopy := STON toStringPretty: projectSpecCopy.

	self assert: stonString = stonStringCopy.
	self assert: projectSpecCopy = projectSpecification.

	projectSpecCopy
		gemstoneSetDefaultMethodEnvTo: 0;
		gemstoneSetDefaultMethodEnvForUser: 'PharoGs' to: 2;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultSymbolDictNameForUser: 'DataCurator'
			to: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: false;
		gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: 'DataCurator'
			to: true;
		yourself.

	self deny: projectSpecCopy = projectSpecification.

	stonStrings := {
	(STON toStringPretty: projectSpecification).
	(STON toStringPretty: projectSpecCopy)}	"useful in case of test failure"
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testIssue_530_2 [
	"https://github.com/GemTalk/Rowan/issues/530"

	"state changes to a copy of a loadSpec should not affect oriinal load spec"

	"predefine platformSpec dictionary in original"

	| projectName projectSpecification stonStrings specName projectSpecCopy stonString |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultSymbolDictNameForUser: 'Bozo'
			to: self _sampleSymbolDictionaryName2;
		yourself.
	stonString := STON toStringPretty: projectSpecification.

	projectSpecCopy := projectSpecification copy.

	self assert: projectSpecCopy = projectSpecification.

	projectSpecCopy
		gemstoneSetDefaultMethodEnvTo: 0;
		gemstoneSetDefaultMethodEnvForUser: 'PharoGs' to: 2;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultSymbolDictNameForUser: 'DataCurator'
			to: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: false;
		gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: 'DataCurator'
			to: true;
		yourself.

	self deny: projectSpecCopy = projectSpecification.

	stonStrings := {stonString.	"original loadSpec"
	(STON toStringPretty: projectSpecification).	"origiinal after copy modified"
	(STON toStringPretty: projectSpecCopy)	"copy"}.
	self assert: stonString = (stonStrings at: 1).	"duh"
	self assert: stonString = (stonStrings at: 2).	"point of test"
	self deny: stonString = (stonStrings at: 3)	"duh"
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testIssue_530_3 [
	"https://github.com/GemTalk/Rowan/issues/530"

	"state changes to a copy of a loadSpec should not affect oriinal load spec"

	"componentNames and groupNames need to be isolated"

	| projectName projectSpecification stonStrings specName projectSpecCopy stonString |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.
	stonString := STON toStringPretty: projectSpecification.

	projectSpecCopy := projectSpecification copy.

	self assert: projectSpecCopy = projectSpecification.

	projectSpecCopy componentNames add: 'Boom'.
	projectSpecCopy groupNames add: 'boom'.

	self deny: projectSpecCopy = projectSpecification.

	stonStrings := {stonString.	"original loadSpec"
	(STON toStringPretty: projectSpecification).	"origiinal after copy modified"
	(STON toStringPretty: projectSpecCopy)	"copy"}.
	self assert: stonString = (stonStrings at: 1).	"duh"
	self assert: stonString = (stonStrings at: 2).	"point of test"
	self deny: stonString = (stonStrings at: 3)	"duh"
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testMissingRevision [
	"error coverage for invalid load specs"

	| projectName projectSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		gitUrl: 'https://github.com/user/' , projectName;
		yourself.

	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	hitError := false.
	[ projectSpecification validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: The instance variable ''revision'' must be set for the''gitUrl'''.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testNilInstanceVariable [
	"error coverage for invalid load specs"

	| projectName projectSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		projectsPath: nil;
		yourself.

	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	hitError := false.
	[ projectSpecification validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: The instance variable ''projectsPath'' cannot be nil'.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testOnlyOneRepositoryUrl [
	"error coverage for invalid load specs"

	| projectName projectSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		gitUrl: 'https://github.com/user/' , projectName;
		diskUrl: 'ftp://$ROWAN_PROJECTS_HOME/' , projectName;
		yourself.

	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	hitError := false.
	[ projectSpecification validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: Only one of (gitUrl diskUrl mercurialUrl svnUrl) must be be set'.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testSpec_0001_A [
	"a UserDefinedError occurred (error 2318), reason:halt, The component 'Core' is undefined"

	"functional variant of testSpec_0001_B"

	| rowanProject fileUrl projectSpecification resolvedProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	fileUrl := 'file:' , rowanProject repositoryRootPath
		, '/test/specs/RowanSample9_0000.ston'.
	projectSpecification := (RwSpecification fromUrl: fileUrl)
		projectsHome: self _testRowanProjectsSandbox;
		yourself.

	(self _testRowanProjectsSandbox / 'RowanSample9') ensureDeleteAll.

	resolvedProject := projectSpecification resolve. "project is not being loaded from disk on resolve"
	resolvedProject
		addPackagesNamed: {(projectSpecification projectName , '-Core')}
		toComponentNamed: 'Core'
		withConditions: {'common'}
		andGroupName: 'core'.
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testSpec_0001_B [
	"a UserDefinedError occurred (error 2318), reason:halt, The component 'Core' is undefined"

	"testSpec_0001_A behavies correctly"

	| rowanProject fileUrl projectSpecification resolvedProject repositoryDefinition oldPolicy |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	fileUrl := 'file:' , rowanProject repositoryRootPath
		, '/test/specs/RowanSample9_0000.ston'.
	projectSpecification := (RwSpecification fromUrl: fileUrl)
		projectsHome: self _testRowanProjectsSandbox;
		repositoryResolutionPolicy: #'strict';
		yourself.	"force #strict policy to ensure that the revision is checkout out in the repository"

	(self _testRowanProjectsSandbox / 'RowanSample9') ensureDeleteAll.


	resolvedProject := projectSpecification resolve.

	repositoryDefinition := projectSpecification repositoryDefinition.
	repositoryDefinition checkout: 'master'.

	oldPolicy := projectSpecification repositoryResolutionPolicy.
	[ 
	"force #strict policy to ensure that the revision is checkout out in the repolicy"
	projectSpecification repositoryResolutionPolicy: #'strict'.
	resolvedProject := projectSpecification resolve ]
		ensure: [ projectSpecification repositoryResolutionPolicy: oldPolicy ].

	resolvedProject
		addPackagesNamed: {(projectSpecification projectName , '-Core')}
		toComponentNamed: 'Core'
		withConditions: {'common'}
		andGroupName: 'core'
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testUknownPlatform [
	"error coverage for invalid load specs"

	| projectName projectSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultMethodEnvTo: 0;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: true;
		yourself.

	hitError := false.
	projectSpecification platformProperties at: 'boom' put: Dictionary new.
	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	[ projectSpecification validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: Unknown platform name ''boom'' in platform properties'.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testUnknownPropertyKey [
	"error coverage for invalid load specs"

	| projectName projectSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultMethodEnvTo: 0;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: true;
		yourself.
	(projectSpecification platformProperties at: 'gemstone')
		at: 'BOOM'
		put:
			(Dictionary new
				at: #'boom' put: nil;
				yourself).

	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	hitError := false.
	[ projectSpecification validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description) = 'Error: Unknown platform property key #''boom'''.
			hitError := true ].
	self assert: hitError
]
