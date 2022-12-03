Class {
	#name : 'RwRowanSample9Test',
	#superclass : 'RwAbstractRowanSampleTest',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwRowanSample9Test class >> _gitPullSessionCacheKey [
	^ #'RowanSample9GitBranchDict'
]

{ #category : 'private' }
RwRowanSample9Test class >> _loadSpecSessionCacheKey [
	^ #'RowanSample9LoadSpecsDict'
]

{ #category : 'private' }
RwRowanSample9Test class >> _rowanSample_0000_load_spec [
	| rowanProject fileUrl |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	fileUrl := 'file:' , rowanProject repositoryRootPath
		, '/test/specs/RowanSample9_0000.ston'.
	^ (RwSpecification fromUrl: fileUrl)
		projectsHome: self _testRowanProjectsSandbox;
		yourself
]

{ #category : 'private' }
RwRowanSample9Test >> _adjustRequiredProjectsForIssue805: resolvedProject [
	"If the tests are run by someone other than myself, we have to arrange for anonymous access to the public repos using http://"

	"see https://github.com/GemTalk/Rowan/issues/805"

	(System gemEnvironmentVariable: 'USER') ~= 'dhenrich'
		ifTrue: [ 
			| projectLoadSpec projectsRoot |
			"need to modify the rquired project load spec on disk ..."
			projectsRoot := resolvedProject projectsRoot.
			projectLoadSpec := RwSpecification
				fromFile: projectsRoot / 'RowanSample10.ston'.
			projectLoadSpec gitUrl: 'https://git@github.com/dalehenrich/RowanSample10'.
			projectLoadSpec exportTo: projectsRoot ]
]

{ #category : 'audit tests' }
RwRowanSample9Test >> _auditRemoveAndRecompiledMethods_0081 [
	| className packageName1 packageName2 class |
"remove compiled methods"
	className := 'RowanSample9Class1'.
	packageName1 := 'RowanSample9-Core'.
	packageName2 := 'RowanSample9-Extensions'.
	class := Rowan globalNamed: className.
	class removeSelector: #foo4.
	class removeSelector: #foo5.
	class removeSelector: #foo6.
	class class  removeSelector: #bar.
	class class  removeSelector: #bar4.
	class class  removeSelector: #bar5.
"recompile without packaging"
	class
		compileMethod:  'foo4 ^4'
		dictionaries: GsCurrentSession currentSession symbolList
		category: ('*', packageName2 asLowercase) asSymbol
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.
	class
		compileMethod:  'foo5 ^5'
		dictionaries: GsCurrentSession currentSession symbolList
		category: ('*', packageName2 asLowercase) asSymbol
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.
	class
		compileMethod:  'foo6 ^6'
		dictionaries: GsCurrentSession currentSession symbolList
		category: ('*', packageName2 asLowercase) asSymbol
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.
	class class
		compileMethod:  'bar ^''bar'''
		dictionaries: GsCurrentSession currentSession symbolList
		category: ('*', packageName2 asLowercase) asSymbol
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.
	class class
		compileMethod:  'bar4 ^4'
		dictionaries: GsCurrentSession currentSession symbolList
		category: ('*', packageName2 asLowercase) asSymbol
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.
	class class
		compileMethod:  'bar5 ^5'
		dictionaries: GsCurrentSession currentSession symbolList
		category: ('*', packageName2 asLowercase) asSymbol
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.
]

{ #category : 'private' }
RwRowanSample9Test >> _expectedIssue766GsFileContents [
	^ '! Class Declarations
! Generated file, do not Edit

doit
(IdentityKeyValueDictionary
	subclass: ''RowanSample9IdentityKeyValueDictionary''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanSample9_1
	options: #( #logCreation )
)
		category: ''RowanSample9-GemStone'';
		comment: ''I am an example class'';
		immediateInvariant.
true.
%

doit
(Object
	subclass: ''RowanSample9Class1''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanSample9_1
	options: #( #logCreation )
)
		category: ''RowanSample9-Core'';
		comment: ''I am an example class'';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: ''RowanSample9GemStoneTestCase''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanSample9_1
	options: #( #logCreation )
)
		category: ''RowanSample9-GemStone-Tests'';
		comment: ''I test the GemStone class'';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: ''RowanSample9TestCase''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanSample9_1
	options: #( #logCreation )
)
		category: ''RowanSample9-Tests'';
		comment: ''I test the Core class'';
		immediateInvariant.
true.
%

! Class implementation for ''RowanSample9IdentityKeyValueDictionary''

!		Class methods for ''RowanSample9IdentityKeyValueDictionary''

category: ''accessing''
classmethod: RowanSample9IdentityKeyValueDictionary
bar ^''bar''
%

!		Instance methods for ''RowanSample9IdentityKeyValueDictionary''

category: ''accessing''
method: RowanSample9IdentityKeyValueDictionary
foo ^1
%

! Class implementation for ''RowanSample9Class1''

!		Instance methods for ''RowanSample9Class1''

category: ''accessing''
method: RowanSample9Class1
foo ^1
%

! Class implementation for ''RowanSample9GemStoneTestCase''

!		Instance methods for ''RowanSample9GemStoneTestCase''

category: ''tests''
method: RowanSample9GemStoneTestCase
test1  self assert: RowanSample9IdentityKeyValueDictionary new foo = 1
%

category: ''tests''
method: RowanSample9GemStoneTestCase
test2  self assert: RowanSample9IdentityKeyValueDictionary bar = ''bar''
%

! Class implementation for ''RowanSample9TestCase''

!		Instance methods for ''RowanSample9TestCase''

category: ''tests''
method: RowanSample9TestCase
test1  self assert: RowanSample9Class1 new foo = 1
%

category: ''tests''
method: RowanSample9TestCase
test2  self assert: RowanSample9Class1 bar = ''bar''
%

! Class extensions for ''RowanSample9Class1''

!		Class methods for ''RowanSample9Class1''

category: ''*rowansample9-extensions''
classmethod: RowanSample9Class1
bar ^''bar''
%

'
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

	self assert: symDictName = expectedSymDictName asString.

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
	^ self class _rowanSample_0000_load_spec
]

{ #category : 'private' }
RwRowanSample9Test >> _spec0057GsFilein [
	^ self _spec0057GsFilein: true
]

{ #category : 'private' }
RwRowanSample9Test >> _spec0057GsFilein: includeRemoveAll [
	| filein |
	filein := 'set compile_env: 0
! ------------------- Class definition for RowanSample9Class1
expectvalue /Class
doit
Object subclass: ''RowanSample9Class1''
  instVarNames: #( ivar1)
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: RowanSample9_1
  options: #()
%
expectvalue /Class
doit
RowanSample9Class1 comment: 
''I am an example class''
%
expectvalue /Class
doit
RowanSample9Class1 category: ''RowanSample9-Core''
%
'.
	includeRemoveAll
		ifTrue: [ 
			filein := filein
				,
					'
! ------------------- Remove existing behavior from RowanSample9Class1
removeAllMethods RowanSample9Class1
removeallclassmethods RowanSample9Class1
' ].
	filein := filein
		,
			'
set compile_env: 0
! ------------------- Class methods for RowanSample9Class1
category: ''accessing''
classmethod: RowanSample9Class1
bar ^''bar''
%
! ------------------- Instance methods for RowanSample9Class1
category: ''accessing''
method: RowanSample9Class1
foo ^1
%

set compile_env: 0
! ------------------- Class definition for RowanSample9TestCase
expectvalue /Class
doit
TestCase subclass: ''RowanSample9TestCase''
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: RowanSample9_1
  options: #()
%
expectvalue /Class
doit
RowanSample9TestCase comment: 
''I test the example class''
%
expectvalue /Class
doit
RowanSample9TestCase category: ''RowanSample9-Tests''
%
'.
	includeRemoveAll
		ifTrue: [ 
			filein := filein
				,
					'
! ------------------- Remove existing behavior from RowanSample9TestCase
removeAllMethods RowanSample9TestCase
removeAllClassMethods RowanSample9TestCase
' ].
	filein := filein
		,
			'
set compile_env: 0
! ------------------- Class methods for RowanSample9TestCase
! ------------------- Instance methods for RowanSample9TestCase
category: ''tests''
method: RowanSample9TestCase
test  self assert: RowanSample9Class1 new foo = 1
%
'.
	^ filein
]

{ #category : 'private' }
RwRowanSample9Test >> _specListProjectNames [
	^ super _specListProjectNames
		,
			{'PrimerSample9'.
			'RowanSample9_testbed'}
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

{ #category : 'tests' }
RwRowanSample9Test >> _testNefariousUnmanagedSpec_0057: includeRemoveAll  packageName1: packageName1 packageName2: packageName2 className1: className1 className2: className2 [
	"based on spec_0057 after loading, use nefarious methods to corrupt loaded methods"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0057'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
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

"with the fix for GemTalk/Rowan#830, have to use nefarious mans to explicitly corrupt packaging"
	Rowan image 
		loadedMethod: #foo 
		inClassNamed: 'RowanSample9Class1' 
		isMeta: false 
		ifFound: [:loadedMethod |
			"nefarious method"
			loadedMethod unpackageMethod ] 
		ifAbsent: [ self assert: false description: 'unexpected missing loaded method for RowanSample9Class1>>foo'].
	Rowan image 
		loadedMethod: #bar 
		inClassNamed: 'RowanSample9Class1' 
		isMeta: true 
		ifFound: [:loadedMethod |
			"nefarious method"
			loadedMethod unpackageMethod ] 
		ifAbsent: [ self assert: false description: 'unexpected missing loaded method for RowanSample9Class1 class>>bar'].
	Rowan image 
		loadedMethod: #test
		inClassNamed: 'RowanSample9TestCase' 
		isMeta: false 
		ifFound: [:loadedMethod |
			"nefarious method"
			loadedMethod unpackageMethod ] 
		ifAbsent: [ self assert: false description: 'unexpected missing loaded method for RowanSample9TestCase>>test'].
	^ loadedProjects
]

{ #category : 'tests' }
RwRowanSample9Test >> _testUnmanagedSpec_0057: includeRemoveAll  packageName1: packageName1 packageName2: packageName2 className1: className1 className2: className2 [
	"based on spec_0057 after loading, do a filein (with removeAllMethods) and run unmanaged report and an audit"

	| loadSpec projectName projectNames resolvedProject loadedProjects stream |
	loadSpec := self _loadSpecNamed: 'spec_0057'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
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

"do filein"
	stream := (self _spec0057GsFilein: true) readStreamPortable.
	((Rowan globalNamed: 'GsFileIn') _fromStream: stream)
		setEnableRemoveAll: includeRemoveAll;
		doFileIn.
	^ loadedProjects
]

{ #category : 'private' }
RwRowanSample9Test >> _validate_571: resolvedProject loadedProjects: loadedProjects expectedProjectNames: expectedProjectNames expectedPackageName: packageName [
	| project x y |
	project := loadedProjects at: 1.
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: expectedProjectNames.

	self
		assert:
			resolvedProject projectDefinitionPlatformConditionalAttributes
				= resolvedProject platformConditionalAttributes.
	self
		assert:
			(x := resolvedProject platformConditionalAttributes)
				= (y := project platformConditionalAttributes).
	self
		assert:
			(x := resolvedProject projectDefinitionPlatformConditionalAttributes)
				= (y := project projectDefinitionPlatformConditionalAttributes).
	self assert: (project packageNames includes: packageName)
]

{ #category : 'private' }
RwRowanSample9Test >> _validate_571: resolvedProject loadedProjects: loadedProjects expectedProjectNames: expectedProjectNames packageName: packageName present: isPresent [
	| project x y |
	project := loadedProjects at: 1.
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: expectedProjectNames.

	self
		assert:
			resolvedProject projectDefinitionPlatformConditionalAttributes
				= resolvedProject platformConditionalAttributes.
	self
		assert:
			(x := resolvedProject platformConditionalAttributes)
				= (y := project platformConditionalAttributes).
	self
		assert:
			(x := resolvedProject projectDefinitionPlatformConditionalAttributes)
				= (y := project projectDefinitionPlatformConditionalAttributes).

	isPresent
		ifTrue: [ self assert: (project packageNames includes: packageName) ]
		ifFalse: [ self deny: (project packageNames includes: packageName) ].

	^ project projectDefinitionPlatformConditionalAttributes
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

{ #category : 'running' }
RwRowanSample9Test >> tearDown [
	super tearDown.
	Rowan gemstoneTools topaz currentTopazPackageName: nil
]

{ #category : 'audit tests' }
RwRowanSample9Test >> testAuditMethodsNotIdentical [
	"RwAuditMethodDetail with #'methodsNotIdentical' reason for instance and class side methods"

	| loadSpec projectName projectNames resolvedProject loadedProjects project audit className packageName1 packageName2 reasons class |
	loadSpec := self _loadSpecNamed: 'spec_0081'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	audit := project audit.
	self assert: audit isEmpty.

"remove compiled methods and recompile without packaging"
	className := 'RowanSample9Class1'.
	packageName1 := 'RowanSample9-Core'.
	packageName2 := 'RowanSample9-Extensions'.
	class := Rowan globalNamed: className.
	self _auditRemoveAndRecompiledMethods_0081.

"audit"
	audit := project audit.

	self deny: audit isEmpty.
	reasons := (((audit at: packageName1) at: className) collect: [:auditDetail | auditDetail reason]) asArray.
	self assert: reasons size equals: 6.
	self
		assert: reasons asSet asArray sort
		equals: #(missingLoadedMethod) sort.
	reasons := (((audit at: packageName2) at: className) collect: [:auditDetail | auditDetail reason]) asArray.
	self assert: reasons size equals: 6.
	self
		assert: reasons asSet asArray sort
		equals: #(methodsNotIdentical) sort.
]

{ #category : 'audit tests' }
RwRowanSample9Test >> testAuditMismatchedMethodCategory [
	"RwAuditMethodDetail with #'differentMethodCategory' reason for instance and class side methods
		#'missingExtensionCategory' comes along for the ride"

	| loadSpec projectName projectNames resolvedProject loadedProjects project audit class className packageName1 packageName2 reasons |
	loadSpec := self _loadSpecNamed: 'spec_0081'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	audit := project audit.
	self assert: audit isEmpty.

"rename method categories"
	className := 'RowanSample9Class1'.
	packageName1 := 'RowanSample9-Core'.
	packageName2 := 'RowanSample9-Extensions'.
	class := Rowan globalNamed: className.

	class renameCategory: '*rowansample9-extensions' to: 'another category'.
	class class renameCategory: '*rowansample9-extensions' to: 'another category'.

"audit"
	audit := project audit.

"validate"
	self deny: audit isEmpty.
	reasons := (((audit at: packageName2) at: className) collect: [:auditDetail | auditDetail reason]) asArray.
	self assert: reasons size equals: 8.
	self
		assert: reasons asSet asArray sort
		equals: #(differentMethodCategory missingExtensionCategory) sort.
]

{ #category : 'audit tests' }
RwRowanSample9Test >> testAuditMismatchedMethodCategoryAndMethodsNotIdentical [
	"RwAuditMethodDetail with #'methodsNotIdentical' and #'differentMethodCategory' reason for instance and class side methods"

	"testAuditMismatchedMethodCategory passes and testAuditMethodsNotIdentical passes, but the OLD audit fails to detect differentMethodCategory ... new audit
		created to address this deficiency"

	| loadSpec projectName projectNames resolvedProject loadedProjects project audit class className packageName1 packageName2 reasons |
	loadSpec := self _loadSpecNamed: 'spec_0081'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	loadSpec customConditionalAttributes: {}.
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	audit := project audit.
	self assert: audit isEmpty.

"remove compiled methods and recompile without packaging"
	self _auditRemoveAndRecompiledMethods_0081.

"rename method categories"
	className := 'RowanSample9Class1'.
	packageName1 := 'RowanSample9-Core'.
	packageName2 := 'RowanSample9-Extensions'.
	class := Rowan globalNamed: className.
	class renameCategory: '*rowansample9-extensions' to: 'another category'.
	class class renameCategory: '*rowansample9-extensions' to: 'another category'.

"audit"
	audit := project audit.

"validate"
	self deny: audit isEmpty.
	reasons := (((audit at: packageName2) at: className) collect: [:auditDetail | auditDetail reason]) asArray.
	self assert: reasons size equals: 14.
	self
		assert: reasons asSet asArray sort
		equals: #(differentMethodCategory methodsNotIdentical missingExtensionCategory) sort.
]

{ #category : 'audit tests' }
RwRowanSample9Test >> testAuditMissingCompiledMethod [
	"RwAuditMethodDetail with #'missingCompiledMethod' reason for instance and class side methods"

	| loadSpec projectName projectNames resolvedProject loadedProjects project audit class className packageName1 packageName2 reasons |
	loadSpec := self _loadSpecNamed: 'spec_0081'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	audit := project audit.
	self assert: audit isEmpty.

"remove compiled methods"
	className := 'RowanSample9Class1'.
	packageName1 := 'RowanSample9-Core'.
	packageName2 := 'RowanSample9-Extensions'.
	class := Rowan globalNamed: className.
	class removeSelector: #foo.
	class class  removeSelector: #bar.

"audit"
	audit := project audit.

	self deny: audit isEmpty.
	reasons := (((audit at: packageName1) at: className) collect: [:auditDetail | auditDetail reason]) asArray.
	self assert: reasons size equals: 1.
	self
		assert: reasons asSet asArray sort
		equals: #(missingCompiledMethod) sort.
	reasons := (((audit at: packageName2) at: className) collect: [:auditDetail | auditDetail reason]) asArray.
	self assert: reasons size equals: 1.
	self
		assert: reasons asSet asArray sort
		equals: #(missingCompiledMethod) sort.
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
		gitUrl: 'https://git@gitlab.com/dalehenrichs/rowansample9.git'.

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
	self class _adjustSpecForIssue805: projectSpecification.
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
			projectSpec projectsHome: self _testRowanProjectsSandbox ]
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_254 [
	"https://github.com/dalehenrich/Rowan/issues/254"

	| loadSpec projectName resolvedProject loadedProjects project 
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
	self _runTestSuiteForProject: project.

	loadSpec := self _loadSpecNamed: 'spec_0002'.	"basic project (remove instancesInvariant option and move class to default symbol dictionary: self _sampleSymbolDictionaryName1)"

"resolve/load"
	resolvedProject := loadSpec resolve.
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: project audit isEmpty.
	self _runTestSuiteForProject: project.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_446 [
	"Not a real test ... used for V2.0 spec development"

	"https://github.com/GemTalk/Rowan/issues/446"

	"create a project that has 2 components, one component 
		requires the other component; and two load specs, each spec
		loads a different component ... needed to consolidate spec_004 
		creation"

	| project1 project2 projectName projectsHome resolvedProject |
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
		addNewComponentNamed: 'Core1'
		condition: 'common'
		comment: 'contains ' , projectName , '-Core1 package and Core2 component')
			addComponentNames: #('Core2');
			yourself.
	resolvedProject
		addNewComponentNamed: 'Core2'
		condition: 'common'
		comment: 'contains ' , projectName , '-Core2 package'.

	resolvedProject
		componentNames: #('Core1');
		yourself.

	project2 := RwResolvedProjectV2 new
		componentNames: #('Core2');
		yourself.

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
RwRowanSample9Test >> testIssue_504_5 [
	"reverse of testIssue_504_3"
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

	loadSpec := self _loadSpecNamed: 'spec_0005'.

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

{ #category : 'issue 493' }
RwRowanSample9Test >> testIssue_587 [
	"supplement to testIssue493_RowanSample2"

	"load spec_0035; fiddle symbol dictionary, load spec_0035"

	"https://github.com/dalehenrich/Rowan/issues/587"

	| loadSpec projectName resolvedProject loadedProjects project  audit |
	loadSpec := self 
		_migrationLoadSpecNamed:  'spec_0035' 
		projectName: 'RowanSample9' 
		projectAlias: 'RowanSample9_Issue_493' 
		gitUrl: 'https://git@github.com/dalehenrich/RowanSample9'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"we want to clone the repository using the specified gitUrl"
	loadSpec repositoryRoot ensureDeleteAll.

"Issue #587: use _symbolDictionaryName for initial load"
	loadSpec gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName.

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project - migration_step_1"
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: (audit := project audit) isEmpty.

"checkout and load - migration_step_2 -- Issue #487: switch symbol dicts and use _symbolDictionaryName_1 for second load"
	resolvedProject := project asDefinition.
	resolvedProject 
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		checkout: 'spec_0035';
		read;
		load.

	self assert: (audit := project audit) isEmpty.
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
RwRowanSample9Test >> testIssue185_move_class_to_symbolDict [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"
		issue_185_0 --> issue_185_1 --> issue_185_3	:: move NewRowanSample4 class to RowanSample4SymbolDict symbol dictionary (no package rename)
		spec_0002 --> spec_0038 	:: move NewRowanSample9 class to _sampleSymbolDictionaryName1 (no package rename)
	"

	"based on RwRowanSample4Test>>testIssue185_move_class_to_symbolDict"

	| loadSpec projectName projectNames resolvedProject loadedProjects project ar className1 class1 className2 class5 
		packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className3 className4 
		className5 x |
	loadSpec := self _loadSpecNamed: 'spec_0002'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.
	className1 := 'RowanSample9Class1'.

	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.

	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project using symbol dict  _sampleSymbolDictionaryName1"
	resolvedProject 
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		yourself.
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class1 := Rowan globalNamed: className1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (ar first at: 1) name = self _sampleSymbolDictionaryName1.
	self assert: (Rowan globalNamed: className5) isNil.

"load branch spec_0038 which adds new package and new class in _sampleSymbolDictionaryName2"
	resolvedProject checkout: 'spec_0038'.
	resolvedProject
		read;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName6;
		yourself.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class1 := Rowan globalNamed: className1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
	class5 := Rowan globalNamed: className5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName2.

"reload branch spec_0038 using different sym dict mappings which moves new pacakge to symbol dict _sampleSymbolDictionaryName1"
	resolvedProject := project asDefinition.
	resolvedProject
		read;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		yourself.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class1 := Rowan globalNamed: className1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
	class5 := Rowan globalNamed: className5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
]

{ #category : 'issue 185' }
RwRowanSample9Test >> testIssue185_move_class_to_symbolDict_1 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"move RowanSample9Class from symbol dictionary RowanSample9_1 to _sampleSymbolDictionaryName2"

	"using default symbol dict name"

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

	"using symbol dict name for package"

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

{ #category : 'issue 185' }
RwRowanSample9Test >> testIssue185_move_newClassVariable_to_symbolDict [
	"https://github.com/dalehenrich/Rowan/issues/185"

	"
		issue_185_1 --> issue_185_7	:: move NewRowanSample4 class with new class var to _sampleSymbolDictionaryName1 (no package rename)
		spec_0038 --> spec_0039 	:: move NewRowanSample9 class with new class var to _sampleSymbolDictionaryName1 (no package rename)
	"

	"based on RwRowanSample4Test>>testIssue185_move_newClassVariable_to_symbolDict"

	| loadSpec projectName projectNames resolvedProject loadedProjects ar className1 class1 className2 class5 
		packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className3 className4 
		className5 x |
	loadSpec := self _loadSpecNamed: 'spec_0038'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.
	className1 := 'RowanSample9Class1'.

	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.

	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load spec_0038 which adds new package and new class in _sampleSymbolDictionaryName2"
	resolvedProject
		read;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName6;
		yourself.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class1 := Rowan globalNamed: className1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
	class5 := Rowan globalNamed: className5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName2.

"load spec_0039 which moves new pacakge to symbol dict _sampleSymbolDictionaryName1"
	loadSpec := self _loadSpecNamed: 'spec_0039'.
	resolvedProject := loadSpec resolveStrict.
	resolvedProject
		read;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		yourself.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class1 := Rowan globalNamed: className1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
	class5 := Rowan globalNamed: className5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
]

{ #category : 'issue 185' }
RwRowanSample9Test >> testIssue185_move_newClassVersion_to_symbolDict [
	"https://github.com/dalehenrich/Rowan/issues/185"

	"
		issue_185_1 --> issue_185_5	:: move NewRowanSample4 class with new class var to _sampleSymbolDictionaryName1 (no package rename)
		spec_0038 --> spec_0040 	:: move NewRowanSample9 class with new inst var to _sampleSymbolDictionaryName1 (no package rename)
	"

	"based on RwRowanSample4Test>>testIssue185_move_newClassVersion_to_symbolDict"

	| loadSpec projectName projectNames resolvedProject loadedProjects ar className1 class1 className2 class5 
		packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className3 className4 
		className5 x |
	loadSpec := self _loadSpecNamed: 'spec_0038'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.
	className1 := 'RowanSample9Class1'.

	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.

	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load spec_0038 which adds new package and new class in _sampleSymbolDictionaryName2"
	resolvedProject
		read;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName6;
		yourself.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class1 := Rowan globalNamed: className1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
	class5 := Rowan globalNamed: className5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName2.

"load spec_0040 which moves new pacakge to symbol dict _sampleSymbolDictionaryName1"
	loadSpec := self _loadSpecNamed: 'spec_0040'.
	resolvedProject := loadSpec resolveStrict.
	resolvedProject
		read;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		yourself.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class1 := Rowan globalNamed: className1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
	class5 := Rowan globalNamed: className5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
]

{ #category : 'issue 185' }
RwRowanSample9Test >> testIssue185_rename_package_move_class [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"
		issue_185_1 --> issue_185_2	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; move NewRowanSample4 to _sampleSymbolDictionaryName1
		spec_0038 --> spec_0041 	:: rename RowanSample9-NewPackage to RowanSample9-RenamedPackage; move NewRowanSample9 to _sampleSymbolDictionaryName1
	"

	"based on RwRowanSample4Test>>testIssue185_rename_package_move_class"

	| loadSpec projectName projectNames resolvedProject loadedProjects ar className1 class1 className2 class5 
		packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className3 className4 
		className5 x packageName7 |
	loadSpec := self _loadSpecNamed: 'spec_0038'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.
	className1 := 'RowanSample9Class1'.

	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.


	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load spec_0038 which adds new package and new class in _sampleSymbolDictionaryName2"
	resolvedProject
		read;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName6;
		yourself.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class1 := Rowan globalNamed: className1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
	class5 := Rowan globalNamed: className5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName2.

"load spec_0041 which moves renamed package to symbol dict _sampleSymbolDictionaryName1"
	loadSpec := self _loadSpecNamed: 'spec_0041'.
	resolvedProject := loadSpec resolveStrict.
	resolvedProject
		read;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		yourself.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class1 := Rowan globalNamed: className1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
	class5 := Rowan globalNamed: className5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
]

{ #category : 'issue 185' }
RwRowanSample9Test >> testIssue185_rename_package_move_classVariable [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"
		issue_185_7 --> issue_185_8	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
		spec_0039 --> spec_0042 	:: rename RowanSample9-NewPackage to RowanSample9-RenamedPackage;
	"

	"based on RwRowanSample4Test>>testIssue185_rename_package_move_class"

	| loadSpec projectName projectNames resolvedProject loadedProjects ar className1 class1 className2 class5 
		packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className3 className4 
		className5 x packageName7 |
	loadSpec := self _loadSpecNamed: 'spec_0039'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.
	className1 := 'RowanSample9Class1'.

	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.


	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load spec_0038 which adds new package and new class in _sampleSymbolDictionaryName2"
	resolvedProject
		read;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName6;
		yourself.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class1 := Rowan globalNamed: className1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
	class5 := Rowan globalNamed: className5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName2.

"load spec_0042 which moves renamed package to symbol dict _sampleSymbolDictionaryName1"
	loadSpec := self _loadSpecNamed: 'spec_0042'.
	resolvedProject := loadSpec resolveStrict.
	resolvedProject
		read;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		yourself.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class1 := Rowan globalNamed: className1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
	class5 := Rowan globalNamed: className5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
]

{ #category : 'issue 185' }
RwRowanSample9Test >> testIssue185_rename_package_move_newClassVersion [

		"https://github.com/dalehenrich/Rowan/issues/185"

	"
		issue_185_1 --> issue_185_6	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
		spec_0038 --> spec_0043 	:: rename RowanSample9-NewPackage to RowanSample9-RenamedPackage;
	"

	"based on RwRowanSample4Test>>testIssue185_rename_package_move_class"

	| loadSpec projectName projectNames resolvedProject loadedProjects ar className1 class1 className2 class5 
		packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className3 className4 
		className5 x packageName7 |
	loadSpec := self _loadSpecNamed: 'spec_0038'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.
	className1 := 'RowanSample9Class1'.

	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.


	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load spec_0038 which adds new package and new class in _sampleSymbolDictionaryName2"
	resolvedProject
		read;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName6;
		yourself.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class1 := Rowan globalNamed: className1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
	class5 := Rowan globalNamed: className5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName2.

"load spec_0043 which moves renamed package to symbol dict _sampleSymbolDictionaryName1"
	loadSpec := self _loadSpecNamed: 'spec_0043'.
	resolvedProject := loadSpec resolveStrict.
	resolvedProject
		read;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		yourself.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class1 := Rowan globalNamed: className1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
	class5 := Rowan globalNamed: className5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
]

{ #category : 'issue 185' }
RwRowanSample9Test >> testIssue185_simple_package_rename [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"issue_185_1 --> issue_185_4	:: Simply rename RowanSample4-NewPackage to RowanSample4-RenamedPackage (no symbol dictionary move)"

	"
		issue_185_1 --> issue_185_4	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
		spec_0038 --> spec_0044 	:: rename RowanSample9-NewPackage to RowanSample9-RenamedPackage;
	"

	"based on RwRowanSample4Test>>testIssue185_simple_package_rename"

	| loadSpec projectName projectNames resolvedProject loadedProjects ar className1 class1 className2 class5 
		packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 className3 className4 
		className5 x packageName7 |
	loadSpec := self _loadSpecNamed: 'spec_0038'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.
	className1 := 'RowanSample9Class1'.

	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className3 := projectName , 'TestCase'.
	className4 := projectName , 'GemStoneTestCase'.
	className5 := 'New', projectName , 'Class'.


	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.
	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load spec_0038 which adds new package and new class in _sampleSymbolDictionaryName2"
	resolvedProject
		read;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName6;
		yourself.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class1 := Rowan globalNamed: className1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
	class5 := Rowan globalNamed: className5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName2.

"load spec_0043 which moves renamed package to symbol dict _sampleSymbolDictionaryName1"
	loadSpec := self _loadSpecNamed: 'spec_0044'.
	resolvedProject := loadSpec resolveStrict.
	resolvedProject
		read;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		yourself.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	class1 := Rowan globalNamed: className1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
	class5 := Rowan globalNamed: className5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (x := (ar first at: 1) name) = self _sampleSymbolDictionaryName1.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue208_adopt_load [

	"https://github.com/dalehenrich/Rowan/issues/208"

	"ported from RowanSample4>>testIssue208_adopt_load"

	"Using non-Rowan api, create the classes in the appropriate symbol dictionaries, adopt into Primer project, then load branch spec_0002 ...
		the primer project should be empty"

		"Create classes in a couple of symbol dictionaries"
		"Create and load the primer project ... with two packages (one for each symbol dictionary) into which the classes will be adopted"
		"Adopt the classes into the primer project"
		"load the RowanSample9 spec_0002 project - overlya the correct project structure over the primer project classes"
		"validate that the primer project packages are empty"

	| primerProjectName projectName symDictName1 symDictName2 symDict theClass instanceMethod classMethod symbolList resolvedProject 
		primerPackageName1 primerPackageName2 loadedPrimerProject componentName loadSpec primerResolvedProject audit |

	primerProjectName := 'PrimerSample9'.
	symDictName1 := self _symbolDictionaryName1.
	symDictName2 := self _symbolDictionaryName2.
	primerPackageName1 := primerProjectName, '-', symDictName1.
	primerPackageName2 := primerProjectName, '-', symDictName2.
	projectName := 'RowanSample9'.
	symbolList := Rowan image symbolList.
	componentName := 'Core'.

	{ primerProjectName. projectName} 
		do: [:pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
					ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	"Create classes in a couple of symbol dictionaries"
	symDict := Rowan image newOrExistingSymbolDictionaryNamed: symDictName1.
	theClass := Object subclass: 'RowanSample9Class1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	symDict := Rowan image newOrExistingSymbolDictionaryNamed: symDictName2.
	theClass := IdentityKeyValueDictionary
		subclass: 'RowanSample9IdentityKeyValueDictionary'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	"Create and load the primer project ... with two packages (one for each symbol dictionary) into which the classes will be adopted"
	primerResolvedProject := RwResolvedProjectV2 new
		projectName: primerProjectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		yourself.

	primerResolvedProject
		addNewComponentNamed: componentName;
		addPackageNamed: primerPackageName1 toComponentNamed: componentName;
		addPackageNamed: primerPackageName2  toComponentNamed: componentName;
		gemstoneSetSymbolDictName: symDictName1 forPackageNamed: primerPackageName1;
		gemstoneSetSymbolDictName: symDictName2 forPackageNamed: primerPackageName2;
		yourself.

	primerResolvedProject resolve; load.

	"Adopt the classes into the primer project"
	Rowan packageTools adopt
		adoptSymbolDictionaryNamed: symDictName1 intoPackageNamed: primerPackageName1;
		adoptSymbolDictionaryNamed: symDictName2 intoPackageNamed: primerPackageName2.

	"load the RowanSample4 project from disk - overly the correct project structure over the primer project classes"
	(self _testRowanProjectsSandbox / projectName) ensureDeleteAll.

	loadSpec := self _loadSpecNamed: 'spec_0002'.
	resolvedProject := loadSpec resolveStrict.

	resolvedProject load.

	"validate that the primer project packages are empty"
	loadedPrimerProject := Rowan projectNamed: primerProjectName.
	loadedPrimerProject packages 
		do: [:loadedPackage |
			self assert: loadedPackage isEmpty ].

	self assert: loadedPrimerProject isDirty.
	self deny: (Rowan projectNamed: projectName) isDirty.

	self assert: (audit := loadedPrimerProject audit) isEmpty.
	self assert: (audit := (Rowan projectNamed: projectName) audit) isEmpty.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue230 [

	"This test is also important for validating the fix to Issue #504:
		https://github.com/GemTalk/Rowan/issues/504#issuecomment-530599164
		At least confirm that it is NOT a good candidate, since this test is now failing
		with the new error introduced in the partial for for Issue #504"
	"https://github.com/dalehenrich/Rowan/issues/230"

	"new class version for class being loaded in after adopt --- initial RowanSample9Class1 class created with an instance variable that's not present in spec_0002, 
		so we get new version when class is loaded from disk"

	"ported from RowanSample4>>testIssue208_adopt_load"

	"based on RowanSample9>>testIssue208_adopt_load"

	| primerProjectName projectName symDictName1 symDictName2 symDict theClass instanceMethod classMethod symbolList resolvedProject 
		primerPackageName1 primerPackageName2 loadedPrimerProject componentName loadSpec primerResolvedProject audit className class newClass loadedProjects project |

	primerProjectName := 'PrimerSample9'.
	symDictName1 := self _symbolDictionaryName1.
	symDictName2 := self _symbolDictionaryName2.
	primerPackageName1 := primerProjectName, '-', symDictName1.
	primerPackageName2 := primerProjectName, '-', symDictName2.
	projectName := 'RowanSample9'.
	symbolList := Rowan image symbolList.
	componentName := 'Core'.
	className := 'RowanSample9Class1'.

	{ primerProjectName. projectName} 
		do: [:pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
					ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	"Create classes in a couple of symbol dictionaries"
	symDict := Rowan image newOrExistingSymbolDictionaryNamed: symDictName1.
	theClass := Object subclass: className
		instVarNames: #('xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().	"new class version will e created after load of spec_0002"
	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	symDict := Rowan image newOrExistingSymbolDictionaryNamed: symDictName2.
	theClass := IdentityKeyValueDictionary
		subclass: 'RowanSample9IdentityKeyValueDictionary'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	"Create and load the primer project ... with two packages (one for each symbol dictionary) into which the classes will be adopted"
	primerResolvedProject := RwResolvedProjectV2 new
		projectName: primerProjectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		yourself.

	primerResolvedProject
		addNewComponentNamed: componentName;
		addPackageNamed: primerPackageName1 toComponentNamed: componentName;
		addPackageNamed: primerPackageName2  toComponentNamed: componentName;
		gemstoneSetSymbolDictName: symDictName1 forPackageNamed: primerPackageName1;
		gemstoneSetSymbolDictName: symDictName2 forPackageNamed: primerPackageName2;
		yourself.

	loadedProjects := primerResolvedProject resolve; load.
	project := loadedProjects first.
	audit := project audit.
	self assert: audit isEmpty.

	"Adopt the classes into the primer project"
	Rowan packageTools adopt
		adoptSymbolDictionaryNamed: symDictName1 intoPackageNamed: primerPackageName1;
		adoptSymbolDictionaryNamed: symDictName2 intoPackageNamed: primerPackageName2.

	"load the RowanSample4 project from disk - overly the correct project structure over the primer project classes"
	(self _testRowanProjectsSandbox / projectName) ensureDeleteAll.

	loadSpec := self _loadSpecNamed: 'spec_0002'.
	resolvedProject := loadSpec resolveStrict.

	class := Rowan globalNamed: className.
	loadedProjects := resolvedProject load.
	project := loadedProjects first.
	audit := project audit.
	self assert: audit isEmpty.

	newClass := Rowan globalNamed: className.

"validate that we got a new class version"
	self assert: class ~~ newClass.

"validate that the primer project packages are empty"
	loadedPrimerProject := Rowan projectNamed: primerProjectName.
	loadedPrimerProject packages 
		do: [:loadedPackage |
			self assert: loadedPackage isEmpty ].

	self assert: loadedPrimerProject isDirty.
	self deny: (Rowan projectNamed: projectName) isDirty.

	self assert: (audit := loadedPrimerProject audit) isEmpty.
	self assert: (audit := (Rowan projectNamed: projectName) audit) isEmpty.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue284 [

	"https://github.com/dalehenrich/Rowan/issues/284"

	"issue_185_0 --> issue_284_0"

	"spec_0038 --> spec_0045		:: benign commit"

	"ensure that loaded commit id is changed after loading a benign commit (no definition changes)"

	| loadSpec projectName projectNames resolvedProject loadedProjects project loadedCommitId x |
	loadSpec := self _loadSpecNamed: 'spec_0038'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.
	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone-Tests';
		yourself.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"get project and record load commit id"
	project := loadedProjects at: 1.
	loadedCommitId := project loadedCommitId.

"benign load sequence - loaded commit id should change"
	loadSpec := self _loadSpecNamed: 'spec_0045'.

"resolve project"
	resolvedProject := loadSpec resolve.

	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		yourself.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self deny: loadedCommitId = (x := project loadedCommitId). "loaded commit id SHOULD change"

]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue295_rename_package_move_newClassVersion_newProject_1 [

	"This test is also important for validating the fix to Issue #504:
		https://github.com/GemTalk/Rowan/issues/504#issuecomment-530599164
		At least confirm that it is NOT a good candidate, since this test is now failing
		with the new error introduced in the partial for for Issue #504"

	"https://github.com/dalehenrich/Rowan/issues/295"

	"port of RowanSample4>>testIssue295_rename_package_move_newClassVersion_newProject_1"

	"Error creating a new class version while moving to a new package in a new project and a new symbol dictionary"

	"issue_295_1 --> issue_295_2	:: "

	"spec_0040 --> spec_0046		:: "

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0040'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.
	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'NewPackage';
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone';
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone-Tests';
		yourself.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"benign load sequence - loaded commit id should change"
	loadSpec := self _loadSpecNamed: 'spec_0046'.

"resolve project"
	resolvedProject := loadSpec resolve.
	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone_295';
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone-Tests_295';
		yourself.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"unload"
	(loadedProjects at: 1) unload.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue295_rename_package_move_newClassVersion_with_subclass_newProject [

	"attempting to reproduce the actual issue (_1 doesn't reproduce problem, but should remain static to ensure behavior does not change detrimentally"

	"https://github.com/dalehenrich/Rowan/issues/295"

	"Error creating a new class version of a superclass while moving to a new package in a new project and a new symbol dictionary"

	"issue_295_4 --> issue_295_5"

	"spec_0047 --> spec_0048"

	| loadSpec projectName projectNames resolvedProject loadedProjects project |
	loadSpec := self _loadSpecNamed: 'spec_0047'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.
	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'NewPackage';
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone';
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone-Tests';
		yourself.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"get project"
	project := loadedProjects at: 1.
	
"resolve project"
	resolvedProject := project asDefinition.
	resolvedProject loadSpecification customConditionalAttributes: resolvedProject loadSpecification customConditionalAttributes, {'renamed'}. "since we're not loading the spec from disk"
	resolvedProject 
		checkout: 'spec_0048';
		read;
		yourself.

	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone_295';
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone-Tests_295';
		yourself.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue300 [

	"https://github.com/dalehenrich/Rowan/issues/300"

	"regression test for bug ... mixed symbolic link and absolute path referencing same git repository"

	| loadSpec projectName resolvedProject loadedProjects projectsHome projectsHome_symLink symLinkName commandLine |
	loadSpec := self _loadSpecNamed: 'spec_0002'.

	projectName := loadSpec projectName.

	{ projectName } do: [:projName |
		(Rowan image loadedProjectNamed: projName ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"prepare directories"

	projectsHome :=  self _testRowanProjectsSandbox / 'issue_300_dir'.
	projectsHome ensureDeleteAll.

"resolve project"
	loadSpec projectsHome: projectsHome.
	resolvedProject := loadSpec resolve.
"load project"
	loadedProjects := resolvedProject load.
"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

	self assert: (projectsHome / projectName) exists.

	self assert: (Rowan projectNamed: projectName) currentBranchName = 'spec_0002'.

"create symbolic link..."
	self assert: (projectsHome / projectName) exists.

	projectsHome_symLink  := self _testRowanProjectsSandbox / 'rowan_issue_300'.
	projectsHome_symLink ensureDeleteAll.

	symLinkName := 'issue_300_symLink'.
	commandLine := 'set -e;  cd ' , projectsHome pathString, '; mkdir ', projectsHome_symLink pathString, '; ln -s ', projectsHome pathString , ' ', projectsHome_symLink pathString, '/', symLinkName.
	Rowan gitTools performOnServer: commandLine.

	self assert: (projectsHome_symLink / symLinkName / projectName ) exists.

"...and now resolve project using symbolic link"
	loadSpec 
		projectsHome: projectsHome_symLink / symLinkName;
		yourself.

	resolvedProject := loadSpec resolve.
"load project"
	loadedProjects := resolvedProject load.
"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

	self assert: (projectsHome_symLink / symLinkName / projectName ) exists.

	self assert: (Rowan projectNamed: projectName) currentBranchName = 'spec_0002'.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue304 [

	"https://github.com/dalehenrich/Rowan/issues/304"

	"ported from RowanSampl4>>testIssue304"

	"
		Original test comment:

			load a different config and group using the spec url load api ... SHA needs to change as well

		For V2, the test should:

			load two different load specs that specify a different revision and different customConditionalAttributes
			spec_0048 --> spec_0049
"

	| loadSpec projectName projectNames resolvedProject loadedProjects fileUrl project loadedCommitId x |
	loadSpec := self _loadSpecNamed: 'spec_0049'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.
	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		yourself.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"record the loaded commit id"
	self assert: project currentBranchName =  'spec_0049'.

	loadedCommitId := project loadedCommitId.

"Use a different load spec in spec_0049, that has a different revision (spec_0048) and a different customConditionalAttributes"
	fileUrl := 'file:' , (resolvedProject specsRoot / 'Issue_304', 'ston') pathString.
	loadSpec := (RwSpecification fromUrl: fileUrl)
		projectsHome: self _testRowanProjectsSandbox;
		yourself.
	
"resolve project"
	resolvedProject := loadSpec resolveStrict.

	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		yourself.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: project currentBranchName =  'spec_0048'.
	self deny: (x := project loadedCommitId) = loadedCommitId.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue305 [

	"https://github.com/dalehenrich/Rowan/issues/305"

	"load spec_0049, then remove tests attribute and reload"

	| loadSpec projectName projectNames resolvedProject loadedProjects project x packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 packageName7 packageName8 className6 y |
	loadSpec := self _loadSpecNamed: 'spec_0049'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	packageName1 := projectName , '-' , 'Core', '_295'.
	packageName2 := projectName , '-' , 'Extensions', '_295'.
	packageName3 := projectName , '-' , 'GemStone', '_295'.
	packageName4 := projectName , '-' , 'GemStone-Tests', '_295'.
	packageName5 := projectName , '-' , 'Tests', '_295'.
	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.
	packageName8 := projectName , '-' , 'Tests-RenamedPackage'.
	className6 := 'New', projectName , 'Subclass'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.
	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		yourself.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (x := resolvedProject packageNames asArray sort) = (y := { packageName1 . packageName2 . packageName3 . packageName4 . packageName5 . packageName7 . packageName8 } sort).
	self assert: (x := Rowan globalNamed: className6) notNil.	

"resolve project"
	loadSpec customConditionalAttributes:  { }. "unload several packages: 'RowanSample9-GemStone-Tests_295',
		'RowanSample9-Tests_295', 'RowanSample9-Tests-RenamedPackage' 'RowanSample9-RenamedPackage' "
	resolvedProject := loadSpec resolve.

	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		yourself.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (x := resolvedProject packageNames asArray sort) = (y := { packageName1 . packageName2 . packageName3 } sort).
	self assert: (x := Rowan globalNamed: className6) isNil.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue460_0 [

	"confirm that normal load sequence can be executed without errors"

	"https://github.com/dalehenrich/Rowan/issues/260"

	"Error creating a new class version of a superclass while moving to a new package in a new project and a new symbol dictionary"

	"issue_295_6 --> issue_295_5	:: "
	"spec_0050 --> spec_0048		:: "

	| projectName projectNames loadSpec loadedProjects resolvedProject packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 packageName7 packageName8 x y className6 oldClass6 class6 ar className1 class1 className2 class2 className5 class5 oldClass5 |
	loadSpec := self _loadSpecNamed: 'spec_0050'.

	projectName := 'RowanSample9'.
	projectNames := {projectName . projectName, '_295'}.

	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.
	packageName8 := projectName , '-' , 'Tests-RenamedPackage'.
	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className5 := 'New', projectName , 'Class'.
	className6 := 'New', projectName , 'Subclass'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	projectNames := { projectName }.

"resolve project"
	resolvedProject := loadSpec resolve.
	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'NewPackage';
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone';
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone-Tests';
		yourself.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.

	self assert: (x := resolvedProject packageNames asArray sort) = (y := { packageName1 . packageName2 . packageName3 . packageName4 . packageName5 . packageName6 } sort).
	self assert: (class1 := Rowan globalNamed: className1) notNil.	
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName.
	self assert: (class2 := Rowan globalNamed: className2) notNil.	
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class2.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2.
	self assert: (oldClass5 := Rowan globalNamed: className5) notNil.	
	ar := Rowan image symbolList dictionariesAndSymbolsOf: oldClass5.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2.
	self assert: (oldClass6 := Rowan globalNamed: className6) notNil.	
	ar := Rowan image symbolList dictionariesAndSymbolsOf: oldClass6.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2.

"trigger bug ... load spec_0048"
	loadSpec := self _loadSpecNamed: 'spec_0048'.

"resolve project"
	resolvedProject := loadSpec resolveStrict.
	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName4 forPackageNamed: projectName , '-' , 'GemStone_295';
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName4 forPackageNamed: projectName , '-' , 'GemStone-Tests_295';
		yourself.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	packageName1 := projectName , '-' , 'Core', '_295'.
	packageName2 := projectName , '-' , 'Extensions', '_295'.
	packageName3 := projectName , '-' , 'GemStone', '_295'.
	packageName4 := projectName , '-' , 'GemStone-Tests', '_295'.
	packageName5 := projectName , '-' , 'Tests', '_295'.

	self assert: (x := resolvedProject packageNames asArray sort) = (y := { packageName1 . packageName2 . packageName3 . packageName4 . packageName5 . packageName7 . packageName8 } sort).
	self assert: (class1 := Rowan globalNamed: className1) notNil.	
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (ar first at: 1) name = self _sampleSymbolDictionaryName3.
	self assert: (class2 := Rowan globalNamed: className2) notNil.	
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class2.
	self assert: (ar first at: 1) name = self _sampleSymbolDictionaryName4.
	self assert: (class5 := Rowan globalNamed: className5) notNil.	
	self assert: oldClass5 ~~ class5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (ar first at: 1) name = self _sampleSymbolDictionaryName3.
	self assert: (class6 := Rowan globalNamed: className6) notNil.	
	self assert: oldClass6 ~~ class6.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class6.
	self assert: (ar first at: 1) name = self _sampleSymbolDictionaryName3.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue460_1 [

	"https://github.com/dalehenrich/Rowan/issues/260"

	"Error creating a new class version of a superclass while moving to a new package in a new project and a new symbol dictionary"

	"issue_295_6 --> issue_295_5	:: "
	"spec_0050 --> spec_0048		:: "

	| projectName projectNames loadSpec loadedProjects resolvedProject packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 packageName7 packageName8 x y className6 oldClass6 class6 ar className1 class1 className2 class2 className5 class5 oldClass5 projectName2 resolvedProject2 project oldResolvedProject projectSetDefinition |
	loadSpec := self _loadSpecNamed: 'spec_0050'.

	projectName := 'RowanSample9'.
	projectName2 := projectName, '_295'.
	projectNames := {projectName . projectName2}.

	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.
	packageName8 := projectName , '-' , 'Tests-RenamedPackage'.
	className1 := projectName , 'Class1'.
	className2 :=  projectName , 'IdentityKeyValueDictionary'.
	className5 := 'New', projectName , 'Class'.
	className6 := 'New', projectName , 'Subclass'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	projectNames := { projectName }.

"resolve project"
	resolvedProject := loadSpec resolve.
	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'NewPackage';
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone';
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone-Tests';
		yourself.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Extensions'.
	packageName3 := projectName , '-' , 'GemStone'.
	packageName4 := projectName , '-' , 'GemStone-Tests'.
	packageName5 := projectName , '-' , 'Tests'.

	self assert: (x := resolvedProject packageNames asArray sort) = (y := { packageName1 . packageName2 . packageName3 . packageName4 . packageName5 . packageName6 } sort).
	self assert: (class1 := Rowan globalNamed: className1) notNil.	
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName.
	self assert: (class2 := Rowan globalNamed: className2) notNil.	
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class2.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2.
	self assert: (oldClass5 := Rowan globalNamed: className5) notNil.	
	ar := Rowan image symbolList dictionariesAndSymbolsOf: oldClass5.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2.
	self assert: (oldClass6 := Rowan globalNamed: className6) notNil.	
	ar := Rowan image symbolList dictionariesAndSymbolsOf: oldClass6.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2.

"trigger bug ... load spec_0048"
	loadSpec := self _loadSpecNamed: 'spec_0048'.

"resolve project for spec_0048"
	projectNames := {projectName . projectName2}.

	loadSpec
		projectName: projectName2;
		projectAlias: projectName;
		yourself.
	resolvedProject := loadSpec resolveStrict.

"need to add old project definition with all classes and extensions removed to the projectSet Definition to reproduce bug"

	projectSetDefinition := resolvedProject readProjectSet.
	resolvedProject2 := projectSetDefinition projectNamed: projectName2.
	resolvedProject2
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName4 forPackageNamed: projectName , '-' , 'GemStone_295';
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName4 forPackageNamed: projectName , '-' , 'GemStone-Tests_295';
		yourself.

	self assert: (resolvedProject2 projectDefinitionSourceProperty = RwLoadedProject _projectDiskDefinitionSourceValue).

	oldResolvedProject := project asDefinition.
	projectSetDefinition addProject: oldResolvedProject.

	oldResolvedProject packages do: [:pkgDefinition |
	    pkgDefinition classDefinitions do: [:classDefinition |
	        pkgDefinition removeClassDefinition: classDefinition ].
	    pkgDefinition classExtensions do: [:classExtension |
        	pkgDefinition removeClassExtensionDefinition: classExtension ]].

"load project set"
	loadedProjects := Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.
	self assert: (Rowan projectNamed: projectName) isDirty.
	self deny: (Rowan projectNamed: projectName2) isDirty.
	Rowan projectTools load markProjectSetNotDirty: projectSetDefinition.
	self deny: (Rowan projectNamed: projectName) isDirty.

"validate"
	self
		_standard_validate: resolvedProject2
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	packageName1 := projectName , '-' , 'Core', '_295'.
	packageName2 := projectName , '-' , 'Extensions', '_295'.
	packageName3 := projectName , '-' , 'GemStone', '_295'.
	packageName4 := projectName , '-' , 'GemStone-Tests', '_295'.
	packageName5 := projectName , '-' , 'Tests', '_295'.

	self assert: (x := resolvedProject packageNames asArray sort) = (y := { packageName1 . packageName2 . packageName3 . packageName4 . packageName5 . packageName7 . packageName8 } sort).
	self assert: (class1 := Rowan globalNamed: className1) notNil.	
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class1.
	self assert: (ar first at: 1) name = self _sampleSymbolDictionaryName3.
	self assert: (class2 := Rowan globalNamed: className2) notNil.	
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class2.
	self assert: (ar first at: 1) name = (x := self _sampleSymbolDictionaryName4).
	self assert: (class5 := Rowan globalNamed: className5) notNil.	
	self assert: oldClass5 ~~ class5.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class5.
	self assert: (ar first at: 1) name = (x := self _sampleSymbolDictionaryName3).
	self assert: (class6 := Rowan globalNamed: className6) notNil.	
	self assert: oldClass6 ~~ class6.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class6.
	self assert: (ar first at: 1) name = self _sampleSymbolDictionaryName3.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue490_rename_package_move_newClassVersion_newProject_1 [

	"https://github.com/dalehenrich/Rowan/issues/490"

	"extension package that should have been emptied when loading a new project that is taking over management of the classes and axtensions methods"

	"issue_295_1 --> issue_295_2	:: "
	"spec_0040 --> spec_0046		:: issue_295_1 has a couple more ivs than spec_0040, but otherwise the same"

	| loadSpec projectName projectNames resolvedProject loadedProjects project |
	loadSpec := self _loadSpecNamed: 'spec_0040'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.
	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'NewPackage';
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone';
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone-Tests';
		yourself.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"get project"
	project := loadedProjects at: 1.
	
"resolve project"
	loadSpec := self _loadSpecNamed: 'spec_0046'.

	resolvedProject := loadSpec resolveStrict.

	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone_295';
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone-Tests_295';
		yourself.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue493 [

	"https://github.com/dalehenrich/Rowan/issues/493"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0051'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project using Core component"
	resolvedProject readProjectComponentNames: {'Core'}.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"load project using Issue_493 component"
	resolvedProject readProjectComponentNames: {'Issue_493'}.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
]

{ #category : 'issue 493' }
RwRowanSample9Test >> testIssue493_RowanSample2 [
	"load spec_0035; fiddle symbol dictionary, load spec_0036"

	"https://github.com/GemTalk/Rowan/issues/313"

	"testIssue_587 is the same test without introducing class versions"

	"the fix for this test will involve changing the code that was written to addresss issue #313;
		which has been reopened"

	"https://github.com/dalehenrich/Rowan/issues/493"

	"duplicate of RwRowanSample2Test debug: #testIssue493 (no existing LoadedMethod found for the old compiledMethod.) ... "

	"https://github.com/GemTalk/Rowan/issues/254  (no existing LoadedMethod found for the old compiledMethod.) ... "

	"using the migration tests set of definitions"

	| loadSpec projectName resolvedProject loadedProjects project  audit |
	loadSpec := self 
		_migrationLoadSpecNamed:  'spec_0035' 
		projectName: 'RowanSample9' 
		projectAlias: 'RowanSample9_Issue_493' 
		gitUrl: 'https://git@github.com/dalehenrich/RowanSample9'.

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
		gitUrl: 'https://git@github.com/dalehenrich/RowanSample9'.

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

{ #category : 'tests' }
RwRowanSample9Test >> testIssue571 [
	"record/reuse/update the platformConditionalAttributes used to read the project from disk"

	"https://github.com/GemTalk/Rowan/issues/571"

	| loadSpec projectName projectNames resolvedProject loadedProjects customConditionalAttributes  
		defaultPlatformAttributes expectedProjectDefinitionPlatformConditionalAttributes  
		packageName1 theLoadSpec project thePlatformConditionalAttributes |
	loadSpec := self _loadSpecNamed: 'spec_0032'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.
	packageName1 := 'RowanSample9-Specs-V1'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	customConditionalAttributes := loadSpec customConditionalAttributes.
	defaultPlatformAttributes := Rowan platformConditionalAttributes.
	expectedProjectDefinitionPlatformConditionalAttributes := customConditionalAttributes, defaultPlatformAttributes.

"resolve project - baseline"
	resolvedProject := loadSpec resolve.

	self assert: resolvedProject projectDefinitionPlatformConditionalAttributes = expectedProjectDefinitionPlatformConditionalAttributes.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	self
		_validate_571: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
		packageName: packageName1
		present: true.

"resolve project using loadSpec - remove v1 attribute, resulting in packageName1 being unloaded"
	theLoadSpec := loadSpec copy.
	theLoadSpec customConditionalAttributes remove: 'v1' ifAbsent: [self error: 'missing attribute'].
	resolvedProject := theLoadSpec resolve.
	resolvedProject load.
"validate"
	self
		_validate_571: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
		packageName: packageName1
		present: false.

"resolve project using loadSpec - add v1 attribute back, resulting in packageName1 being loaded"
	theLoadSpec := theLoadSpec copy.
	theLoadSpec customConditionalAttributes add: 'v1' .
	resolvedProject := theLoadSpec resolve.
	resolvedProject load.
"validate"
	thePlatformConditionalAttributes := (self
		_validate_571: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
		packageName: packageName1
		present: true) copy.

"resolve project from loaded definition - remove v1 attribute, resulting in packageName1 being unloaded"
	resolvedProject := project asDefinition.
	self assert: resolvedProject projectDefinitionPlatformConditionalAttributes =
		thePlatformConditionalAttributes.

	thePlatformConditionalAttributes remove: 'v1' ifAbsent: [self error: 'missing attribute'].

	resolvedProject read: thePlatformConditionalAttributes.
	loadedProjects := resolvedProject load.
"validate"
	thePlatformConditionalAttributes := (self
		_validate_571: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
		packageName: packageName1
		present: false) copy.

"resolve project from loaded definition - add v1 attribute back, resulting in packageName1 being loaded"
	resolvedProject := project asDefinition.
	self assert: resolvedProject projectDefinitionPlatformConditionalAttributes =
		thePlatformConditionalAttributes.

	thePlatformConditionalAttributes add: 'v1'.

	loadedProjects := resolvedProject loadProjectSet: thePlatformConditionalAttributes.
"validate"
	thePlatformConditionalAttributes := (self
		_validate_571: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
		packageName: packageName1
		present: true) copy.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue743_0 [
	"Class>>category: hooked up to Rowan"

	"https://github.com/GemTalk/Rowan/issues/743"

	| loadSpec projectName projectNames resolvedProject loadedProjects project audit class category definedProject projectModification packagesModification elementsModified packageModification classesModification classModification propertiesModification propertyModification oldCategory confirmationBlock |
	loadSpec := self _loadSpecNamed: 'spec_0001'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	confirmationBlock := [:oldCat :newCat |
		"confirm that the class category change shows up as a modification"
		projectModification := project asDefinition compareAgainstBase: definedProject _resolvedProject.
		self deny: projectModification isEmpty.
		self assert: projectModification propertiesModification isEmpty.
		packagesModification := projectModification packagesModification.
		self deny: packagesModification isEmpty.
		elementsModified := packagesModification elementsModified.
		self assert: elementsModified size = 1.
		packageModification := elementsModified at: 'RowanSample9-Core'.
		self assert: packageModification propertiesModification isEmpty.
		self assert: packageModification classExtensionsModification isEmpty.
		classesModification := packageModification classesModification.
		self deny: classesModification isEmpty.
		elementsModified := classesModification elementsModified.
		self assert: elementsModified size = 1.
		classModification := elementsModified at: 'RowanSample9Class1'.
		self assert: classModification instanceMethodsModification isEmpty.
		self assert: classModification classMethodsModification isEmpty.
		propertiesModification := classModification propertiesModification.
		self deny: propertiesModification isEmpty.
		elementsModified := propertiesModification elementsModified.
		self assert: elementsModified size = 1.
		propertyModification := elementsModified at: 'category'.
		self assert: propertyModification oldValue equals: oldCat.
		self assert: propertyModification newValue equals: newCat.
	].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	project := loadedProjects at: 1.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: 'RowanSample9Class1'.
	self assert: class category equals: class rwCategory.
	oldCategory := class category.

"change class category"
	definedProject := project defined.
	category := 'aaa'.
	class rwCategory: category.

"validate"
	self assert: (audit := project audit) isEmpty.
	self assert: class category equals: category.
	confirmationBlock value: oldCategory value: category.

"change class category to nil"
	definedProject := project defined.
	oldCategory := category.
	category := nil.
	class rwCategory: category.
	category := 'Kernel'. "default category when value of category is nil"

"validate"
	self assert: (audit := project audit) isEmpty.
	self assert: class category equals: category.
	confirmationBlock value: oldCategory value: nil. "actual value compared in block"
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue743_1 [
	"Behavior>>moveMethod:toCategory: hooked up to Rowan"

	"https://github.com/GemTalk/Rowan/issues/743"

	| loadSpec projectName projectNames resolvedProject loadedProjects project audit class category definedProject projectModification packagesModification elementsModified packageModification classesModification classModification propertiesModification propertyModification oldCategory confirmationBlock methodModification selector |
	loadSpec := self _loadSpecNamed: 'spec_0001'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	confirmationBlock := [:oldCat :newCat |
		"confirm that the class category change shows up as a modification"
		projectModification := project asDefinition compareAgainstBase: definedProject _resolvedProject.
		self deny: projectModification isEmpty.
		self assert: projectModification propertiesModification isEmpty.
		packagesModification := projectModification packagesModification.
		self deny: packagesModification isEmpty.
		elementsModified := packagesModification elementsModified.
		self assert: elementsModified size = 1.
		packageModification := elementsModified at: 'RowanSample9-Core'.
		self assert: packageModification propertiesModification isEmpty.
		self assert: packageModification classExtensionsModification isEmpty.
		classesModification := packageModification classesModification.
		self deny: classesModification isEmpty.
		elementsModified := classesModification elementsModified.
		self assert: elementsModified size = 1.
		classModification := elementsModified at: 'RowanSample9Class1'.
		self deny: classModification instanceMethodsModification isEmpty.
		elementsModified :=  classModification instanceMethodsModification elementsModified.
		methodModification := elementsModified at: #instanceFoo.
		propertiesModification := methodModification propertiesModification.
		self deny: propertiesModification isEmpty.
		elementsModified := propertiesModification elementsModified.
		propertyModification := elementsModified at: 'protocol'.
		self assert: propertyModification oldValue equals: oldCat.
		self assert: propertyModification newValue equals: newCat.
	].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	project := loadedProjects at: 1.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: 'RowanSample9Class1'.

	[	
		Rowan gemstoneTools topaz currentTopazPackageName: class rowanPackageName.

		"add a method to class in protocol 'other'"
			definedProject := project defined.
			category := 'other'.
			selector := #instanceFoo.
			class
				compileMethod: 'instanceFoo ^true'
					dictionaries: System myUserProfile symbolList
					category: category.

		"validate"
			self assert: (audit := project audit) isEmpty.
			self assert: (class categoryOfSelector: selector) asString equals: category.
			confirmationBlock value: oldCategory value: category.

		"add new category and move method to new category"
			definedProject := project defined.
			oldCategory := category.
			category := 'bar'.
			class addCategory: category.
			class moveMethod: selector toCategory: category.

		"validate"
			self assert: (audit := project audit) isEmpty.
			self assert: (class categoryOfSelector: selector) asString equals: category.
			confirmationBlock value: oldCategory value: category
	] ensure: [ Rowan gemstoneTools topaz currentTopazPackageName: nil ]
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue761 [
	"Modify and write an extension method"

	"https://github.com/GemTalk/Rowan/issues/761"

	| loadSpec projectName projectNames resolvedProject definedProject methodDef |
	loadSpec := self _loadSpecNamed: 'spec_0079'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"read project"
	definedProject := resolvedProject read.

	methodDef := ((definedProject packageNamed: 'RowanSample9-Extensions')
		classExtensionDefinitionNamed: 'RowanSample9Class1') classMethodDefinitions at:#bar.
	methodDef source: 'bar "modified method" ^''bar'''.

"write project without error"
	[ definedProject exportPackages ]
		ensure: [ (self _testRowanProjectsSandbox / projectName) ensureDeleteAll ]
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue766 [
	"packages read from a Pharo tonel format v1 repository are not exported to .gs files correctly"

	"https://github.com/GemTalk/Rowan/issues/766"

	| loadSpec projectName projectsHome resolvedProject repositoryRootPath 
		projectSetDefinition projectSetModification visitor stream |
	loadSpec := self _loadSpecNamed: 'spec_0080'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

	projectsHome := self _testRowanProjectsSandbox.
	(projectsHome / projectName) ensureDeleteAll.

"resolve project"
	resolvedProject := loadSpec resolve.

"export to .gs file"
	repositoryRootPath := projectsHome / projectName / 'topaz'.
	repositoryRootPath ensureCreateDirectory.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: resolvedProject.
	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitorV2 new
		excludeRemoveAllMethods: true;
		logCreation: true;
		repositoryRootPath: repositoryRootPath;
		topazFilename: projectName;
		yourself.
	visitor visit: projectSetModification.

	stream := ZnBufferedReadStream on: visitor topazFileReference readStream.
	self assert: stream contents equals: self _expectedIssue766GsFileContents
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue777_01 [
	"keys  and values in definitions structures should not be Symbols"

	"https://github.com/GemTalk/Rowan/issues/777"

	| loadSpec projectName resolvedProject |
	loadSpec := self _loadSpecNamed: 'spec_0002'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"validate"
	resolvedProject packages keysAndValuesDo: [:packageName :packageDef | 
		| methodDefs |
		methodDefs := Array new.
		self deny: (packageName isKindOf: Symbol).
		packageDef classDefinitions keysAndValuesDo: [:className :classDef |
			self deny: (className isKindOf: Symbol).
			classDef properties keysAndValuesDo: [:key :value | 
				self deny: (key isKindOf: Symbol).
				value _isArray
					ifTrue: [ value do: [:each | self deny: (each isKindOf: Symbol) ] ]
					ifFalse: [ self deny: (value isKindOf: Symbol) ] ].
			methodDefs 
				addAll: (classDef instanceMethodDefinitions values);
				addAll: (classDef classMethodDefinitions values) ].
		packageDef classExtensions keysAndValuesDo: [:className :classDef |
			self deny: (className isKindOf: Symbol).
			classDef properties keysAndValuesDo: [:key :value | 
				self deny: (key isKindOf: Symbol).
				value _isArray
					ifTrue: [ value do: [:each | self deny: (each isKindOf: Symbol) ] ]
					ifFalse: [ self deny: (value isKindOf: Symbol) ].
			methodDefs 
				addAll: (classDef instanceMethodDefinitions values);
				addAll: (classDef classMethodDefinitions values) ] ].
		methodDefs do: [:methodDef |
			methodDef properties keysAndValuesDo: [:key :value |
				self deny: (key isKindOf: Symbol).
				key = 'selector'
					ifTrue: [  self assert: (value isKindOf: Symbol) ]
					ifFalse: [ self deny: (value isKindOf: Symbol) ] ] ] ]
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue777_02 [
	"keys  and values in definitions structures should not be Symbols"

	"https://github.com/GemTalk/Rowan/issues/777"

	| loadSpec projectName resolvedProject |
	loadSpec := self _loadSpecNamed: 'spec_0080'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"validate"
	resolvedProject packages keysAndValuesDo: [:packageName :packageDef | 
		| methodDefs |
		methodDefs := Array new.
		self deny: (packageName isKindOf: Symbol).
		packageDef classDefinitions keysAndValuesDo: [:className :classDef |
			self deny: (className isKindOf: Symbol).
			classDef properties keysAndValuesDo: [:key :value | 
				self deny: (key isKindOf: Symbol).
				value _isArray
					ifTrue: [ value do: [:each | self deny: (each isKindOf: Symbol) ] ]
					ifFalse: [ self deny: (value isKindOf: Symbol) ] ].
			methodDefs 
				addAll: (classDef instanceMethodDefinitions values);
				addAll: (classDef classMethodDefinitions values) ].
		packageDef classExtensions keysAndValuesDo: [:className :classDef |
			self deny: (className isKindOf: Symbol).
			classDef properties keysAndValuesDo: [:key :value | 
				self deny: (key isKindOf: Symbol).
				value _isArray
					ifTrue: [ value do: [:each | self deny: (each isKindOf: Symbol) ] ]
					ifFalse: [ self deny: (value isKindOf: Symbol) ].
			methodDefs 
				addAll: (classDef instanceMethodDefinitions values);
				addAll: (classDef classMethodDefinitions values) ] ].
		methodDefs do: [:methodDef |
			methodDef properties keysAndValuesDo: [:key :value |
				self deny: (key isKindOf: Symbol).
				key = 'selector'
					ifTrue: [  self assert: (value isKindOf: Symbol) ]
					ifFalse: [ self deny: (value isKindOf: Symbol) ] ] ] ]
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue830_1 [
	"Test coverage for the method 
		Behavior >> _rwCompileMethodForConditionalPackaging:symbolList:category:environmentId:ifUnpackagedDo:"

	"https://github.com/GemTalk/Rowan/issues/830"

	| loadSpec projectName loadedProject packageName1 packageName2 class method audit |
	loadSpec := self _loadSpecNamed: 'spec_0082'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"setup"
	packageName1 := projectName, '-Core'.
	packageName2 := projectName, '-Extensions'.

"case1: protocol begins with a '*' and DOES map to an existing package in project 
		using RowanHybrid package convention ... ignore currentTopazPackageName"
	loadedProject := loadSpec resolve load first.
	class := Rowan globalNamed: projectName, 'Class1'.
	self assert: loadedProject packageConvention equals: 'RowanHybrid'.
	Rowan gemstoneTools topaz currentTopazPackageName: packageName1.
	method := class
		compileMethod: 'issue830'
		dictionaries: Rowan image symbolList
		category: '*', packageName2 asLowercase
		environmentId: 0.
	self assert: method rowanPackageName equals: packageName2.
	self assert: (audit := loadedProject audit) isEmpty.
	loadedProject unload.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue830_2a [
	"Test coverage for the method 
		Behavior >> _rwCompileMethodForConditionalPackaging:symbolList:category:environmentId:ifUnpackagedDo:"

	"https://github.com/GemTalk/Rowan/issues/830"

	| loadSpec projectName loadedProject packageName1 packageName2 class method audit |
	loadSpec := self _loadSpecNamed: 'spec_0057'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"setup"
	packageName1 := projectName, '-Core'.
	packageName2 := projectName, '-Extensions'.

"case2a: protocol begins with a '*' and DOES NOT map to an existing package and  
		package of class in package with Rowan package convention. Ignore 
		currentTopazPackageName."
	loadedProject := loadSpec resolve load first.
	class := Rowan globalNamed: projectName, 'Class1'.
	self assert: loadedProject packageConvention equals: 'Rowan'.
	self assert: class rowanPackageName equals: packageName1.
	[
		Rowan gemstoneTools topaz currentTopazPackageName: packageName2.
		method := class
			compileMethod: 'issue830'
			dictionaries: Rowan image symbolList
			category: '*not-a-package-name'
			environmentId: 0 ] ensure: [ Rowan gemstoneTools topaz currentTopazPackageName: nil ].
	self assert: method rowanPackageName equals: class rowanPackageName.
	self assert: (audit := loadedProject audit) isEmpty.
	loadedProject unload.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue830_2b [
	"Test coverage for the method 
		Behavior >> _rwCompileMethodForConditionalPackaging:symbolList:category:environmentId:ifUnpackagedDo:"

	"https://github.com/GemTalk/Rowan/issues/830"

	| loadSpec projectName loadedProject packageName1 packageName2 class method audit |
	loadSpec := self _loadSpecNamed: 'spec_0082'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"setup"
	packageName1 := projectName, '-Core'.
	packageName2 := projectName, '-Extensions'.

"case2b: protocol begins with a '*' and DOES NOT map to an existing package and  
		package of class in package with Rowan package convention. Ignore 
		currentTopazPackageName."
	loadedProject := loadSpec resolve load first.
	class := Rowan globalNamed: projectName, 'Class1'.
	self assert: loadedProject packageConvention equals: 'RowanHybrid'.
	self assert: class rowanPackageName equals: packageName1.
	[
		| hitExpectedError |
		hitExpectedError := false.
		Rowan gemstoneTools topaz currentTopazPackageName: packageName2.
		[
			method := class
				compileMethod: 'issue830'
				dictionaries: Rowan image symbolList
				category: '*not-a-package-name'
				environmentId: 0 ] 
			on: Error 
			do: [:ex | 
				self assert: ex description equals: 'UserDefinedError: The method category ''*not-a-package-name'' does not map to a known package and you may not add an unpackaged method to a class that is packaged (RowanSample9-Core),'.
				hitExpectedError := true .
				nil].
		self assert: hitExpectedError description: 'Expected to hit an error' ] ensure: [ Rowan gemstoneTools topaz currentTopazPackageName: nil ].
	self assert: (audit := loadedProject audit) isEmpty.
	loadedProject unload.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue830_3a [
	"Test coverage for the method 
		Behavior >> _rwCompileMethodForConditionalPackaging:symbolList:category:environmentId:ifUnpackagedDo:"

	"https://github.com/GemTalk/Rowan/issues/830"

	| loadSpec projectName loadedProject packageName1 packageName2 class method audit |
	loadSpec := self _loadSpecNamed: 'spec_0057'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"setup"
	packageName1 := projectName, '-Core'.
	packageName2 := projectName, '-Extensions'.

"case3a: existing method is in a package. Method source changed and package preserved."
	loadedProject := loadSpec resolve load first.
	class := Rowan globalNamed: projectName, 'Class1'.
	self assert: loadedProject packageConvention equals: 'Rowan'.
	self assert: (class class compiledMethodAt: #bar) rowanPackageName equals: packageName2.
	self assert: (class class categoryOfSelector: #bar) equals: #accessing.
		method := class class
			compileMethod: 'bar ^ ''barf'''
			dictionaries: Rowan image symbolList
			category: 'butter'
			environmentId: 0.
	self assert: method rowanPackageName equals: packageName2.
	self assert: method sourceString equals:  'bar ^ ''barf'''.
	self assert: (class class categoryOfSelector: #bar) equals: #butter.
	self assert: (audit := loadedProject audit) isEmpty.
	loadedProject unload.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue830_3b [
	"Test coverage for the method 
		Behavior >> _rwCompileMethodForConditionalPackaging:symbolList:category:environmentId:ifUnpackagedDo:"

	"victim of https://github.com/GemTalk/Rowan/issues/557"

	"https://github.com/GemTalk/Rowan/issues/830"

	| loadSpec projectName loadedProject packageName1 packageName2 class method audit|
	loadSpec := self _loadSpecNamed: 'spec_0057'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"setup"
	packageName1 := projectName, '-Core'.
	packageName2 := projectName, '-Extensions'.

"case3b: existing method is NOT in a package. Method source changed and moved to package."
	[
		Rowan image newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.
		class := Object
			subclass: projectName, 'ClassXXX'
			instVarNames: #()
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			inDictionary: (Rowan globalNamed: self _sampleSymbolDictionaryName1)
			options: #().
		self assert: Rowan gemstoneTools topaz currentTopazPackageName equals: nil.
		self assert: class rowanPackageName equals: Rowan unpackagedName.
		method := class
			compileMethod: 'issue830'
			dictionaries: Rowan image symbolList
			category: 'accessing'
			environmentId: 0.
		self assert: method rowanPackageName equals: Rowan unpackagedName.

		loadedProject := loadSpec resolve load first.

		Rowan gemstoneTools topaz currentTopazPackageName: packageName2.
		method := class
			compileMethod: 'issue830 "hello"'
			dictionaries: Rowan image symbolList
			category: 'accessing'
			environmentId: 0.
		self assert: method rowanPackageName equals: packageName2.

		self assert: (audit := loadedProject audit) isEmpty.
	] ensure: [ 
		class ifNotNil: [ 
			(Rowan globalNamed: self _sampleSymbolDictionaryName1) removeKey: class name.
			Rowan gemstoneTools topaz currentTopazPackageName: nil.
			loadedProject unload ] ].

]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue830_4a [
	"Test coverage for the method 
		Behavior >> _rwCompileMethodForConditionalPackaging:symbolList:category:environmentId:ifUnpackagedDo:"

	"https://github.com/GemTalk/Rowan/issues/830"

	| projectName packageName1 packageName2 class method |

	projectName := 'RowanSample9'.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"setup"
	packageName1 := projectName, '-Core'.
	packageName2 := projectName, '-Extensions'.

"case4a: protocol DOES NOT begin with a '*'; currentTopazPackageName not set; 
		the class is not packaged, the method is not packaged"
	[
		Rowan image newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.
		class := Object
			subclass: projectName, 'ClassXXX'
			instVarNames: #()
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			inDictionary: (Rowan globalNamed: self _sampleSymbolDictionaryName1)
			options: #().
		self assert: Rowan gemstoneTools topaz currentTopazPackageName equals: nil.
		self assert: class rowanPackageName equals: Rowan unpackagedName.
		method := class
			compileMethod: 'issue830'
			dictionaries: Rowan image symbolList
			category: 'accessing'
			environmentId: 0.
		self assert: method rowanPackageName equals: class rowanPackageName.
	] ensure: [ 
		class ifNotNil: [ 
			(Rowan globalNamed: self _sampleSymbolDictionaryName1) removeKey: class name ] ].
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue830_4b [
	"Test coverage for the method 
		Behavior >> _rwCompileMethodForConditionalPackaging:symbolList:category:environmentId:ifUnpackagedDo:"

	"victim of https://github.com/GemTalk/Rowan/issues/557"

	"https://github.com/GemTalk/Rowan/issues/830"

	| loadSpec projectName loadedProject packageName1 packageName2 class method audit |
	loadSpec := self _loadSpecNamed: 'spec_0082'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"setup"
	packageName1 := projectName, '-Core'.
	packageName2 := projectName, '-Extensions'.

"case4b: protocol DOES NOT begin with a '*'; currentTopazPackageName not set; 
		the method is added to package of class"
	loadedProject := loadSpec resolve load first.
	class := Rowan globalNamed: projectName, 'Class1'.
	self assert: Rowan gemstoneTools topaz currentTopazPackageName equals: nil.
	self assert: loadedProject packageConvention equals: 'RowanHybrid'.
	self assert: class rowanPackageName equals: packageName1.
	method := class
		compileMethod: 'issue830'
		dictionaries: Rowan image symbolList
		category: 'accessing'
		environmentId: 0.
	self assert: method rowanPackageName equals: class rowanPackageName.
	self assert: (audit := loadedProject audit) isEmpty.
	loadedProject unload.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue830_4c [
	"Test coverage for the method 
		Behavior >> _rwCompileMethodForConditionalPackaging:symbolList:category:environmentId:ifUnpackagedDo:"

	"https://github.com/GemTalk/Rowan/issues/830"

	| projectName packageName1 packageName2 class method |

	projectName := 'RowanSample9'.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"setup"
	packageName1 := projectName, '-Core'.
	packageName2 := projectName, '-Extensions'.

"case4c: protocol DOES NOT begin with a '*'; currentTopazPackageName not set; 
		the class is not packaged, the method is not packaged"
	[
		Rowan image newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.
		class := Object
			subclass: projectName, 'ClassXXX'
			instVarNames: #()
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			inDictionary: (Rowan globalNamed: self _sampleSymbolDictionaryName1)
			options: #().
		self assert: Rowan gemstoneTools topaz currentTopazPackageName equals: nil.
		self assert: class rowanPackageName equals: Rowan unpackagedName.
		method := class
			compileMethod: 'issue830'
			dictionaries: Rowan image symbolList
			category: 'accessing'
			environmentId: 0.
		self assert: method rowanPackageName equals: class rowanPackageName.
		method := class
			compileMethod: 'issue830 "hello"'
			dictionaries: Rowan image symbolList
			category: '*accessing'
			environmentId: 0.
		self assert: method rowanPackageName equals: Rowan unpackagedName.
		self assert: method sourceString equals:   'issue830 "hello"'.
		self assert: (class categoryOfSelector: #issue830) equals: #'*accessing'.
	] ensure: [ 
		class ifNotNil: [ 
			(Rowan globalNamed: self _sampleSymbolDictionaryName1) removeKey: class name ] ].
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue830_4d [
	"Test coverage for the method 
		Behavior >> _rwCompileMethodForConditionalPackaging:symbolList:category:environmentId:ifUnpackagedDo:"

	"https://github.com/GemTalk/Rowan/issues/830"

	| projectName packageName1 packageName2 class method |

	projectName := 'RowanSample9'.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"setup"
	packageName1 := projectName, '-Core'.
	packageName2 := projectName, '-Extensions'.

"case4d: protocol begins with a '*'; currentTopazPackageName not set; 
		the class is not packaged, the new method is not packaged"
	[
		Rowan image newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.
		class := Object
			subclass: projectName, 'ClassXXX'
			instVarNames: #()
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			inDictionary: (Rowan globalNamed: self _sampleSymbolDictionaryName1)
			options: #().
		self assert: Rowan gemstoneTools topaz currentTopazPackageName equals: nil.
		self assert: class rowanPackageName equals: Rowan unpackagedName.
		method := class
			compileMethod: 'issue830'
			dictionaries: Rowan image symbolList
			category: '*accessing'
			environmentId: 0.
		self assert: method rowanPackageName equals: class rowanPackageName.
	] ensure: [ 
		class ifNotNil: [ 
			(Rowan globalNamed: self _sampleSymbolDictionaryName1) removeKey: class name ] ].
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue830_5a [
	"Test coverage for the method 
		Behavior >> _rwCompileMethodForConditionalPackaging:symbolList:category:environmentId:ifUnpackagedDo:"

	"victim of https://github.com/GemTalk/Rowan/issues/557"

	"https://github.com/GemTalk/Rowan/issues/830"

	| loadSpec projectName loadedProject packageName1 packageName2 class method audit |
	loadSpec := self _loadSpecNamed: 'spec_0082'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"setup"
	packageName1 := projectName, '-Core'.
	packageName2 := projectName, '-Extensions'.

"case5a: protocol DOES NOT begin with a '*'; currentTopazPackageName IS set and the 
		package convention is NOT Rowan"
	[
		Rowan image newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.
		class := Object
			subclass: projectName, 'ClassXXX'
			instVarNames: #()
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			inDictionary: (Rowan globalNamed: self _sampleSymbolDictionaryName1)
			options: #().
		loadedProject := loadSpec resolve load first.
		self assert: loadedProject packageConvention ~= 'Rowan'.
		Rowan gemstoneTools topaz currentTopazPackageName: packageName2.
		self assert: class rowanPackageName equals: Rowan unpackagedName.
		method := class
			compileMethod: 'issue830'
			dictionaries: Rowan image symbolList
			category: 'accessing'
			environmentId: 0.
		self assert: method rowanPackageName equals: class rowanPackageName.
		self assert: (audit := loadedProject audit) isEmpty.
		loadedProject unload.
	] ensure: [ 
		Rowan gemstoneTools topaz currentTopazPackageName: nil.
		class ifNotNil: [ 
			(Rowan globalNamed: self _sampleSymbolDictionaryName1) removeKey: class name ] ].
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue830_5b [
	"Test coverage for the method 
		Behavior >> _rwCompileMethodForConditionalPackaging:symbolList:category:environmentId:ifUnpackagedDo:"

	"victim of https://github.com/GemTalk/Rowan/issues/557"

	"https://github.com/GemTalk/Rowan/issues/830"

	| loadSpec projectName loadedProject packageName1 packageName2 class method audit |
	loadSpec := self _loadSpecNamed: 'spec_0057'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"setup"
	packageName1 := projectName, '-Core'.
	packageName2 := projectName, '-Extensions'.

"case5b: protocol DOES NOT begin with a '*'; currentTopazPackageName IS set and the 
		package convention is Rowan"
	[
		Rowan image newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.
		class := Object
			subclass: projectName, 'ClassXXX'
			instVarNames: #()
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			inDictionary: (Rowan globalNamed: self _sampleSymbolDictionaryName1)
			options: #().
		loadedProject := loadSpec resolve load first.
		self assert: loadedProject packageConvention = 'Rowan'.
		Rowan gemstoneTools topaz currentTopazPackageName: packageName2.
		self assert: class rowanPackageName equals: Rowan unpackagedName.
		method := class
			compileMethod: 'issue830'
			dictionaries: Rowan image symbolList
			category: 'accessing'
			environmentId: 0.
		self assert: method rowanPackageName equals: Rowan gemstoneTools topaz currentTopazPackageName.
		self assert: (audit := loadedProject audit) isEmpty.
		loadedProject unload.
	] ensure: [ 
		class ifNotNil: [ 
			(Rowan globalNamed: self _sampleSymbolDictionaryName1) removeKey: class name ] ].
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue830_5c [
	"Test coverage for the method 
		Behavior >> _rwCompileMethodForConditionalPackaging:symbolList:category:environmentId:ifUnpackagedDo:"

	"https://github.com/GemTalk/Rowan/issues/830"

	| loadSpec projectName loadedProject packageName1 packageName2 class method audit |
	loadSpec := self _loadSpecNamed: 'spec_0082'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"setup"
	packageName1 := projectName, '-Core'.
	packageName2 := projectName, '-Extensions'.

"case5c: protocol DOES NOT begin with a '*'; currentTopazPackageName IS set and the 
		package convention is NOT Rowan, BUT the packageName matches the packageName 
		of the class"
	[
		Rowan image newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.
		loadedProject := loadSpec resolve load first.
		self assert: loadedProject packageConvention ~= 'Rowan'.
		class := Object
			rwSubclass: projectName, 'ClassXXX'
			instVarNames: #()
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			category: packageName1
			options: #().
		Rowan gemstoneTools topaz currentTopazPackageName: packageName1.
		self assert: class rowanPackageName equals: packageName1.
		method := class
			compileMethod: 'issue830'
			dictionaries: Rowan image symbolList
			category: 'accessing'
			environmentId: 0.
		self assert: method rowanPackageName equals: class rowanPackageName.
		self assert: (audit := loadedProject audit) isEmpty.
		loadedProject unload.
	] ensure: [ 
		Rowan gemstoneTools topaz currentTopazPackageName: nil ].
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
	true
		ifTrue: [
			"bitbucket access started hanging (from web browser and command line) today (Sun 25 Sep 2022 12:16:22 PM PDT)"
			^ self ].
	loadSpec := self 
		_migrationLoadSpecNamed:  'spec_0034' 
		projectName: 'RowanSample9' 
		projectAlias: 'RowanSample9_No_Migration' 
		gitUrl: 'https://git@bitbucket.org/dalehenrich/rowansample9'.

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
		gitUrl: 'https://git@github.com/dalehenrich/RowanSample9'.

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
RwRowanSample9Test >> testSpec_0002 [
	"spec_0002 should load cleanly"


	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0002'.

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
RwRowanSample9Test >> testSpec_0002_to_0003 [
	"https://github.com/dalehenrich/Rowan/issues/230"

	"Passing variation"

	| loadSpec projectName resolvedProject loadedProjects project 
		packageName1 packageName2 |
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
	self _runTestSuiteForProject: project.

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
	self _runTestSuiteForProject: project.
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

{ #category : 'issue 554' }
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

{ #category : 'issue 554' }
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
RwRowanSample9Test >> testSpec_0038 [
	"spec_0038 should load cleanly"

	"equivalent to RowanSample4 issue_185_1"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0038'.

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
RwRowanSample9Test >> testSpec_0039 [
	"spec_0039 should load cleanly"

	"equivalent to RowanSample4 issue_185_7"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0039'.

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
RwRowanSample9Test >> testSpec_0040 [
	"spec_0040 should load cleanly"

	"equivalent to RowanSample4 issue_185_5"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0040'.

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
RwRowanSample9Test >> testSpec_0041 [
	"spec_0041 should load cleanly"

	"equivalent to RowanSample4 issue_185_2"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0041'.

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
RwRowanSample9Test >> testSpec_0042 [
	"spec_0042 should load cleanly"

	"equivalent to RowanSample4 issue_185_8"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0042'.

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
RwRowanSample9Test >> testSpec_0043 [
	"spec_0043 should load cleanly"

	"equivalent to RowanSample4 issue_185_6"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0043'.

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
RwRowanSample9Test >> testSpec_0044 [
	"spec_0044 should load cleanly"

	"equivalent to RowanSample4 issue_185_4"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0044'.

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
RwRowanSample9Test >> testSpec_0045 [
	"spec_0045 should load cleanly"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0045'.

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
RwRowanSample9Test >> testSpec_0046 [
	"spec_0046 should load cleanly"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0046'.

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
RwRowanSample9Test >> testSpec_0047 [
	"spec_0047 should load cleanly"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0047'.

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
RwRowanSample9Test >> testSpec_0048 [
	"spec_0048 ensure that the custom attribute renamed contols the right set of packages"

	| loadSpec projectName projectNames resolvedProject loadedProjects packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 packageName7 packageName8 x y className6 |
	loadSpec := self _loadSpecNamed: 'spec_0048'.

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

	packageName1 := projectName , '-' , 'Core', '_295'.
	packageName2 := projectName , '-' , 'Extensions', '_295'.
	packageName3 := projectName , '-' , 'GemStone', '_295'.
	packageName4 := projectName , '-' , 'GemStone-Tests', '_295'.
	packageName5 := projectName , '-' , 'Tests', '_295'.
	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.
	packageName8 := projectName , '-' , 'Tests-RenamedPackage'.
	className6 := 'New', projectName , 'Subclass'.

	self assert: (x := resolvedProject packageNames asArray sort) = (y := { packageName1 . packageName2 . packageName3 . packageName4 . packageName5 . packageName7 . packageName8 } sort).
	self assert: (x := Rowan globalNamed: className6) notNil.

	self deny: (resolvedProject packageNamed: packageName7) isEmpty.
	self deny: (resolvedProject packageNamed: packageName8) isEmpty.

"remove the renamed attribute"
	loadSpec customConditionalAttributes: { 'tests' }.

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (x := resolvedProject packageNames asArray sort) = (y := { packageName1 . packageName2 . packageName3 . packageName4 . packageName5  } sort).
	self deny: (x := Rowan globalNamed: className6) notNil.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0049 [
	"spec_0049 should load cleanly ... both specs"

	| loadSpec projectName projectNames resolvedProject loadedProjects fileUrl packageName1 packageName2 packageName3 packageName4 packageName5 packageName6 packageName7 packageName8 className6 x y project |
	loadSpec := self _loadSpecNamed: 'spec_0049'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: project currentBranchName =  'spec_0049'.

	packageName1 := projectName , '-' , 'Core', '_295'.
	packageName2 := projectName , '-' , 'Extensions', '_295'.
	packageName3 := projectName , '-' , 'GemStone', '_295'.
	packageName4 := projectName , '-' , 'GemStone-Tests', '_295'.
	packageName5 := projectName , '-' , 'Tests', '_295'.
	packageName6 := projectName , '-' , 'NewPackage'.
	packageName7 := projectName , '-' , 'RenamedPackage'.
	packageName8 := projectName , '-' , 'Tests-RenamedPackage'.
	className6 := 'New', projectName , 'Subclass'.

	self assert: (x := resolvedProject packageNames asArray sort) = (y := { packageName1 . packageName2 . packageName3 . packageName4 . packageName5 . packageName7 . packageName8 } sort).
	self assert: (x := Rowan globalNamed: className6) notNil.

	self deny: (resolvedProject packageNamed: packageName7) isEmpty.
	self deny: (resolvedProject packageNamed: packageName8) isEmpty.

"create load spec from Issue_304.ston"
	fileUrl := 'file:' , (resolvedProject specsRoot / 'Issue_304', 'ston') pathString.
	loadSpec := (RwSpecification fromUrl: fileUrl)
		projectsHome: self _testRowanProjectsSandbox;
		yourself.

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: project currentBranchName =  'spec_0049'.

	self assert: (x := resolvedProject packageNames asArray sort) = (y := { packageName1 . packageName2 . packageName3 . packageName4 . packageName5  } sort).
	self deny: (x := Rowan globalNamed: className6) notNil.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0050 [
	"spec_0050 should load cleanly"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0050'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0051 [
	"spec_0051 should load cleanly"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0051'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0052_1 [
	"spec_0052 should load cleanly ... both projects as of https://github.com/GemTalk/Rowan/issues/668"

	"https://github.com/GemTalk/Rowan/issues/571"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0052'.

	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample9_embedded_1'.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0052_2 [
	"spec_0052 should load cleanly ... using resolveProjectSet: message"

	"https://github.com/GemTalk/Rowan/issues/571"

	| loadSpec projectName projectNames projectSet loadedProjects requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0052'.

	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample9_embedded_1'.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	projectSet := loadSpec resolveProjectSetStrict.

"load project set"
	loadedProjects := Rowan projectTools loadV2 loadProjectSetDefinition: projectSet.

"validate"
	self
		_standard_validate: (projectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"unload projects"
	(Rowan projectNamed: projectName) unload.
	(Rowan projectNamed: requiredProjectName) unload.	"unload second, because it is required by project name"

"resolve project using resolveProjectSet:"
	projectSet := loadSpec resolveProjectSet: (projectSet projectNamed: projectName) platformConditionalAttributes.

"load project set"
	loadedProjects := Rowan projectTools loadV2 loadProjectSetDefinition: projectSet.

"validate"
	self
		_standard_validate: (projectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0052_3 [
	"spec_0052 should load cleanly ... testing unload of a project that is required by another project"

	"https://github.com/GemTalk/Rowan/issues/571"

	| loadSpec projectName projectNames projectSet loadedProjects requiredProjectName errorHit |
	loadSpec := self _loadSpecNamed: 'spec_0052'.

	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample9_embedded_1'.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	projectSet := loadSpec resolveProjectSetStrict.

"load project set"
	loadedProjects := Rowan projectTools loadV2 loadProjectSetDefinition: projectSet.

"validate"
	self
		_standard_validate: (projectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"should get an error if attempting to unload a project that is required by another project"
	errorHit := false.
	[ (Rowan projectNamed: requiredProjectName) unload ]
		on: Error do: [:ex |
			self assert: ex description = ''.
			errorHit := true ].
	self assert: errorHit.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0053 [
	"spec_0053 should load cleanly"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0053'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProjectSet := loadSpec resolveProjectSet.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0054_1 [
	"spec_0054 should load cleanly ... without the new gnu classes and tests"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0054'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProjectSet := loadSpec resolveProjectSet.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self deny: (Rowan globalNamed: 'RowanSample9GnuClass1')  notNil.
	self deny: (Rowan globalNamed: 'RowanSample9_embedded_1GnuClass1')  notNil.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0054_2 [
	"spec_0054 should load cleanly ... with the new gnu classes and tests"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0054'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	loadSpec customConditionalAttributes: (loadSpec customConditionalAttributes, #('gnu')).
	resolvedProjectSet := loadSpec resolveProjectSet.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: 'RowanSample9GnuClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GnuClass1')  notNil.

]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0054_3 [
	"spec_0054 should load cleanly ... with the new gnu classes and tests"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0054'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProjectSet := loadSpec resolveProjectSet: #('common' 'gnu' 'tests').

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: 'RowanSample9GnuClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GnuClass1')  notNil.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0055_1 [
	"spec_0055 should load cleanly ... without the new GemStone24x classes and tests"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0055'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProjectSet := loadSpec resolveProjectSet.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self deny: (Rowan globalNamed: 'RowanSample9GemStone24xClass1')  notNil.
	self deny: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone24xClass1')  notNil.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0055_2 [
	"spec_0055 should load cleanly ... with the new GemStone24x classes and tests"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0055'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	loadSpec customConditionalAttributes: ({ '2.4.0' asRwGemStoneVersionNumber }, loadSpec customConditionalAttributes).
	resolvedProjectSet := loadSpec resolveProjectSet.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: 'RowanSample9GemStone24xClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone24xClass1')  notNil.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0055_3 [
	"spec_0055 should load cleanly ... with the new GemStone24x classes and tests"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0055'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProjectSet := loadSpec resolveProjectSet: {'common' . '2.4.0' asRwGemStoneVersionNumber . 'tests'}.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: 'RowanSample9GemStone24xClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone24xClass1')  notNil.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0056_1 [
	"spec_0055 should load cleanly ... without the new GemStone24x classes and tests ... with the new GemStone3x classes and tests - condition spans all supported versions of GemStone"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0056'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProjectSet := loadSpec resolveProjectSet.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: 'RowanSample9GemStone3xClass1')  notNil.
	self deny: (Rowan globalNamed: 'RowanSample9GemStone24xClass1')  notNil.

	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone3xClass1')  notNil.
	self deny: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone24xClass1')  notNil.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0056_2 [
	"spec_0055 should load cleanly ... with the new GemStone24x classes and tests ... with the new GemStone3x classes and tests - condition spans all supported versions of GemStone"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0056'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	loadSpec customConditionalAttributes: ({ '2.4.0' asRwGemStoneVersionNumber }, loadSpec customConditionalAttributes).
	resolvedProjectSet := loadSpec resolveProjectSet.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: 'RowanSample9GemStone3xClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9GemStone24xClass1')  notNil.

	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone3xClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone24xClass1')  notNil.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0056_3 [
	"spec_0055 should load cleanly ... with the new GemStone24x classes and tests ... without the new GemStone3x classes and tests - current platform attributes are not included in the load"

	"This is the closest test case to the reported bug for Issue #594, however the actual bug is not reproduced"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0056'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProjectSet := loadSpec resolveProjectSet: {'common' . '2.4.0' asRwGemStoneVersionNumber . 'tests'}.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self deny: (Rowan globalNamed: 'RowanSample9GemStone3xClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9GemStone24xClass1')  notNil.

	self deny: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone3xClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone24xClass1')  notNil.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0057 [
	"spec_0057 should load cleanly"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0057'.

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
RwRowanSample9Test >> testSpec_0058 [
	"spec_0058 should load cleanly"

	"https://github.com/GemTalk/Rowan/issues/169"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0058'.

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
RwRowanSample9Test >> testSpec_0059_1 [
	"spec_0059 should load cleanly for all 4 attributes combos common, alt1, alt2, alt3"

	"https://github.com/GemTalk/Rowan/issues/660"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0059'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"load project using each of the alt conditions"
	{
		#('common' 'tests').
		#('common' 'tests' 'alt1').
		#('common' 'tests' 'alt2').
		#('common' 'tests' 'alt3').
	} do: [:conditionalAttributes |
		resolvedProject := loadSpec resolve: conditionalAttributes.
		loadedProjects := resolvedProject load.
		self
			_standard_validate: resolvedProject
			loadedProjects: loadedProjects
			expectedProjectNames: projectNames.
		conditionalAttributes size > 2 
			ifTrue: [
				"sanity check in case of massive load failure"
				self assert: ((Rowan globalNamed: 'RowanSample9Class1') selectors includes: (conditionalAttributes at: 3) asSymbol) ] ].
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0059_2 [
	"spec_0059 should load cleanly for all 3 platform combos (platformA, platformB, platformC) and attributes combos common, alt1"

	"https://github.com/GemTalk/Rowan/issues/660"

	| loadSpec projectName projectNames resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0059'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"load project using each of the alt conditions"
	{
		#('common' 'tests' 'alt1' 'platformA').
		#('common' 'tests' 'alt1' 'platformB').
		#('common' 'tests' 'alt1' 'platformC').
	} do: [:conditionalAttributes |
		resolvedProject := loadSpec resolve: conditionalAttributes.
		loadedProjects := resolvedProject load.
		self
			_standard_validate: resolvedProject
			loadedProjects: loadedProjects
			expectedProjectNames: projectNames.
		(conditionalAttributes at: 4) = 'platformC'
			ifTrue: [
				"sanity check in case of massive load failure"
				self assert: ((Rowan globalNamed: 'RowanSample9TestCase') selectors includes: #testPlatformC) ]
			ifFalse: [
				"sanity check in case of massive load failure"
				self assert: ((Rowan globalNamed: 'RowanSample9Class1') selectors includes: (conditionalAttributes at: 4) asSymbol) ]].
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0060_1 [
	"spec_0060 should load cleanly, including RowanSample10"

	"https://github.com/GemTalk/Rowan/issues/668"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0060'.

	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample10'.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	(self _testRowanProjectsSandbox / requiredProjectName) ensureDeleteAll.

"resolve project"
	resolvedProject := loadSpec resolve.

[ 
self _adjustRequiredProjectsForIssue805: resolvedProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
] ensure: [ 
	"Don't leave a dirty git checkout lying around"
	(self _testRowanProjectsSandbox / projectName) ensureDeleteAll.
]
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0060_2 [
	"load spec_0001 then load (reload) spec_0060, RowanProject10 should load cleanly"

	"https://github.com/GemTalk/Rowan/issues/668"

	| loadSpec projectName projectNames_1 projectNames_2 resolvedProject 
		loadedProjects project requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0001'.

	projectName := loadSpec projectName.
	projectNames_1 := {projectName }.
	requiredProjectName := 'RowanSample10'.
	projectNames_2 := {projectName . requiredProjectName}.

	projectNames_2 do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	(self _testRowanProjectsSandbox / requiredProjectName) ensureDeleteAll.

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames_1.

"checkout spec_0060 and reload"
	project checkout: 'spec_0060'.

[
self _adjustRequiredProjectsForIssue805: resolvedProject.

"reload project"
	loadedProjects := project reload. "can afford to take required projects into account"

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames_2.

] ensure: [ 
	"Don't leave a dirty git checkout lying around"
	(self _testRowanProjectsSandbox / projectName) ensureDeleteAll.
]
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0064_1 [
	"spec_0064 should load cleanly, including RowanSample10"

	"https://github.com/GemTalk/Rowan/issues/680"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName className1 |
	loadSpec := self _loadSpecNamed: 'spec_0064'.

	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample10'.
	projectNames := {projectName}.
	className1 := requiredProjectName , 'Class1'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	(self _testRowanProjectsSandbox / requiredProjectName) ensureDeleteAll.

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: className1) rowanProjectName equals: projectName
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0064_2 [
	"Reproduce #680 ... load spec_0064, then load spec_0060 and the packages should be cleanly moved/adopted by RowanSample10 project."

	"https://github.com/GemTalk/Rowan/issues/680"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName className1 project |
	loadSpec := self _loadSpecNamed: 'spec_0064'.

	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample10'.
	projectNames := {projectName}.
	className1 := requiredProjectName , 'Class1'.

	{projectName . requiredProjectName} do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	(self _testRowanProjectsSandbox / requiredProjectName) ensureDeleteAll.

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: className1) rowanProjectName equals: projectName.

"reproduce #680 - load spec_0060 which has an external project with some duplicate packages names in spec_0064"
	loadSpec := self _loadSpecNamed: 'spec_0060'.

"resolve project"
	resolvedProject := loadSpec resolve.

[
self _adjustRequiredProjectsForIssue805: resolvedProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: {projectName . requiredProjectName}.

	self assert: (Rowan globalNamed: className1) rowanProjectName equals: requiredProjectName.

	project := Rowan projectNamed: requiredProjectName.
	self assert: project packageNames asArray sort equals: #('RowanSample10-Core' 'RowanSample10-Tests' ) sort
] ensure: [ 
	"Don't leave a dirty git checkout lying around"
	(self _testRowanProjectsSandbox / projectName) ensureDeleteAll.
]
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0064_3 [
	"Extended test case for #680 with changed classes, methods and symbol dictionary ... load spec_0064, 
		make modifications to RowanSample10Class1: add iv, add class method, change existing method;
		change symbol dictionary, then load spec_0060 and the packages should be cleanly moved/adopted 
		by RowanSample10 project."

	"https://github.com/GemTalk/Rowan/issues/680"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName className1 project packageName |
	loadSpec := self _loadSpecNamed: 'spec_0064'.

	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample10'.
	projectNames := {projectName}.
	className1 := requiredProjectName , 'Class1'.

	{projectName . requiredProjectName} do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	(self _testRowanProjectsSandbox / requiredProjectName) ensureDeleteAll.

"change default symbol dictionary"
	loadSpec gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2.

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.
	project := (loadedProjects at: 1) defined.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: className1) rowanProjectName equals: projectName.

"make changes to classes and methods"
	packageName := requiredProjectName, '-Core'.
	((project packageNamed: packageName)
		classDefinitionNamed: className1)
			instVarNames: #( 'ivar1' 'ivar2');
		addClassMethod: 'bar ^''var''' protocol: 'accessing';
		updateInstanceMethod: 'foo "added a comment" ^ 1' protocol: 'new';
		yourself.

"load project"
	loadedProjects := project load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: {projectName}.

	self assert: (Rowan globalNamed: className1) instVarNames asArray sort 
		equals: #( #'ivar1' #'ivar2' ) sort.
	self assert: ((Rowan globalNamed: className1) compiledMethodAt: #foo) sourceString
		equals: 'foo "added a comment" ^ 1'.
	self assert: ((Rowan globalNamed: className1) categoryOfSelector: #foo)
		equals: #'new'.
	self assert: ((Rowan globalNamed: className1) class compiledMethodAt: #bar) sourceString
		equals: 'bar ^''var'''.

"reproduce #680 - load spec_0060 which has an external project with some duplicate packages names in spec_0064"
	loadSpec := self _loadSpecNamed: 'spec_0060'.

"resolve project"
	resolvedProject := loadSpec resolve.

[
self _adjustRequiredProjectsForIssue805: resolvedProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: {projectName . requiredProjectName}.

	self assert: (Rowan globalNamed: className1) rowanProjectName equals: requiredProjectName.

	project := Rowan projectNamed: requiredProjectName.
	self assert: project packageNames asArray sort equals: #('RowanSample10-Core' 'RowanSample10-Tests' ) sort.
	self assert: ((Rowan globalNamed: className1) compiledMethodAt: #foo) sourceString
		equals: 'foo ^1'.
	self assert: ((Rowan globalNamed: className1) categoryOfSelector: #foo)
		equals: #'accessing'.
	self assert: ((Rowan globalNamed: className1) class compiledMethodAt: #bar otherwise: nil) isNil.

] ensure: [ 
	"Don't leave a dirty git checkout lying around"
	(self _testRowanProjectsSandbox / projectName) ensureDeleteAll.
]
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0064_4 [
	"Extended test case for #680 ... basically testSpec_0064_3 with NO new class version"

	"https://github.com/GemTalk/Rowan/issues/680"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName className1 project packageName |
	loadSpec := self _loadSpecNamed: 'spec_0064'.

	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample10'.
	projectNames := {projectName}.
	className1 := requiredProjectName , 'Class1'.

	{projectName . requiredProjectName} do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	(self _testRowanProjectsSandbox / requiredProjectName) ensureDeleteAll.

"change default symbol dictionary"
	loadSpec gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2.

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.
	project := (loadedProjects at: 1) defined.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: className1) rowanProjectName equals: projectName.

"make changes to classes and methods"
	packageName := requiredProjectName, '-Core'.
	((project packageNamed: packageName)
		classDefinitionNamed: className1)
		addClassMethod: 'bar ^''var''' protocol: 'accessing';
		updateInstanceMethod: 'foo "added a comment" ^ 1' protocol: 'new';
		yourself.

"load project"
	loadedProjects := project load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: {projectName}.

	self assert: ((Rowan globalNamed: className1) compiledMethodAt: #foo) sourceString
		equals: 'foo "added a comment" ^ 1'.
	self assert: ((Rowan globalNamed: className1) categoryOfSelector: #foo)
		equals: #'new'.
	self assert: ((Rowan globalNamed: className1) class compiledMethodAt: #bar) sourceString
		equals: 'bar ^''var'''.

"reproduce #680 - load spec_0060 which has an external project with some duplicate packages names in spec_0064"
	loadSpec := self _loadSpecNamed: 'spec_0060'.

"resolve project"
	resolvedProject := loadSpec resolve.

[
self _adjustRequiredProjectsForIssue805: resolvedProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: {projectName . requiredProjectName}.

	self assert: (Rowan globalNamed: className1) rowanProjectName equals: requiredProjectName.

	project := Rowan projectNamed: requiredProjectName.
	self assert: project packageNames asArray sort equals: #('RowanSample10-Core' 'RowanSample10-Tests' ) sort.
	self assert: ((Rowan globalNamed: className1) compiledMethodAt: #foo) sourceString
		equals: 'foo ^1'.
	self assert: ((Rowan globalNamed: className1) categoryOfSelector: #foo)
		equals: #'accessing'.
	self assert: ((Rowan globalNamed: className1) class compiledMethodAt: #bar otherwise: nil) isNil.

] ensure: [ 
	"Don't leave a dirty git checkout lying around"
	(self _testRowanProjectsSandbox / projectName) ensureDeleteAll.
]
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0064_5 [
	"Extended test case for #680 ... basically testSpec_0064_3 with ONLY new class version (no method changes and no symbol dictionary change)"

	"https://github.com/GemTalk/Rowan/issues/680"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName className1 project packageName |
	loadSpec := self _loadSpecNamed: 'spec_0064'.

	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample10'.
	projectNames := {projectName}.
	className1 := requiredProjectName , 'Class1'.

	{projectName . requiredProjectName} do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	(self _testRowanProjectsSandbox / requiredProjectName) ensureDeleteAll.

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.
	project := (loadedProjects at: 1) defined.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: className1) rowanProjectName equals: projectName.

"make changes to classes and methods"
	packageName := requiredProjectName, '-Core'.
	((project packageNamed: packageName)
		classDefinitionNamed: className1)
			instVarNames: #( 'ivar1' 'ivar2');
		yourself.

"load project"
	loadedProjects := project load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: {projectName}.

	self assert: (Rowan globalNamed: className1) instVarNames asArray sort 
		equals: #( #'ivar1' #'ivar2' ) sort.

"reproduce #680 - load spec_0060 which has an external project with some duplicate packages names in spec_0064"
	loadSpec := self _loadSpecNamed: 'spec_0060'.

"resolve project"
	resolvedProject := loadSpec resolve.

[
self _adjustRequiredProjectsForIssue805: resolvedProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: {projectName . requiredProjectName}.

	self assert: (Rowan globalNamed: className1) rowanProjectName equals: requiredProjectName.

	project := Rowan projectNamed: requiredProjectName.
	self assert: project packageNames asArray sort equals: #('RowanSample10-Core' 'RowanSample10-Tests' ) sort.
	self assert: ((Rowan globalNamed: className1) compiledMethodAt: #foo) sourceString
		equals: 'foo ^1'.
	self assert: ((Rowan globalNamed: className1) categoryOfSelector: #foo)
		equals: #'accessing'.
	self assert: ((Rowan globalNamed: className1) class compiledMethodAt: #bar otherwise: nil) isNil.

] ensure: [ 
	"Don't leave a dirty git checkout lying around"
	(self _testRowanProjectsSandbox / projectName) ensureDeleteAll.
]
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0079 [
	"spec_0079 should load cleanly"


	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0079'.

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
RwRowanSample9Test >> testSpec_0080 [
	"spec_0080 should load cleanly"


	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0080'.

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
RwRowanSample9Test >> testSpec_0081 [
	"spec_0081 should load cleanly"


	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0081'.

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
RwRowanSample9Test >> testSpec_0082 [
	"spec_0082 should load cleanly"


	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0082'.

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

"validate"
	self _standard_validate: resolvedProject1 loadedProjects: loadedProjects.

"load project1 from spec_0003 checkout: changing repository root of loaded project
	results in a load "
	loadedProjects := project1 repositoryRoot: resolvedProject2 repositoryRoot.

"Validate"
	self _standard_validate: resolvedProject1 loadedProjects: loadedProjects.
	self assert: project1 asDefinition repositoryRoot = project1 repositoryRoot.
	self assert: resolvedProject2 repositoryRoot equals: project1 repositoryRoot.
	self deny: resolvedProject2 repositoryRoot = resolvedProject1 repositoryRoot.
	self assert: (Rowan globalNamed: 'RowanSample9Class1') bar = 'bar'.	"bar method not present in spec_0001"
	self assert: resolvedProject2 repositoryRoot = project1 repositoryRoot.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSwitchRepository_591 [
	"https://github.com/GemTalk/Rowan/issues/591"

	"spec_0052 and spec_0053 have embedded projects"

	| projectName1 projectName2 loadSpec1 loadSpec2 projectNames resolvedProjectSet1 
		resolvedProjectSet2 project1 loadedProjects resolvedProject1 resolvedProject2 
		requiredProjectName |
	
	projectName1 := 'RowanSample9'.
	projectName2 :=  'RowanSample9_embedded_1'.

	requiredProjectName := 'RowanSample9_embedded_1'.

	loadSpec1 := (self _loadSpecNamed: 'spec_0052')
		projectAlias: 'RowanSample9_0052';
		yourself.
	loadSpec2 := (self _loadSpecNamed: 'spec_0053')
		projectAlias: 'RowanSample9_0053';
		yourself.

	projectNames := {projectName1 . requiredProjectName }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve projects"
	resolvedProjectSet1 := loadSpec1 resolveProjectSet.
	resolvedProject1 := resolvedProjectSet1 projectNamed: projectName1.

	resolvedProjectSet2 := loadSpec2 resolveProjectSet.	"only used to extract target repositoryRoot ... load of spec_0053 will happen via using spec_0053 repositoryRoot"
	resolvedProject2 := resolvedProjectSet2 projectNamed: projectName2.

"validate"
	self deny: resolvedProject2 repositoryRoot = resolvedProject1 repositoryRoot.

"load project1 from spec_0052"
	loadedProjects := resolvedProjectSet1 load.
	project1 := Rowan projectNamed: projectName1.

"validate"
	self 
		_standard_validate: resolvedProject1 
		loadedProjects: loadedProjects 
		expectedProjectNames: projectNames.

"load project1 from spec_0053 checkout: changing repository root of loaded project
	results in a load "
	loadedProjects := project1 repositoryRoot: resolvedProject2 repositoryRoot.

"Validate"
	self 
		_standard_validate: resolvedProject1 
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self assert: project1 asDefinition repositoryRoot = project1 repositoryRoot.
	self assert: resolvedProject2 repositoryRoot equals: project1 repositoryRoot.
	self deny: resolvedProject2 repositoryRoot = resolvedProject1 repositoryRoot.
	self assert: ((Rowan globalNamed: 'RowanSample9Class1') new perform: #ivar1) isNil.	"ivar1 method not present in spec_0052"
	self assert: resolvedProject2 repositoryRoot = project1 repositoryRoot.
]

{ #category : 'tests' }
RwRowanSample9Test >> testUnmanagedSpec_0057_01 [
	"based on spec_0057 after loading, do a filein (with removeAllMethods) and run unmanaged report and an audit"

	| loadedProjects project audit packageName1 packageName2 className1 className2 reports symbolList |
	packageName1 := 'RowanSample9-Tests'.
	packageName2 := 'RowanSample9-Core'.
	className1 := 'RowanSample9TestCase'.
	className2 := 'RowanSample9Class1'.

	loadedProjects := self 
		_testUnmanagedSpec_0057: true 
		packageName1: packageName1 
		packageName2: packageName2 
		className1: className1 
		className2: className2.

"validate"
	project := loadedProjects at: 1.
	self _runTestSuiteForProject: project.
	audit := project audit.

"run reports"
	reports := {(Rowan gemstoneTools topaz
		unmanagedClassesAndMethodsReportForSymbolDictionaries: Rowan image symbolList).
	(Rowan gemstoneTools topaz
		unmanagedClassesAndMethodsReportForSymbolDictionary:
			(Rowan image
				symbolDictNamed: (project symbolDictNameForPackageNamed: packageName1))).
	(Rowan gemstoneTools topaz
		unmanagedClassesAndMethodsReportForClass: (Rowan globalNamed: className1))}.
	symbolList := GsSession currentSession symbolList.
	reports
		do: [ :details | 
			details
				unmanagedMethodsDo: [ :methodDetails :classDetails | 
					(#(#'GciErrSType' #'GciLibrary') includes: (classDetails at: 'class') name)
						ifFalse: [ 
							| unexpected ar |
							ar := symbolList dictionariesAndSymbolsOf: (classDetails at: 'class').
							ar isEmpty
								ifTrue: [ unexpected := true ]
								ifFalse: [ 
									ar
										do: [ :infoAr | 
											"ignore superDoit classes - need two for short period of time ... the spelling error fixed in latest superDoit version"
											unexpected := (#(SuperDoit_trasientSymbolDictionary SuperDoit_transientSymbolDictionary) includes: (infoAr at: 1) name) not ] ].
							unexpected
								ifTrue: [ 
									self
										assert: false
										description:
											'The class ' , (classDetails at: 'class') name asString
												, ' is unexpectedly unpackaged' ] ] ] ].
	self assert: audit isEmpty description: 'Unexpected audit failure ... need category: intercept'
]

{ #category : 'tests' }
RwRowanSample9Test >> testUnmanagedSpec_0057_01_830 [
	"based on spec_0057 after loading, do a filein (with removeAllMethods) and reloading methods into class 
		from filein -- handled correctly with fix for issue #830"

	"https://github.com/GemTalk/Rowan/issues/830"

	| loadedProjects project audit packageName1 packageName2 classAudit className1 className2 |
	packageName1 := 'RowanSample9-Tests'.
	packageName2 := 'RowanSample9-Core'.
	className1 := 'RowanSample9TestCase'.
	className2 := 'RowanSample9Class1'.

	loadedProjects := self 
		_testNefariousUnmanagedSpec_0057: true 
		packageName1: packageName1 
		packageName2: packageName2 
		className1: className1 
		className2: className2.

"validate"
	project := loadedProjects at: 1.
	self _runTestSuiteForProject: project.
	audit := project audit.

	classAudit := ((audit at: packageName1) at: className1)
		collect: [ :each | each message ].
	self assert: classAudit size = 1.
	self assert: (classAudit includes: 'Missing loaded method: RowanSample9TestCase>>test').

	classAudit := ((audit at: packageName2) at: className2)
		collect: [ :each | each message ].
	self assert: classAudit size = 2.
	self assert: (classAudit includes: 'Missing loaded method: RowanSample9Class1>>foo').
	self assert: (classAudit includes: 'Missing loaded method: RowanSample9Class1 class>>bar').

"run reports"
	{(Rowan gemstoneTools topaz
		unmanagedClassesAndMethodsReportForSymbolDictionaries: Rowan image symbolList).
	(Rowan gemstoneTools topaz
		unmanagedClassesAndMethodsReportForSymbolDictionary:
			(Rowan image
				symbolDictNamed: (project symbolDictNameForPackageNamed: packageName1))).
	(Rowan gemstoneTools topaz
		unmanagedClassesAndMethodsReportForClass: (Rowan globalNamed: className1))}
		do: [ :details | 
			details
				unmanagedMethodsDo: [ :methodDetails :classDetails | 
					(classDetails at: 'class') name asString = className1
						ifTrue: [ self assert: (methodDetails at: 'method') selector == #'test' ].
					(classDetails at: 'class') name asString = className2
						ifTrue: [ 
							self
								assert:
									((methodDetails at: 'method') selector == #'foo'
										or: [ (methodDetails at: 'method') selector == #'bar' ]) ] ] ]
]

{ #category : 'tests' }
RwRowanSample9Test >> testUnmanagedSpec_0057_02 [
	"based on spec_0057 after loading, do a filein (withOUT removeAllMethods) and run unmanaged report and an audit"

	| loadedProjects project audit packageName1 packageName2 className1 className2 reports symbolList |
	packageName1 := 'RowanSample9-Tests'.
	packageName2 := 'RowanSample9-Core'.
	className1 := 'RowanSample9TestCase'.
	className2 := 'RowanSample9Class1'.

	loadedProjects := self
		_testUnmanagedSpec_0057: false
		packageName1: packageName1
		packageName2: packageName2
		className1: className1
		className2: className2.

"validate"
	project := loadedProjects at: 1.
	self _runTestSuiteForProject: project.
	audit := project audit.

"run reports"
	reports := {(Rowan gemstoneTools topaz
		unmanagedClassesAndMethodsReportForSymbolDictionaries: Rowan image symbolList).
	(Rowan gemstoneTools topaz
		unmanagedClassesAndMethodsReportForSymbolDictionary:
			(Rowan image
				symbolDictNamed: (project symbolDictNameForPackageNamed: packageName1))).
	(Rowan gemstoneTools topaz
		unmanagedClassesAndMethodsReportForClass: (Rowan globalNamed: className1))}.
	symbolList := GsSession currentSession symbolList.
	reports
		do: [ :details | 
			details
				unmanagedMethodsDo: [ :methodDetails :classDetails | 
					(#(#'GciErrSType' #'GciLibrary') includes: (classDetails at: 'class') name)
						ifFalse: [ 
							| unexpected ar |
							ar := symbolList dictionariesAndSymbolsOf: (classDetails at: 'class').
							ar isEmpty
								ifTrue: [ unexpected := true ]
								ifFalse: [ 
									ar
										do: [ :infoAr | 
											"ignore superDoit classes - need two for short period of time ... the spelling error fixed in latest superDoit version"
											unexpected := (#(SuperDoit_trasientSymbolDictionary SuperDoit_transientSymbolDictionary) includes: (infoAr at: 1) name) not ] ].
							unexpected
								ifTrue: [ 
									self
										assert: false
										description:
											'The class ' , (classDetails at: 'class') name asString
												, ' is unexpectedly unpackaged' ] ] ] ].
	self assert: audit isEmpty description: 'Unexpected audit failure ... need category: intercept'
]
