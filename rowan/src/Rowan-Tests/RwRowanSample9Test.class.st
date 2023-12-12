Class {
	#name : 'RwRowanSample9Test',
	#superclass : 'RwAbstractRowanSampleTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanSample9Test class >> _rowanSample_0000_load_spec [
	^ (RwSpecification
		fromUrl:
			'https://raw.githubusercontent.com/dalehenrich/' , self _rowanSample9ProjectName
				, '/master/specs/spec_0000.ston')
		projectsHome: self _testRowanProjectsSandbox;
		yourself
]

{ #category : 'Testing' }
RwRowanSample9Test class >> isAbstract [
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #RwRowanSample9Test
]

{ #category : 'audit tests' }
RwRowanSample9Test >> _auditRemoveAndRecompiledMethods_0081: projectName [
	| className packageName1 packageName2 class |
"remove compiled methods"
	className := projectName, 'Class1'.
	packageName1 := projectName, '-Core'.
	packageName2 := projectName, '-Extensions'.
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
RwRowanSample9Test >> _expectedIssue766GsFileContents: projectName [
	^ '! Class Declarations
! Generated file, do not Edit

doit
(IdentityKeyValueDictionary
	subclass: ''', projectName , 'IdentityKeyValueDictionary''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanSample9_1
	options: #( #logCreation )
)
		category: ''', projectName , '-GemStone'';
		comment: ''I am an example class'';
		immediateInvariant.
true.
%

doit
(Object
	subclass: ''', projectName , 'Class1''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanSample9_1
	options: #( #logCreation )
)
		category: ''', projectName , '-Core'';
		comment: ''I am an example class'';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: ''', projectName , 'GemStoneTestCase''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanSample9_1
	options: #( #logCreation )
)
		category: ''', projectName , '-GemStone-Tests'';
		comment: ''I test the GemStone class'';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: ''', projectName , 'TestCase''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanSample9_1
	options: #( #logCreation )
)
		category: ''', projectName , '-Tests'';
		comment: ''I test the Core class'';
		immediateInvariant.
true.
%

! Class implementation for ''', projectName , 'IdentityKeyValueDictionary''

!		Class methods for ''', projectName , 'IdentityKeyValueDictionary''

category: ''accessing''
classmethod: ', projectName , 'IdentityKeyValueDictionary
bar ^''bar''
%

!		Instance methods for ''', projectName , 'IdentityKeyValueDictionary''

category: ''accessing''
method: ', projectName , 'IdentityKeyValueDictionary
foo ^1
%

! Class implementation for ''', projectName , 'Class1''

!		Instance methods for ''', projectName , 'Class1''

category: ''accessing''
method: ', projectName , 'Class1
foo ^1
%

! Class implementation for ''', projectName , 'GemStoneTestCase''

!		Instance methods for ''', projectName , 'GemStoneTestCase''

category: ''tests''
method: ', projectName , 'GemStoneTestCase
test1  self assert: ', projectName , 'IdentityKeyValueDictionary new foo = 1
%

category: ''tests''
method: ', projectName , 'GemStoneTestCase
test2  self assert: ', projectName , 'IdentityKeyValueDictionary bar = ''bar''
%

! Class implementation for ''', projectName , 'TestCase''

!		Instance methods for ''', projectName , 'TestCase''

category: ''tests''
method: ', projectName , 'TestCase
test1  self assert: ', projectName , 'Class1 new foo = 1
%

category: ''tests''
method: ', projectName , 'TestCase
test2  self assert: ', projectName , 'Class1 bar = ''bar''
%

! Class extensions for ''', projectName , 'Class1''

!		Class methods for ''', projectName , 'Class1''

category: ''*', projectName asLowercase , '-extensions''
classmethod: ', projectName , 'Class1
bar ^''bar''
%

'
]

{ #category : 'private' }
RwRowanSample9Test >> _issue_527_resolve_load_validate: projectSpec className: className expectedSymDictName: expectedSymDictName [
	| resolvedProject loadedProjects symDictName symDict |
	resolvedProject := projectSpec resolveProject.
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

	(loadedProjects projectNamed: projectSpec projectName) unload.

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
RwRowanSample9Test >> _processIssue705: resolvedProject componentNamed: componentName previousRequiredProjectNames: loadedComponentRequiredProjectNames projectNamesMap: projectNamesMap [
	| loadSpec requiredLoadSpecs loadSpecSet loadedProjects |
"prepare to load component"
	loadSpec := RwSpecification fromFile: resolvedProject specsRoot / componentName, 'ston'.
	loadSpec projectsHome:  self _testRowanProjectsSandbox.

"collect set of specs that will be required by the component component"
	requiredLoadSpecs := loadSpec resolve.

"arrange for the projects required by currently loaded component to be unloaded"
	loadSpecSet := (Rowan projectNamed: resolvedProject name) loadedLoadSpecifications.
	loadedComponentRequiredProjectNames do: [:projectName |
		"packages for project will be unloaded"
		(loadSpecSet specForProjectNamed: projectName) componentNames: {} ].

	requiredLoadSpecs do: [:aLoadSpec |
		 "Add the specs required by the component (overrides are on purpose) and make sure 
				that when the required spec is resolved, that the correct revision is checked out"
		aLoadSpec repositoryResolutionPolicy: #strict.
		loadSpecSet addLoadSpec: aLoadSpec ].

"load component"
	loadedProjects := loadSpecSet load.
	loadedProjects do: [:proj | 
		"unload projects whose packageNames are empty ... should correspond to projects with empty componentNames"
		proj packageNames isEmpty
			ifTrue: [ proj unload ] ].
	loadedProjects := loadedProjects select: [:proj | 
		"select loaded projects"
		proj isLoaded ].

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames:  (projectNamesMap at: componentName).
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
RwRowanSample9Test >> _spec0057GsFilein: includeRemoveAll projectName: projectName [
	| filein |
	filein := 'set compile_env: 0
! ------------------- Class definition for ', projectName, 'Class1
expectvalue /Class
doit
Object subclass: ''', projectName, 'Class1''
  instVarNames: #( ivar1)
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: RowanSample9_1
  options: #()
%
expectvalue /Class
doit
', projectName, 'Class1 comment: 
''I am an example class''
%
expectvalue /Class
doit
', projectName, 'Class1 category: ''RowanSample9-Core''
%
'.
	includeRemoveAll
		ifTrue: [ 
			filein := filein
				,
					'
! ------------------- Remove existing behavior from ', projectName, 'Class1
removeAllMethods ', projectName, 'Class1
removeallclassmethods ', projectName, 'Class1
' ].
	filein := filein
		,
			'
set compile_env: 0
! ------------------- Class methods for ', projectName, 'Class1
category: ''accessing''
classmethod: ', projectName, 'Class1
bar ^''bar''
%
! ------------------- Instance methods for ', projectName, 'Class1
category: ''accessing''
method: ', projectName, 'Class1
foo ^1
%

set compile_env: 0
! ------------------- Class definition for ', projectName, 'TestCase
expectvalue /Class
doit
TestCase subclass: ''', projectName, 'TestCase''
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: RowanSample9_1
  options: #()
%
expectvalue /Class
doit
', projectName, 'TestCase comment: 
''I test the example class''
%
expectvalue /Class
doit
', projectName, 'TestCase category: ''', projectName, '-Tests''
%
'.
	includeRemoveAll
		ifTrue: [ 
			filein := filein
				,
					'
! ------------------- Remove existing behavior from ', projectName, 'TestCase
removeAllMethods ', projectName, 'TestCase
removeAllClassMethods ', projectName, 'TestCase
' ].
	filein := filein
		,
			'
set compile_env: 0
! ------------------- Class methods for ', projectName, 'TestCase
! ------------------- Instance methods for ', projectName, 'TestCase
category: ''tests''
method: ', projectName, 'TestCase
test  self assert: ', projectName, 'Class1 new foo = 1
%
'.
	^ filein
]

{ #category : 'private' }
RwRowanSample9Test >> _spec0057GsFileinUnManagedMethods: includeRemoveAll projectName: projectName [
	| filein |
	filein := 'set compile_env: 0
! ------------------- Class definition for ', projectName, 'Class1
expectvalue /Class
doit
Object subclass: ''', projectName, 'Class1''
  instVarNames: #( ivar1)
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: RowanSample9_1
  options: #()
%
expectvalue /Class
doit
', projectName, 'Class1 comment: 
''I am an example class''
%
expectvalue /Class
doit
', projectName, 'Class1 category: ''RowanSample9-Core''
%
'.
	includeRemoveAll
		ifTrue: [ 
			filein := filein
				,
					'
! ------------------- Remove existing behavior from ', projectName, 'Class1
removeAllMethods ', projectName, 'Class1
removeallclassmethods ', projectName, 'Class1
' ].
	filein := filein
		,
			'
set compile_env: 0
! ------------------- Class methods for ', projectName, 'Class1
run
',  projectName, 'Class1 class
		compileMethod: ''bar ^''''bar''''''
		dictionaries: Rowan image symbolList
		category: #''accessing''
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.
',  projectName, 'Class1
		compileMethod: ''foo ^1''
		dictionaries: Rowan image symbolList
		category: #''accessing''
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.
%

set compile_env: 0
! ------------------- Class definition for ', projectName, 'TestCase
expectvalue /Class
doit
TestCase subclass: ''', projectName, 'TestCase''
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: RowanSample9_1
  options: #()
%
expectvalue /Class
doit
', projectName, 'TestCase comment: 
''I test the example class''
%
expectvalue /Class
doit
', projectName, 'TestCase category: ''', projectName, '-Tests''
%
'.
	includeRemoveAll
		ifTrue: [ 
			filein := filein
				,
					'
! ------------------- Remove existing behavior from ', projectName, 'TestCase
removeAllMethods ', projectName, 'TestCase
removeAllClassMethods ', projectName, 'TestCase
' ].
	filein := filein
		,
			'
set compile_env: 0
! ------------------- Class methods for ', projectName, 'TestCase
! ------------------- Instance methods for ', projectName, 'TestCase


run
',  projectName, 'TestCase
		compileMethod: ''test  self assert: ', projectName, 'Class1 new foo = 1''
		dictionaries: Rowan image symbolList
		category: #''tests''
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.
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
	project := loadedProjects projectNamed: projectName.
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
RwRowanSample9Test >> _testUnmanagedSpec_0057: loadSpec useStandardMethodFilein: useStandardMethodFilein  removeAllMethods: removeAllMethods packageName1: packageName1 packageName2: packageName2 className1: className1 className2: className2 [
	"based on spec_0057 after loading, do a filein (with removeAllMethods) and run unmanaged report and an audit"

	| projectName projectNames resolvedProject loadedProjects stream |

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveProject.
"load project"
	loadedProjects := resolvedProject load.
"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"do filein"
	stream := useStandardMethodFilein 
		ifTrue: [  (self _spec0057GsFilein: removeAllMethods projectName: projectName) readStreamPortable ]
		ifFalse: [ (self _spec0057GsFileinUnManagedMethods: removeAllMethods projectName: projectName) readStreamPortable ].
	((Rowan globalNamed: 'GsFileIn') _fromStream: stream)
		setEnableRemoveAll: true;
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
			resolvedProject _projectDefinitionPlatformConditionalAttributes
				= resolvedProject platformConditionalAttributes.
	self
		assert:
			(x := resolvedProject platformConditionalAttributes)
				= (y := project platformConditionalAttributes).
	self
		assert:
			(x := resolvedProject _projectDefinitionPlatformConditionalAttributes)
				= (y := project _projectDefinitionPlatformConditionalAttributes).
	self assert: (project packageNames includes: packageName)
]

{ #category : 'private' }
RwRowanSample9Test >> _validate_571: resolvedProject loadedProjects: loadedProjects expectedProjectNames: expectedProjectNames packageName: packageName present: isPresent [
	| project x y |
	project := loadedProjects projectNamed: resolvedProject projectName.
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: expectedProjectNames.

	self
		assert:
			(x := resolvedProject platformConditionalAttributes)
				= (y := project platformConditionalAttributes).
	self
		assert:
			(x := resolvedProject customConditionalAttributes)
				= (y := project customConditionalAttributes).

	isPresent
		ifTrue: [ self assert: (project packageNames includes: packageName) ]
		ifFalse: [ self deny: (project packageNames includes: packageName) ]
]

{ #category : 'private' }
RwRowanSample9Test >> _validate_spec_0061_0062_0063: project expectedComponentNames: expectedComponentNames expectedPackageGroupMap: packageGroupMap [
	self
		assert: project loadedComponentNames asArray sort
		equals: expectedComponentNames sort.

	self
		assert: project packageGroupNames asArray sort
		equals: packageGroupMap keys sort.

	packageGroupMap
		keysAndValuesDo: [ :componentName :expectedClassNames | 
			self
				assert: (project allClassNamesFor: componentName) asArray sort
				equals: expectedClassNames sort ]
]

{ #category : 'private' }
RwRowanSample9Test >> _validate_spec_0061: project [
	| packageGroupMap projectsClassNames |
	projectsClassNames := #('Rs9AbstractProject' 'Rs9DefinedProjects' 'Rs9Project' 'Rs9ResolvedProject' 'Rs9AbstractUnloadedProject').
	packageGroupMap := Dictionary new
		at: 'groups/Examples' put: #();
		at: 'groups/Rowan' put: #();
		at: 'groups/Tests' put: #();
		at: 'groups/Projects' put: projectsClassNames;
		at: 'groups/WorkGroup_dkh' put: projectsClassNames , #('Rs9Interface');
		at: 'groups/Loader'
			put:
				#('Rs9GsLoadedClass' 'Rs9GsLoadedClassExtension' 'Rs9GsLoadedMethod' 'Rs9GsLoadedPackage' 'Rs9GsLoadedProject' 'Rs9LoadedClass' 'Rs9LoadedClassExtension' 'Rs9LoadedMethod' 'Rs9LoadedPackage' 'Rs9LoadedProject' 'Rs9LoadedThing');
		yourself.
	^ self
		_validate_spec_0061_0062_0063: project
		expectedComponentNames:
			#('common/Core' 'common/Examples' 'common/examples/Examples' 'common/Loader' 'common/platforms/gemstone/Loader' 'common/platforms/Loader' 'common/platforms/pharo/Loader' 'common/Tests' 'common/tests/Tests' 'Core')
		expectedPackageGroupMap: packageGroupMap
]

{ #category : 'private' }
RwRowanSample9Test >> _validate_spec_0063: project [
	| packageGroupMap projectsClassNames |
	projectsClassNames := #('Rs9AbstractProject' 'Rs9DefinedProjects' 'Rs9Project' 'Rs9ResolvedProject' 'Rs9AbstractUnloadedProject').
	packageGroupMap := Dictionary new
		at: 'Examples_group' put: #();
		at: 'Rowan_group' put: #();
		at: 'Tests_group' put: #();
		at: 'Projects_group' put: projectsClassNames;
		at: 'WorkGroup_dkh_group' put: projectsClassNames , #('Rs9Interface');
		at: 'Loader_group'
			put:
				#('Rs9GsLoadedClass' 'Rs9GsLoadedClassExtension' 'Rs9GsLoadedMethod' 'Rs9GsLoadedPackage' 'Rs9GsLoadedProject' 'Rs9LoadedClass' 'Rs9LoadedClassExtension' 'Rs9LoadedMethod' 'Rs9LoadedPackage' 'Rs9LoadedProject' 'Rs9LoadedThing');
		yourself.
	^ self
		_validate_spec_0061_0062_0063: project
		expectedComponentNames:
			#('Core' 'Examples_examples' 'Loader_common' 'Loader_gemstone' 'Loader_pharo' 'Tests_tests')
		expectedPackageGroupMap: packageGroupMap
]

{ #category : 'private' }
RwRowanSample9Test >> _validateSpec_0017_componentStructure: resolvedProject [
	self
		_validateComponent: (resolvedProject componentNamed: 'Core')
			componentNames: { 'tests/Tests'}
			packageNames:
				{resolvedProject projectName, '-Core'.
				resolvedProject projectName, '-Extensions'.
				resolvedProject projectName, '-Extensions1'}
			projectNames: {};
		_validateComponent: (resolvedProject componentNamed: 'tests/Tests')
			componentNames: {}
			packageNames: {resolvedProject projectName, '-Tests'}
			projectNames: {};
		yourself
]

{ #category : 'private' }
RwRowanSample9Test >> _validateSpec_0021_componentStructure: resolvedProject [
	self
		_validateComponent: (resolvedProject componentNamed: 'Core')
			componentNames: {'tests/Tests'}
			packageNames:
				{resolvedProject projectName, '-Core'.
				resolvedProject projectName, '-Extensions'}
			projectNames: {};
		_validateComponent: (resolvedProject componentNamed: 'tests/Tests')
			componentNames: {}
			packageNames: {resolvedProject projectName, '-Tests'}
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
	project := loadedProjects projectNamed: projectName.

"validate"
	audit := project audit.
	self assert: audit isEmpty.

"remove compiled methods and recompile without packaging"
	className := projectName , 'Class1'.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Extensions'.
	class := Rowan globalNamed: className.
	self _auditRemoveAndRecompiledMethods_0081: projectName.

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
	project := loadedProjects projectNamed: projectName.

"validate"
	audit := project audit.
	self assert: audit isEmpty.

"rename method categories"
	className := projectName , 'Class1'.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Extensions'.
	class := Rowan globalNamed: className.

	class renameCategory: '*', projectName asLowercase, '-extensions' to: 'another category'.
	class class renameCategory: '*', projectName asLowercase, '-extensions' to: 'another category'.

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
	project := loadedProjects projectNamed: projectName.

"validate"
	audit := project audit.
	self assert: audit isEmpty.

"remove compiled methods and recompile without packaging"
	self _auditRemoveAndRecompiledMethods_0081: projectName.

"rename method categories"
	className := projectName , 'Class1'.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Extensions'.
	class := Rowan globalNamed: className.
	class renameCategory: '*', projectName asLowercase, '-extensions' to: 'another category'.
	class class renameCategory: '*', projectName asLowercase, '-extensions' to: 'another category'.

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
	project := loadedProjects projectNamed: projectName.

"validate"
	audit := project audit.
	self assert: audit isEmpty.

"remove compiled methods"
	className := projectName , 'Class1'.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Extensions'.
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
	resolvedProject := loadSpec resolveProject.
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
	project := loadedProjects projectNamed: projectName.
	self assert: project audit isEmpty.
	self _runTestSuiteForProject: project.

	loadSpec := self _loadSpecNamed: 'spec_0002'.	"basic project (remove instancesInvariant option and move class to default symbol dictionary: self _sampleSymbolDictionaryName1)"

"resolve/load"
	resolvedProject := loadSpec resolveProject.
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects projectNamed: projectName.
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

	resolvedProject := project1 resolveProject.

	(resolvedProject
		addSubcomponentNamed: 'Core1'
		condition: 'common'
		comment: 'contains ' , projectName , '-Core1 package and Core2 component')
			addComponentNames: #('Core2');
			yourself.
	resolvedProject
		addSubcomponentNamed: 'Core2'
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
	resolvedProject := loadSpec resolveProject.

"load project soec_0008"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

"spec_0010"
	loadSpec := self _loadSpecNamed: 'spec_0010'.
	resolvedProject := loadSpec resolveProject.

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
			self assert: ex description equals: 'UserDefinedError: The extension class named ''', projectName, 'Class1'' cannot be found in this symbol dictionary (#''RowanSample9_3''). Packages that extend a class must be loaded into the symbol dictionary in which the class is installed.'.
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
			self assert: ex description equals: 'UserDefinedError: The extension class named ''', projectName, 'Class1'' cannot be found in this symbol dictionary (#''RowanSample9_1''). Packages that extend a class must be loaded into the symbol dictionary in which the class is installed.'.
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
	resolvedProject := loadSpec resolveProject.

"load primer project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

	loadSpec := self _loadSpecNamed: 'spec_0002'.

"resolve"
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

"load primer project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

	loadSpec := self _loadSpecNamed: 'spec_0006'.

"resolve"
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

"load primer project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

	loadSpec := self _loadSpecNamed: 'spec_0006'.

"resolve"
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

"load primer project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

	loadSpec := self _loadSpecNamed: 'spec_0007'.	"primer -- spec_0005 with instance variables"

"resolve"
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

"load primer project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

	loadSpec := self _loadSpecNamed: 'spec_0005'.

"resolve"
	resolvedProject := loadSpec resolveProject.

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
	"RwProjectDefinition>>removePackageNamed: needs to clean up the component references to the removed package name"

	"https://github.com/dalehenrich/Rowan/issues/549"

	| loadSpec projectName  resolvedProject1 resolvedProject2 loadedProjects project |
	loadSpec := self _loadSpecNamed: 'spec_0014'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject1 := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject1 load.

"validate"
	self _standard_validate: resolvedProject1 loadedProjects: loadedProjects.

"remove package RowanSample9-Extensions"
	project := loadedProjects projectNamed: projectName.
	resolvedProject2 := project asDefinition.
	resolvedProject2 removePackageNamed: projectName, '-Extensions'.

"validation should pass"
	self assert: resolvedProject2 _validate
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_549_errorCondition [
	"Verify that when component packages and package definitions are inconsistent we generate an error"

	"https://github.com/dalehenrich/Rowan/issues/549"

	| loadSpec projectName  resolvedProject1 errorHit projectsHome |
	loadSpec := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0015.ston'.
	projectsHome := '$ROWAN_PROJECTS_SANDBOX' asFileReference / 'isolated'.
	projectsHome ensureDeleteAll.

	loadSpec projectsHome: projectsHome.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject1 := loadSpec resolveProject.
	resolvedProject1 repository checkout: 'issue_549'.	"tag pointing to inconsistent checkout"
	resolvedProject1 read.

"load project"
	errorHit := false.
	[resolvedProject1 _validate ] 
		on: Error
		do: [:ex | 
			self assert: ex description equals: 'UserDefinedError: Component references package(s) that are not defined
	The following packages are defined, but not referenced in a component:
		', projectName, '-Extensions
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
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
	self _validateSpec_0021_componentStructure: resolvedProject.

"load spec_0022"
	loadSpec := self _loadSpecNamed: 'spec_0022'.

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project should trigger preload doit"
	loadedProjects := resolvedProject load.

"validate test will validate that preload doit executed"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
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
		gitUrl: 'git@github.com:dalehenrich/RowanSample9.git'.

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
	project := loadedProjects projectNamed: projectName.
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
	resolvedProject := loadSpec resolveProject.

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
	className1 := projectName, 'Class1'.

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
	resolvedProject := loadSpec resolveProject.

"load project using symbol dict  _sampleSymbolDictionaryName1"
	resolvedProject 
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName2 forPackageNamed: packageName4;
		yourself.
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

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

	| loadSpec projectName projectNames resolvedProject loadedProjects project ar className class |
	loadSpec := self _loadSpecNamed: 'spec_0002'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project using symbol dict  _sampleSymbolDictionaryName1"
	resolvedProject gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1.
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.
	className := projectName , 'Class1'.

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
	self assert: (ar first at: 1) name equals: self _sampleSymbolDictionaryName2.
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

	packageName := projectName , '-Core'.

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project using symbol dict  _sampleSymbolDictionaryName1"
	resolvedProject gemstoneSetSymbolDictName: self _sampleSymbolDictionaryName1 forPackageNamed: packageName.
	loadedProjects := resolvedProject load.
	project := loadedProjects  projectNamed: projectName.
	className := projectName , 'Class1'.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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

	loadSpec := self _loadSpecNamed: 'spec_0002'.
	projectName := loadSpec projectName.

	primerProjectName := 'PrimerSample9'.
	symDictName1 := self _symbolDictionaryName1.
	symDictName2 := self _symbolDictionaryName2.
	primerPackageName1 := primerProjectName, '-', symDictName1.
	primerPackageName2 := primerProjectName, '-', symDictName2.
	symbolList := Rowan image symbolList.
	componentName := 'Core'.

	{ primerProjectName. projectName} 
		do: [:pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
					ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	"Create classes in a couple of symbol dictionaries"
	symDict := Rowan image newOrExistingSymbolDictionaryNamed: symDictName1.
	theClass := Object subclass: projectName, 'Class1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	theClass category: primerPackageName1.
	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	symDict := Rowan image newOrExistingSymbolDictionaryNamed: symDictName2.
	theClass := IdentityKeyValueDictionary
		subclass: projectName, 'IdentityKeyValueDictionary'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	theClass category: primerPackageName2.
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
		addLoadComponentNamed: componentName;
		addPackageNamed: primerPackageName1 toComponentNamed: componentName;
		addPackageNamed: primerPackageName2  toComponentNamed: componentName;
		gemstoneSetSymbolDictName: symDictName1 forPackageNamed: primerPackageName1;
		gemstoneSetSymbolDictName: symDictName2 forPackageNamed: primerPackageName2;
		yourself.

	primerResolvedProject resolveProject; load.

	"Adopt the classes into the primer project"
	Rowan packageTools adopt
		adoptSymbolDictionaryNamed: symDictName1 intoPackageNamed: primerPackageName1;
		adoptSymbolDictionaryNamed: symDictName2 intoPackageNamed: primerPackageName2.

	"load the RowanSample4 project from disk - overly the correct project structure over the primer project classes"
	(self _testRowanProjectsSandbox / projectName) ensureDeleteAll.

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
		primerPackageName1 primerPackageName2 loadedPrimerProject componentName loadSpec primerResolvedProject audit className class newClass |

	loadSpec := self _loadSpecNamed: 'spec_0002'.
	projectName := loadSpec projectName.

	primerProjectName := 'PrimerSample9'.
	symDictName1 := self _symbolDictionaryName1.
	symDictName2 := self _symbolDictionaryName2.
	primerPackageName1 := primerProjectName, '-', symDictName1.
	primerPackageName2 := primerProjectName, '-', symDictName2.
	symbolList := Rowan image symbolList.
	componentName := 'Core'.
	className := projectName, 'Class1'.

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
	theClass category: primerPackageName1.
	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	symDict := Rowan image newOrExistingSymbolDictionaryNamed: symDictName2.
	theClass := IdentityKeyValueDictionary
		subclass: projectName, 'IdentityKeyValueDictionary'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	theClass category: primerPackageName2.
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
		addLoadComponentNamed: componentName;
		addPackageNamed: primerPackageName1 toComponentNamed: componentName;
		addPackageNamed: primerPackageName2  toComponentNamed: componentName;
		gemstoneSetSymbolDictName: symDictName1 forPackageNamed: primerPackageName1;
		gemstoneSetSymbolDictName: symDictName2 forPackageNamed: primerPackageName2;
		yourself.

	primerResolvedProject resolveProject; load.

	"Adopt the classes into the primer project"
	Rowan packageTools adopt
		adoptSymbolDictionaryNamed: symDictName1 intoPackageNamed: primerPackageName1;
		adoptSymbolDictionaryNamed: symDictName2 intoPackageNamed: primerPackageName2.

	"load the RowanSample4 project from disk - overly the correct project structure over the primer project classes"
	(self _testRowanProjectsSandbox / projectName) ensureDeleteAll.

	resolvedProject := loadSpec resolveStrict.

	class := Rowan globalNamed: className.
	resolvedProject load.
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
	resolvedProject := loadSpec resolveProject.
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
	project := loadedProjects projectNamed: projectName.
	loadedCommitId := project loadedCommitId.

"benign load sequence - loaded commit id should change"
	loadSpec := self _loadSpecNamed: 'spec_0045'.

"resolve project"
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.
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
	resolvedProject := loadSpec resolveProject.
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
	(loadedProjects projectNamed: projectName) unload.
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
	resolvedProject := loadSpec resolveProject.
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
	project := loadedProjects projectNamed: projectName.
	
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
	resolvedProject := loadSpec resolveProject.
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

	resolvedProject := loadSpec resolveProject.
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
	resolvedProject := loadSpec resolveProject.
	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		yourself.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

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
	resolvedProject := loadSpec resolveProject.
	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		yourself.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

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
	resolvedProject := loadSpec resolveProject.

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

	projectName := loadSpec projectName.
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
	resolvedProject := loadSpec resolveProject.
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

	projectName := loadSpec projectName.
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
	resolvedProject := loadSpec resolveProject.
	resolvedProject
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'NewPackage';
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone';
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: projectName , '-' , 'GemStone-Tests';
		yourself.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

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
	resolvedProject := loadSpec resolveProject.
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
	project := loadedProjects projectNamed: projectName.
	
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
	resolvedProject := loadSpec resolveProject.

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
	project := loadedProjects projectNamed: projectName.
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
	resolvedProject := loadSpec resolveProject.
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
		propertyModification := propertyElementsModified at: #'gs_SymbolDictionary'.
		self assert: propertyModification oldValue isNil.
		self assert: propertyModification newValue = self _symbolDictionaryName asString ].
	(RwSample1TestValidationVisitor new)
		defaultNoopBlock: [ self assert: false description: 'unexpected modification' ];
		changedProjectBlock: [:projectModification | self assert: projectModification propertiesModification isEmpty ];
		changedPackageBlock: [:packageModification | 
			| packageName |
			packageName := packageModification key.
			({projectName , '-Extensions' . projectName , '-Core' . projectName , '-Tests' } includes: packageName)
				ifTrue: [ 
					packageName = (projectName , '-Extensions')
						ifTrue: [ self assert: packageModification classesModification isEmpty ]
						ifFalse: [ self deny: packageModification classesModification isEmpty ].
					self assert: packageModification classExtensionsModification isEmpty.
					propertyModValidationBlock value: packageModification propertiesModification ]
				ifFalse: [
					self assert: (packageModification propertiesModification isEmpty or: [packageModification propertiesModification elementsModified keys asArray = {#'gs_SymbolDictionary'}] )] ];
		changedClassBlock: [ :classModification :visitor |
			| packageName |
			packageName := visitor currentPackageDefinition key.
			self assert: ({projectName , '-Core' . projectName , '-Tests' } includes: packageName).
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

	| loadSpec projectName resolvedProject loadedProjects project  audit loadSpec2 resolvedProject2 hitError errorDescription |
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
	project := loadedProjects projectNamed: projectName.
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
	hitError := false.
	[loadedProjects := resolvedProject2 load] 
		on: Error 
		do: [:ex |
			errorDescription := ex description.
			errorDescription = 'UserDefinedError: A class (RowanSample9Class1) may not be installed in a symbol dictionary (RowanSample9Class1  with the same name.' ifTrue: [ hitError := true ] ].
	self 
		assert: hitError 
		description: 'Unexpected pass with error description: ', errorDescription printString	"watch out ... jadeite won't behave well with class and symbol dictionary of same name ... but that's okay"
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue533 [
	"https://github.com/GemTalk/Rowan/issues/533"

	| loadSpec projectName projectNames resolvedProject definedProject componentName component packageName |
	loadSpec := self _loadSpecNamed: 'spec_0004'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	definedProject := resolvedProject defined.
	packageName := projectName, '-New'.
	componentName := 'Core2'.

	definedProject addPackageNamed: packageName toComponentNamed: componentName.
	definedProject 
		gemstoneSetMethodEnvForUser: 'SystemUser' 
			to: 1
			forPackageNamed: projectName, '-Core2';
		gemstoneSetMethodEnvForUser: 'DataCurator' 
			to: 2
			forPackageNamed: packageName;
		gemstoneSetMethodEnvForUser: RwLoadSpecificationV2 _gemstoneAllUsersName
			to: 3
			forPackageNamed: packageName;
		yourself.
	component := definedProject componentNamed: componentName.

"validate"
	self 
		assert: (definedProject gemstoneMethodEnvForPackageNamed: packageName forUser: 'Bozo')
		equals: 3.	"existing package, unkown user ... should match allusers"
	self 
		assert: (definedProject  gemstoneMethodEnvForPackageNamed: packageName forUser: 'SystemUser')
		equals: 3.	"existing package, no entry for SystemUser and this package"	
	self 
		assert: (definedProject  gemstoneMethodEnvForPackageNamed: packageName forUser: 'DataCurator')
		equals: 2.	"existing package, entry for DataCurator"
	self 
		assert: (definedProject gemstoneMethodEnvForPackageNamed: projectName, '-Core2'  forUser: 'SystemUser')
		equals: 1.	"existing package, entry for SystemUser"
	self 
		assert: (definedProject gemstoneMethodEnvForPackageNamed: projectName, '-Core2X'  forUser: 'SystemUser')
		equals: (definedProject gemstoneDefaultMethodEnvForUser: 'SystemUser').	"unknown package, should match default"
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue571 [
	"record/reuse/update the platformConditionalAttributes used to read the project from disk"

	"https://github.com/GemTalk/Rowan/issues/571"

	| loadSpec projectName projectNames resolvedProject loadedProjects  packageName1 theLoadSpec project |
	loadSpec := self _loadSpecNamed: 'spec_0032'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.
	packageName1 := projectName , '-Specs-V1'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project - baseline"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self
		_validate_571: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
		packageName: packageName1
		present: true.

"resolve project using loadSpec - remove v1 attribute, resulting in packageName1 being unloaded"
	theLoadSpec := loadSpec copy.
	theLoadSpec removeCustomConditionalAttributes: #('v1').
	resolvedProject := theLoadSpec resolveProject.
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
	theLoadSpec addCustomConditionalAttributes: #( 'v1' ).
	resolvedProject := theLoadSpec resolveProject.
	resolvedProject load.
"validate"
	self
		_validate_571: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
		packageName: packageName1
		present: true.

"resolve project from loaded definition - remove v1 attribute, resulting in packageName1 being unloaded"
	resolvedProject := project asDefinition.
	resolvedProject removeCustomConditionalAttributes: #('v1').
	resolvedProject read: Rowan platformConditionalAttributes.
	loadedProjects := resolvedProject load.
"validate"
	self
		_validate_571: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
		packageName: packageName1
		present: false.

"resolve project from loaded definition - add v1 attribute back, resulting in packageName1 being loaded"
	resolvedProject := project asDefinition.
	resolvedProject addCustomConditionalAttributes: #('v1').

"load"
	loadedProjects := resolvedProject loadProjectSet.

"validate"
	self
		_validate_571: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
		packageName: packageName1
		present: true.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue594 [
	"tests for RwProject>>reloadAndIncludeCustomConditionalAttributes: and RwProject>>reloadAndRemoveCustomConditionalAttributes:"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName resolvedProject loadedProjects project audit |
	loadSpec := self _loadSpecNamed: 'spec_0002'.
	self assert: (loadSpec customConditionalAttributes includes: 'tests').

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
	self deny: project testSuite tests isEmpty.

"reload without 'tests' attribute"
	loadSpec := project loadSpecification.
	self assert: (loadSpec customConditionalAttributes includes: 'tests').
	loadSpec removeCustomConditionalAttributes: #('tests').
	loadedProjects := loadSpec load.

"validate"
	self assert: (audit := project audit) isEmpty.
	self assert: project testSuite tests isEmpty.

"reload with 'tests' attribute"
	loadSpec := project loadSpecification.
	self deny: (loadSpec customConditionalAttributes includes: 'tests').
	loadSpec addCustomConditionalAttributes: #('tests').
	loadedProjects := loadSpec load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
	self deny: project testSuite tests isEmpty.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue653 [
	"use spec_0061 to test compoent API "

	"https://github.com/GemTalk/Rowan/issues/653"

	| loadSpec projectName projectNames resolvedProject loadedProjects project |
	loadSpec := self _loadSpecNamed: 'spec_0061'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"component API"
	self 
		assert: ((project loadedSubcomponentsOf: 'Core') collect: [:each | each name]) sort
		equals: #( 'common/Core' 'common/Examples' 'common/Loader' 'common/Tests' 'groups/Examples' 'groups/Loader' 'groups/Projects' 'groups/Rowan' 'groups/Tests' 'groups/WorkGroup_dkh') sort.
	self 
		assert: ((project loadedSubcomponentsOf: 'common/Core') collect: [:each | each name]) sort
		equals: #() sort.
	self 
		assert: ((project loadedSubcomponentsOf: 'common/Loader') collect: [:each | each name]) sort
		equals: #('common/platforms/Loader') sort.
	self 
		assert: ((project loadedSubcomponentsOf: 'common/platforms/Loader') collect: [:each | each name]) sort
		equals: #('common/platforms/gemstone/Loader') sort.
	self 
		assert: (project loadedSubcomponentsOf: 'common/examples/Examples' ifNone: []) isNil.
	self
		assert: ((project loadedSubcomponentsOf: 'common/Tests' attributes: #('common' 'tests') ifNone: [])  collect: [:each | each name])
		equals: #('common/tests/Tests') .
	self
		assert: (project loadedSubcomponentsOf: 'common/Tests' attributes: #('common') ifNone: [#()])
		equals: #() .

"package groups validation"
	self
		assert: (project packageGroupNamed: 'groups/Examples') packageNames sort
		equals: { projectName , '-Examples'} sort.
	self
		assert: (project packageGroupNamed: 'groups/Loader') packageNames sort
		equals: { projectName , '-Loader' . projectName , '-Loader-GemStone' . projectName , '-Loader-Pharo'} sort.
	self
		assert: (project packageGroupNamed: 'groups/Projects') packageNames sort
		equals: { projectName , '-Core-Projects'} sort.
	self
		assert: (project packageGroupNamed: 'groups/Rowan') packageNames sort
		equals: { projectName , '-Core-Rowan'} sort.
	self
		assert: (project packageGroupNamed: 'groups/Tests') packageNames sort
		equals: { projectName , '-Tests'} sort.
	self
		assert: (project packageGroupNamed: 'groups/WorkGroup_dkh') packageNames sort
		equals: { projectName , '-Core' . projectName , '-Core-Projects'} sort.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue711_2 [
	"
		A project may be read into memory using platform conditional attributes that
			do not match the platform conditional attributes of the current image.
		A project that has been read into memory may be loaded into the image.
		A project in memory whose platform conditional attributes do not match the 
			current image should not be allowed to be loaded.
		The  _Project_Definition_PlatformConditionalAttributes should match a subset
			of the platform conditional attributes of the current image, to be eligible
			to be loaded.

		spec_0030 has packages that are conditional upon a pharo platform attribute,
			so use spec_0030 for this test.
	"

	"https://github.com/GemTalk/Rowan/issues/711"

	| loadSpec projectName projectNames loadSpecs pharoSpec pharoProject pharoPlatformAttributes testCustomAttributes warningMessage gsVersionString errorMessage expectedMessage warningCount |
	loadSpec := self _loadSpecNamed: 'spec_0030'.

	pharoPlatformAttributes := { 'common' . 'pharo' }.
	testCustomAttributes := { 'tests' }.

	projectName := loadSpec projectName.
	projectNames := {projectName }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	loadSpecs := loadSpec resolve.

"define attributes"
	pharoSpec := loadSpecs specForProjectNamed: projectName.
	pharoSpec addCustomConditionalAttributes: testCustomAttributes.

"initial load"
	pharoProject := pharoSpec read: pharoPlatformAttributes.

"validate"
	self assert: pharoProject packageNames asArray sort equals: { projectName , '-Core' . projectName , '-Definitions' . projectName , '-Definitions-Pharo' . projectName , '-Definitions-Pharo-Tests' . projectName , '-Definitions-Tests' . projectName , '-Specs' . projectName , '-Specs-Tests' . projectName , '-Tests'} sort.

"load ... and expect one Warning and an error"
	warningCount := 0.
	[pharoProject load]
		on: Warning, Error
		do: [:ex | 
			(ex isKindOf: Warning)
				ifTrue: [ 
					warningCount := warningCount + 1.
					warningMessage := ex messageText .
					ex resume ].
			(ex isKindOf: Error)
				ifTrue: [ errorMessage := ex messageText ] ].
	gsVersionString := (System gemVersionReport at: 'gsVersion') asRwGemStoneVersionNumber asString.
	warningMessage 
		ifNil: [ self assert: false description: 'Expected Warning not signalled' ].
	warningCount = 1
		ifFalse: [  self assert: false description: 'Expected only a single Warning not ', warningCount printString ].
	expectedMessage := 'the project ''', projectName , ''' was read from disk with platform conditional attributes (anArray( ''common'', ''pharo'')) that is not compatible with the platform conditional attributes for the current image (anArray( ''common'', ''gemstone'', ''gemstone-kernel'', ', gsVersionString, ')) so the project may have compile errors when loaded.'.
	self 
		assert: warningMessage 
		equals: expectedMessage. "Warning signalled, but expected message did not match'"
	errorMessage 
		ifNil: [ self assert: false description: 'Expected Error not signalled' ].
	expectedMessage := ''.
	self 
		assert: (errorMessage beginsWith: 'a CompileError occurred (error 1001), undefined symbol  PluggableDictionary,'). "Error signalled, but expected message did not match"
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue743_0 [
	"Class>>category: hooked up to Rowan"

	"https://github.com/GemTalk/Rowan/issues/743"

	| loadSpec projectName projectNames resolvedProject loadedProjects project audit class category definedProject projectModification packagesModification elementsModified packageModification classesModification classModification propertiesModification propertyModification oldCategory confirmationBlock |
	loadSpec := self _loadSpecNamed: 'spec_0057'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	confirmationBlock := [:oldCat :newCat |
		"confirm that the class category change shows up as a modification"
		projectModification := project asDefinition compareAgainstBase: definedProject _concreteProject.
		self deny: projectModification isEmpty.
		self assert: projectModification propertiesModification isEmpty.
		packagesModification := projectModification packagesModification.
		self deny: packagesModification isEmpty.
		elementsModified := packagesModification elementsModified.
		self assert: elementsModified size = 1.
		packageModification := elementsModified at: projectName, '-Core'.
		self assert: packageModification propertiesModification isEmpty.
		self assert: packageModification classExtensionsModification isEmpty.
		classesModification := packageModification classesModification.
		self deny: classesModification isEmpty.
		elementsModified := classesModification elementsModified.
		self assert: elementsModified size = 1.
		classModification := elementsModified at: projectName, 'Class1'.
		self assert: classModification instanceMethodsModification isEmpty.
		self assert: classModification classMethodsModification isEmpty.
		propertiesModification := classModification propertiesModification.
		self deny: propertiesModification isEmpty.
		elementsModified := propertiesModification elementsModified.
		self assert: elementsModified size = 1.
		propertyModification := elementsModified at: #'category'.
		self assert: propertyModification oldValue equals: oldCat.
		self assert: propertyModification newValue equals: newCat.
	].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	project := loadedProjects projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: projectName, 'Class1'.
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
		projectModification := project asDefinition compareAgainstBase: definedProject _concreteProject.
		self deny: projectModification isEmpty.
		self assert: projectModification propertiesModification isEmpty.
		packagesModification := projectModification packagesModification.
		self deny: packagesModification isEmpty.
		elementsModified := packagesModification elementsModified.
		self assert: elementsModified size = 1.
		packageModification := elementsModified at: projectName, '-Core'.
		self assert: packageModification propertiesModification isEmpty.
		self assert: packageModification classExtensionsModification isEmpty.
		classesModification := packageModification classesModification.
		self deny: classesModification isEmpty.
		elementsModified := classesModification elementsModified.
		self assert: elementsModified size = 1.
		classModification := elementsModified at: projectName, 'Class1'.
		self deny: classModification instanceMethodsModification isEmpty.
		elementsModified :=  classModification instanceMethodsModification elementsModified.
		methodModification := elementsModified at: #instanceFoo.
		propertiesModification := methodModification propertiesModification.
		self deny: propertiesModification isEmpty.
		elementsModified := propertiesModification elementsModified.
		propertyModification := elementsModified at: #'protocol'.
		self assert: propertyModification oldValue equals: oldCat.
		self assert: propertyModification newValue equals: newCat.
	].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	project := loadedProjects projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: projectName, 'Class1'.

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
RwRowanSample9Test >> testIssue752_01 [
	"
		Load RowanSample9:spec_0005, then load RowanSample10:spec_0005 which stomps on RowanSample9Class1>>foo and RowanSample9Class1 class>>bar.

		RowanSample9 should be dirty and method should be owned by RowanSample10 project. Audit should be clean.

		Reload RowanSample9; RowanSample10 should be dirty and method should be owned by RowanSample9 project. Audit should be clean.
	"

	"https://github.com/GemTalk/Rowan/issues/752"

	| loadSpec1 loadSpec2 className projectName1 projectName2 projects project1 project2 |
	loadSpec1 := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0005.ston'.
	projectName1 := loadSpec1 projectName.
	loadSpec2 := (RwRowanSample10Test _loadSpecNamed: 'spec_0005')
		gemstoneSetDefaultSymbolDictNameTo: 'RowanSample9_2';
		yourself.
	projectName2 := loadSpec2 projectName.

  {projectName1 . projectName2 }
    do: [ :pn |
      (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
        ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	className := projectName1 , 'Class1'.

"load project1"
	projects := loadSpec1 resolveStrict load.
	project1 := projects projectNamed: projectName1.

"validate"
	self assert: projects size equals: 1.
	self assert: project1 audit isEmpty.
	self assert: (Rowan globalNamed: className) new foo equals: 1.
	self assert: (Rowan globalNamed: className) bar equals: 'bar'.
	self deny: project1 isDirty.

"load project2"
	projects := loadSpec2 load.
	project2 := projects projectNamed: projectName2.

"validate"
	self assert: projects size equals: 2.
	self assert: project1 audit isEmpty.
	self assert: project2 audit isEmpty.
	self assert: (Rowan globalNamed: className) new foo equals: 2.
	self assert: (Rowan globalNamed: className) bar equals: 'bare'.
	self assert: project1 isDirty.
	self deny: project2 isDirty.

"reload project1 - restoring original implementation"
	projects := project1 reload.

"validate"
	self assert: projects size equals: 2.
	self assert: project1 audit isEmpty.
	self assert: project2 audit isEmpty.
	self assert: (Rowan globalNamed: className) new foo equals: 1.
	self assert: (Rowan globalNamed: className) bar equals: 'bar'.
	self deny: project1 isDirty.
	self assert: project2 isDirty.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue752_02a [
	"
		Load RowanSample9:spec_0005, then load RowanSample10:spec_0005 but remove the methods 
			RowanSample9Class1>>foo and RowanSample9Class1 class>>bar from RowanSample9 project.

		RowanSample9 should be clean and method should be owned by RowanSample10 project. Audit should be clean.
	"

	"https://github.com/GemTalk/Rowan/issues/752"

	| loadSpec1 loadSpec2 className projectName1 projectName2 projects project1 project2 
		projectSetDefinition projectDef |
	loadSpec1 := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0005.ston'.
	loadSpec2 := (RwRowanSample10Test _loadSpecNamed: 'spec_0005')
		gemstoneSetDefaultSymbolDictNameTo: 'RowanSample9_2';
		yourself.
	projectName1 := loadSpec1 projectName.
	projectName2 := loadSpec2 projectName.

  {projectName1 . projectName2 }
    do: [ :pn |
      (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
        ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	className := projectName1 , 'Class1'.

"load project1"
	projects := loadSpec1 resolveStrict load.
	project1 := projects projectNamed: projectName1.

"validate"
	self assert: projects size equals: 1.
	self assert: project1 audit isEmpty.
	self assert: (Rowan globalNamed: className) new foo equals: 1.
	self assert: (Rowan globalNamed: className) bar equals: 'bar'.
	self deny: project1 isDirty.

"load project2 and remove the methods RowanSample9Class1>>foo and RowanSample9Class1 class>>bar from project1"
	projectSetDefinition := (RwProjectSetDefinition new)
		addProject: loadSpec2 read;
		yourself.
	projectDef := project1 asDefinition.
	((projectDef packageNamed: 'RowanSample9-Core1')
		classDefinitionNamed: className)
			removeInstanceMethod: #foo;
			removeClassMethod: #bar;
			yourself.
	projectSetDefinition addProject: projectDef.
	projects := projectSetDefinition load.
	project2 := projects projectNamed: projectName2.

"validate"
	self assert: projects size equals: 2.
	self assert: project1 audit isEmpty.
	self assert: project2 audit isEmpty.
	self assert: (Rowan globalNamed: className) new foo equals: 2.
	self assert: (Rowan globalNamed: className) bar equals: 'bare'.
	self assert: project1 isDirty.	"we changed the project so it no longer matches disk"
	self deny: project2 isDirty.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue752_02b1 [
	"
		Load RowanSample9:spec_0005, then load RowanSample10:spec_0005 but remove 
			RowanSample9Class1>>foo and leave RowanSample9Class1 class>>bar to be 'stolen'.

		RowanSample9 should be clean and method should be owned by RowanSample10 project. Audit should be clean.
	"

	"https://github.com/GemTalk/Rowan/issues/752"

	| loadSpec1 loadSpec2 className projectName1 projectName2 projects project1 project2 
		projectSetDefinition projectDef |
	loadSpec1 := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0005.ston'.
	loadSpec2 := (RwRowanSample10Test _loadSpecNamed: 'spec_0005')
		gemstoneSetDefaultSymbolDictNameTo: 'RowanSample9_2';
		yourself.
	projectName1 := loadSpec1 projectName.
	projectName2 := loadSpec2 projectName.

  {projectName1 . projectName2 }
    do: [ :pn |
      (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
        ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	className := projectName1 , 'Class1'.

"load project1"
	projects := loadSpec1 resolveStrict load.
	project1 := projects projectNamed: projectName1.

"validate"
	self assert: projects size equals: 1.
	self assert: project1 audit isEmpty.
	self assert: (Rowan globalNamed: className) new foo equals: 1.
	self assert: (Rowan globalNamed: className) bar equals: 'bar'.
	self deny: project1 isDirty.

"load project2 and remove the methods RowanSample9Class1>>foo and RowanSample9Class1 class>>bar from project1"
	projectSetDefinition := (RwProjectSetDefinition new)
		addProject: loadSpec2 read;
		yourself.
	projectDef := project1 asDefinition.
	((projectDef packageNamed: 'RowanSample9-Core1')
		classDefinitionNamed: className)
			removeInstanceMethod: #foo;
			yourself.
	projectSetDefinition addProject: projectDef.
	projects := projectSetDefinition load.
	project2 := projects projectNamed: projectName2.

"validate"
	self assert: projects size equals: 2.
	self assert: project1 audit isEmpty.
	self assert: project2 audit isEmpty.
	self assert: (Rowan globalNamed: className) new foo equals: 2.
	self assert: (Rowan globalNamed: className) bar equals: 'bare'.
	self assert: project1 isDirty.	"we changed the project so it no longer matches disk"
	self deny: project2 isDirty.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue752_02b2 [
	"
		Load RowanSample9:spec_0005, then load RowanSample10:spec_0005 but remove 
			RowanSample9Class1 class>>bar and leave RowanSample9Class1>>foo to be 'stolen'.

		RowanSample9 should be clean and method should be owned by RowanSample10 project. Audit should be clean.
	"

	"https://github.com/GemTalk/Rowan/issues/752"

	| loadSpec1 loadSpec2 className projectName1 projectName2 projects project1 project2 
		projectSetDefinition projectDef |
	loadSpec1 := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0005.ston'.
	loadSpec2 := (RwRowanSample10Test _loadSpecNamed: 'spec_0005')
		gemstoneSetDefaultSymbolDictNameTo: 'RowanSample9_2';
		yourself.
	projectName1 := loadSpec1 projectName.
	projectName2 := loadSpec2 projectName.

  {projectName1 . projectName2 }
    do: [ :pn |
      (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
        ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	className := projectName1 , 'Class1'.

"load project1"
	projects := loadSpec1 resolveStrict load.
	project1 := projects projectNamed: projectName1.

"validate"
	self assert: projects size equals: 1.
	self assert: project1 audit isEmpty.
	self assert: (Rowan globalNamed: className) new foo equals: 1.
	self assert: (Rowan globalNamed: className) bar equals: 'bar'.
	self deny: project1 isDirty.

"load project2 and remove the methods RowanSample9Class1>>foo and RowanSample9Class1 class>>bar from project1"
	projectSetDefinition := (RwProjectSetDefinition new)
		addProject: loadSpec2 read;
		yourself.
	projectDef := project1 asDefinition.
	((projectDef packageNamed: 'RowanSample9-Core1')
		classDefinitionNamed: className)
			removeClassMethod: #bar;
			yourself.
	projectSetDefinition addProject: projectDef.
	projects := projectSetDefinition load.
	project2 := projects projectNamed: projectName2.

"validate"
	self assert: projects size equals: 2.
	self assert: project1 audit isEmpty.
	self assert: project2 audit isEmpty.
	self assert: (Rowan globalNamed: className) new foo equals: 2.
	self assert: (Rowan globalNamed: className) bar equals: 'bare'.
	self assert: project1 isDirty.	"we changed the project so it no longer matches disk"
	self deny: project2 isDirty.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue752_03 [
	"
		Load RowanSample9:spec_0005, then load RowanSample10:spec_0005 but RowanSample9 project is in project set for unrelated reason (new method #baz).

		RowanSample9 should be clean and method should be owned by RowanSample10 project. Audit should be clean.
	"

	"https://github.com/GemTalk/Rowan/issues/752"

	| loadSpec1 loadSpec2 className projectName1 projectName2 projects project1 project2 
		projectSetDefinition projectDef |
	loadSpec1 := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0005.ston'.
	loadSpec2 := (RwRowanSample10Test _loadSpecNamed: 'spec_0005')
		gemstoneSetDefaultSymbolDictNameTo: 'RowanSample9_2';
		yourself.
	projectName1 := loadSpec1 projectName.
	projectName2 := loadSpec2 projectName.

  {projectName1 . projectName2 }
    do: [ :pn |
      (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
        ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	className := projectName1 , 'Class1'.

"load project1"
	projects := loadSpec1 resolveStrict load.
	project1 := projects projectNamed: projectName1.

"validate"
	self assert: projects size equals: 1.
	self assert: project1 audit isEmpty.
	self assert: (Rowan globalNamed: className) new foo equals: 1.
	self assert: (Rowan globalNamed: className) bar equals: 'bar'.
	self deny: project1 isDirty.

"load project2 and add unrelated method RowanSample9Class1>>baz to project1"
	projectSetDefinition := (RwProjectSetDefinition new)
		addProject: loadSpec2 read;
		yourself.
	projectDef := project1 asDefinition.
	((projectDef packageNamed: 'RowanSample9-Core1')
		classDefinitionNamed: className)
			addInstanceMethod: 'baz ^1' protocol: 'accessing';
			yourself.
	projectSetDefinition addProject: projectDef.
	projects := projectSetDefinition load.
	project2 := projects projectNamed: projectName2.

"validate"
	self assert: projects size equals: 2.
	self assert: project1 audit isEmpty.
	self assert: project2 audit isEmpty.
	self assert: (Rowan globalNamed: className) new baz equals: 1.
	self assert: (Rowan globalNamed: className) new foo equals: 2.
	self assert: (Rowan globalNamed: className) bar equals: 'bare'.
	self assert: project1 isDirty.	"we changed the project so it no longer matches disk"
	self deny: project2 isDirty.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue752_04 [
	"
		Load RowanSample9:spec_0005, then load RowanSample10:spec_0005 the method
			RowanSample9Class1>>foo has a competing change in RowanSample9 project.

		The load should fail with an error.
	"

	"https://github.com/GemTalk/Rowan/issues/752"

	| loadSpec1 loadSpec2 className projectName1 projectName2 projects project1 exceptionSeen
		projectSetDefinition projectDef |
	loadSpec1 := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0005.ston'.
	loadSpec2 := (RwRowanSample10Test _loadSpecNamed: 'spec_0005')
		gemstoneSetDefaultSymbolDictNameTo: 'RowanSample9_2';
		yourself.
	projectName1 := loadSpec1 projectName.
	projectName2 := loadSpec2 projectName.

  {projectName1 . projectName2 }
    do: [ :pn |
      (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
        ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	className := projectName1 , 'Class1'.

"load project1"
	projects := loadSpec1 resolveStrict load.
	project1 := projects projectNamed: projectName1.

"validate"
	self assert: projects size equals: 1.
	self assert: project1 audit isEmpty.
	self assert: (Rowan globalNamed: className) new foo equals: 1.
	self assert: (Rowan globalNamed: className) bar equals: 'bar'.
	self deny: project1 isDirty.

"load project2 and make competing change to the method RowanSample9Class1>>foo in project1"
	projectSetDefinition := (RwProjectSetDefinition new)
		addProject: loadSpec2 read;
		yourself.
	projectDef := project1 asDefinition.
	((projectDef packageNamed: 'RowanSample9-Core1')
		classDefinitionNamed: className)
			updateInstanceMethod: 'foo  "competing change" ^3' protocol: 'accessing';
			yourself.
	projectSetDefinition addProject: projectDef.
	exceptionSeen := false.
	[ projectSetDefinition load ] 
		on: Error 
		do: [:ex |
			exceptionSeen := true.
			self assert: ex description equals: 'UserDefinedError: Incompatible changes to the same method (RowanSample9Class1>>foo ) in project ''RowanSample9'' and project ''RowanSample10''' ].
	self assert: exceptionSeen description: 'Expected an error'
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
	resolvedProject := loadSpec resolveProject.

"read project"
	definedProject := resolvedProject read.

	methodDef := ((definedProject packageNamed: projectName, '-Extensions')
		classExtensionDefinitionNamed: projectName, 'Class1') classMethodDefinitions at:#bar.
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
	resolvedProject := loadSpec resolveProject.

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
	self assert: stream contents equals: (self _expectedIssue766GsFileContents: projectName)
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue777_01 [
	"values in definitions structures should not be Symbols, 
		in v3.0, the keys in definition properties ARE Symbols
		as an optimization"

	"https://github.com/GemTalk/Rowan/issues/777"

	| loadSpec projectName resolvedProject |
	loadSpec := self _loadSpecNamed: 'spec_0002'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"validate"
	resolvedProject packages keysAndValuesDo: [:packageName :packageDef | 
		| methodDefs |
		methodDefs := Array new.
		self deny: (packageName isKindOf: Symbol).
		packageDef classDefinitions keysAndValuesDo: [:className :classDef |
			self deny: (className isKindOf: Symbol).
			classDef properties keysAndValuesDo: [:key :value | 
				self assert: (key isKindOf: Symbol).
				value _isArray
					ifTrue: [ value do: [:each | self deny: (each isKindOf: Symbol) ] ]
					ifFalse: [ self deny: (value isKindOf: Symbol) ] ].
			methodDefs 
				addAll: (classDef instanceMethodDefinitions values);
				addAll: (classDef classMethodDefinitions values) ].
		packageDef classExtensions keysAndValuesDo: [:className :classDef |
			self deny: (className isKindOf: Symbol).
			classDef properties keysAndValuesDo: [:key :value | 
				self assert: (key isKindOf: Symbol).
				value _isArray
					ifTrue: [ value do: [:each | self deny: (each isKindOf: Symbol) ] ]
					ifFalse: [ self deny: (value isKindOf: Symbol) ].
			methodDefs 
				addAll: (classDef instanceMethodDefinitions values);
				addAll: (classDef classMethodDefinitions values) ] ].
		methodDefs do: [:methodDef |
			methodDef properties keysAndValuesDo: [:key :value |
				self assert: (key isKindOf: Symbol).
				key = #'selector'
					ifTrue: [  self assert: (value isKindOf: Symbol) ]
					ifFalse: [ self deny: (value isKindOf: Symbol) ] ] ] ]
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue777_02 [
	"values in definitions structures should not be Symbols, 
		in v3.0, the keys in definition properties ARE Symbols
		as an optimization"

	"https://github.com/GemTalk/Rowan/issues/777"

	| loadSpec projectName resolvedProject |
	loadSpec := self _loadSpecNamed: 'spec_0080'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"validate"
	resolvedProject packages keysAndValuesDo: [:packageName :packageDef | 
		| methodDefs |
		methodDefs := Array new.
		self deny: (packageName isKindOf: Symbol).
		packageDef classDefinitions keysAndValuesDo: [:className :classDef |
			self deny: (className isKindOf: Symbol).
			classDef properties keysAndValuesDo: [:key :value | 
				self assert: (key isKindOf: Symbol).
				value _isArray
					ifTrue: [ value do: [:each | self deny: (each isKindOf: Symbol) ] ]
					ifFalse: [ self deny: (value isKindOf: Symbol) ] ].
			methodDefs 
				addAll: (classDef instanceMethodDefinitions values);
				addAll: (classDef classMethodDefinitions values) ].
		packageDef classExtensions keysAndValuesDo: [:className :classDef |
			self deny: (className isKindOf: Symbol).
			classDef properties keysAndValuesDo: [:key :value | 
				self assert: (key isKindOf: Symbol).
				value _isArray
					ifTrue: [ value do: [:each | self deny: (each isKindOf: Symbol) ] ]
					ifFalse: [ self deny: (value isKindOf: Symbol) ].
			methodDefs 
				addAll: (classDef instanceMethodDefinitions values);
				addAll: (classDef classMethodDefinitions values) ] ].
		methodDefs do: [:methodDef |
			methodDef properties keysAndValuesDo: [:key :value |
				self assert: (key isKindOf: Symbol).
				key = #'selector'
					ifTrue: [  self assert: (value isKindOf: Symbol) ]
					ifFalse: [ self deny: (value isKindOf: Symbol) ] ] ] ]
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue780 [
	"use spec_0061 to test compoent API "

	"https://github.com/GemTalk/Rowan/issues/780"

	| loadSpec projectName projectNames resolvedProject loadedProjects definedProject
		expectedComponentLabels |
	loadSpec := self _loadSpecNamed: 'spec_0061'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	definedProject := (loadedProjects projectNamed: projectName) defined.

"component API"
	self 
		assert: ((definedProject subcomponentsOf: 'Core') collect: [:each | each name]) sort
		equals: #( 'common/Core' 'common/Examples' 'common/Loader' 'common/Tests' 'groups/Examples' 'groups/Loader' 'groups/Projects' 'groups/Rowan' 'groups/Tests' 'groups/WorkGroup_dkh') sort.
	self 
		assert: ((definedProject subcomponentsOf: 'common/Core') collect: [:each | each name]) sort
		equals: #() sort.
	self 
		assert: ((definedProject subcomponentsOf: 'common/Loader') collect: [:each | each name]) sort
		equals: #('common/platforms/Loader') sort.
	self 
		assert: ((definedProject subcomponentsOf: 'common/platforms/Loader') collect: [:each | each name]) sort
		equals: #('common/platforms/gemstone/Loader' 'common/platforms/pharo/Loader') sort.
	self 
		assert: (definedProject subcomponentsOf: 'common/examples/Examples' ifNone: []) isNil.
	self
		assert: ((definedProject subcomponentsOf: 'common/Tests' attributes: #('common' 'tests') ifNone: [])  collect: [:each | each name])
		equals: #('common/tests/Tests') .
	self
		assert: (definedProject subcomponentsOf: 'common/Tests' attributes: #('common') ifNone: [#()])
		equals: #() .

"RowanClientServices component API use case"
	expectedComponentLabels := #( 'Core (common)' 'Examples (common)' 'Loader (common)' 'Tests (common)' '__ Examples (common)' '__ Loader (common)' '__ Projects (common)' '__ Rowan (common)' '__ Tests (common)' '__ WorkGroup_dkh (dkh)') sort.
	(definedProject subcomponentsOf: 'Core'
			ifNone: [ Array new ])
			collect: [ :subcomponent | 
				| component componentName label |
				componentName := subcomponent name.
				component := definedProject componentOrPackageGroupNamed: componentName.
				label := component label.
				self assert: (expectedComponentLabels includes: label)].

"package groups validation"
	self
		assert: (definedProject packageGroupNamed: 'groups/Examples') packageNames sort
		equals: { projectName, '-Examples'} sort.
	self
		assert: (definedProject packageGroupNamed: 'groups/Loader') packageNames sort
		equals: { projectName, '-Loader' . projectName, '-Loader-GemStone' . projectName, '-Loader-Pharo'} sort.
	self
		assert: (definedProject packageGroupNamed: 'groups/Projects') packageNames sort
		equals: { projectName, '-Core-Projects'} sort.
	self
		assert: (definedProject packageGroupNamed: 'groups/Rowan') packageNames sort
		equals: { projectName, '-Core-Rowan'} sort.
	self
		assert: (definedProject packageGroupNamed: 'groups/Tests') packageNames sort
		equals: { projectName, '-Tests'} sort.
	self
		assert: (definedProject packageGroupNamed: 'groups/WorkGroup_dkh') packageNames sort
		equals: { projectName, '-Core' . projectName, '-Core-Projects'} sort.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue781 [
	"use spec_0061 to test RowanClientServices component API use cases"

	"https://github.com/GemTalk/Rowan/issues/653"
	"https://github.com/GemTalk/Rowan/issues/781"

	| loadSpec projectName projectNames resolvedProject loadedProjects project
		expectedComponentNames expectedComponentLabels |
	loadSpec := self _loadSpecNamed: 'spec_0061'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"RowanComponentService>>computeSubComponentsUsingProjectService:"
	expectedComponentNames := #( 'common/Core' 'common/Examples' 'common/Loader' 'common/Tests' 'groups/Examples' 'groups/Loader' 'groups/Projects' 'groups/Rowan' 'groups/Tests' 'groups/WorkGroup_dkh') sort.
	expectedComponentLabels := #( 'Core (common)' 'Examples (common)' 'Loader (common)' 'Tests (common)' '__ Examples (common)' '__ Loader (common)' '__ Projects (common)' '__ Rowan (common)' '__ Tests (common)' '__ WorkGroup_dkh (dkh)') sort.
	self 
		assert: ((project loadedSubcomponentsOf: 'Core') collect: [:each | each name]) sort
		equals: expectedComponentNames.
	(project
			loadedSubcomponentsOf: 'Core'
			ifNone: [ Array new ])
			collect: [ :subcomponent | 
				| component componentName label |
				componentName := subcomponent name.
				component := project componentOrPackageGroupNamed: componentName.
				label := component label.
				self assert: (expectedComponentLabels includes: label)] .

"package groups validation"
	self
		assert: (project packageGroupNamed: 'groups/Examples') packageNames sort
		equals: { projectName, '-Examples'} sort.
	self
		assert: (project packageGroupNamed: 'groups/Loader') packageNames sort
		equals: { projectName, '-Loader' . projectName, '-Loader-GemStone' . projectName, '-Loader-Pharo'} sort.
	self
		assert: (project packageGroupNamed: 'groups/Projects') packageNames sort
		equals: { projectName, '-Core-Projects'} sort.
	self
		assert: (project packageGroupNamed: 'groups/Rowan') packageNames sort
		equals: { projectName, '-Core-Rowan'} sort.
	self
		assert: (project packageGroupNamed: 'groups/Tests') packageNames sort
		equals: { projectName, '-Tests'} sort.
	self
		assert: (project packageGroupNamed: 'groups/WorkGroup_dkh') packageNames sort
		equals: { projectName, '-Core' . projectName, '-Core-Projects'} sort.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue784 [
	"https://github.com/GemTalk/Rowan/issues/784"

	| loadSpec projectName projectNames resolvedProject loadedProjects definedProject |
	loadSpec := self _loadSpecNamed: 'spec_0061'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	definedProject := (loadedProjects projectNamed: projectName) defined.

"validate"
	self assert: definedProject componentNames sort equals: #('Core') sort.

"add component"
	definedProject addLoadComponentNamed: 'Def'.
	self assert: definedProject componentNames sort equals: #('Core' 'Def') sort.

"remove component"
	definedProject removeComponentNamed: 'Def'.
	self assert: definedProject componentNames sort equals: #('Core') sort.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue787 [
	"https://github.com/GemTalk/Rowan/issues/787"

	| loadSpec projectName projectNames resolvedProject loadedProjects definedProject |
	loadSpec := self _loadSpecNamed: 'spec_0061'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	definedProject := (loadedProjects projectNamed: projectName) defined.

"validate"
	self assert: definedProject packageGroupNames sort equals: #( 'groups/Examples' 'groups/Loader' 'groups/Projects' 'groups/Rowan' 'groups/Tests' 'groups/WorkGroup_dkh') sort.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue788 [
	"https://github.com/GemTalk/Rowan/issues/788"

	| loadSpec projectName projectNames resolvedProject loadedProjects definedProject packageGroup |
	loadSpec := self _loadSpecNamed: 'spec_0061'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	definedProject := (loadedProjects projectNamed: projectName) defined.

"validate"
	self assert: definedProject packageGroupNames sort equals: #( 'groups/Examples' 'groups/Loader' 'groups/Projects' 'groups/Rowan' 'groups/Tests' 'groups/WorkGroup_dkh') sort.

"add new package group"
	packageGroup := definedProject
		addPackageGroupNamed: 'groups/issue_788' condition: 'common' comment: ''.

"validate"
	self assert: definedProject packageGroupNames sort equals: #( 'groups/Examples' 'groups/Loader' 'groups/Projects' 'groups/Rowan' 'groups/Tests' 'groups/WorkGroup_dkh' 'groups/issue_788') sort.

"reload"
	loadedProjects := definedProject load.
	definedProject := (loadedProjects projectNamed: projectName) defined.

"validate"
	self assert: definedProject packageGroupNames sort equals: #( 'groups/Examples' 'groups/Loader' 'groups/Projects' 'groups/Rowan' 'groups/Tests' 'groups/WorkGroup_dkh' 'groups/issue_788') sort.

"remove new package group"
	definedProject removePackageGroupNamed: 'groups/issue_788'.

"validate"
	self assert: definedProject packageGroupNames sort equals: #( 'groups/Examples' 'groups/Loader' 'groups/Projects' 'groups/Rowan' 'groups/Tests' 'groups/WorkGroup_dkh') sort.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue789_01 [
	"https://github.com/GemTalk/Rowan/issues/789"

	| loadSpec projectName projectNames resolvedProject loadedProjects loadedProject definedProject1 definedProject2 packageGroup1 packageGroup2|
	loadSpec := self _loadSpecNamed: 'spec_0061'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	loadedProject := loadedProjects projectNamed: projectName.

	definedProject1 := loadedProject defined.
	packageGroup1 := definedProject1 packageGroupNamed: 'groups/Rowan'.

	definedProject2 := loadedProject defined.
	packageGroup2 := definedProject2 packageGroupNamed: 'groups/Rowan'.

"validate"
	self assert: definedProject1 ~~ definedProject2.
	self assert: definedProject1 _projectRepository ~~ definedProject2 _projectRepository.
	self assert: definedProject1 _projectSpecification ~~ definedProject2 _projectSpecification.
	self assert: definedProject1 _projectComponents ~~ definedProject2 _projectComponents.
	self assert: definedProject1 _projectDefinition ~~ definedProject2 _projectDefinition.
	self assert: packageGroup1 ~~ packageGroup2.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue789_02 [
	"https://github.com/GemTalk/Rowan/issues/789"

	| loadSpec projectName projectNames resolvedProject loadedProjects loadedProject definedProject1 definedProject2 component1 component2 conditionalPackageMapSpecs1 conditionalPackageMapSpecs2 |
	loadSpec := self _loadSpecNamed: 'spec_0009'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	loadedProject := loadedProjects projectNamed: projectName.

	definedProject1 := loadedProject defined.
	component1 := definedProject1 componentNamed: 'Core'.
	conditionalPackageMapSpecs1 := component1 conditionalPackageMapSpecs.

	definedProject2 := loadedProject defined.
	component2 := definedProject2 componentNamed: 'Core'.
	conditionalPackageMapSpecs2 := component2 conditionalPackageMapSpecs.

"validate"
	self assert: definedProject1 ~~ definedProject2.
	self assert: definedProject1 _projectRepository ~~ definedProject2 _projectRepository.
	self assert: definedProject1 _projectSpecification ~~ definedProject2 _projectSpecification.
	self assert: definedProject1 _projectComponents ~~ definedProject2 _projectComponents.
	self assert: definedProject1 _projectDefinition ~~ definedProject2 _projectDefinition.
	self assert: component1  ~~ component2 .
	self assert:  conditionalPackageMapSpecs1 ~~ conditionalPackageMapSpecs2.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue809_01 [
	"testing symbol dictionary package properties"

	"https://github.com/GemTalk/Rowan/issues/809"

	| loadSpec projectName projectNames resolvedProject loadedProjects loadedProject definedProject  |
	loadSpec := self _loadSpecNamed: 'spec_0004'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	loadedProject := loadedProjects projectNamed: projectName.
	definedProject := loadedProject defined.

"validate"
	self 
		assert: (loadedProject gemstoneSymbolDictNameForPackageNamed: projectName, '-Core2')
		equals: 'RowanSample9_3'.
	self 
		assert: (definedProject gemstoneSymbolDictNameForPackageNamed: projectName, '-Core2')
		equals: 'RowanSample9_3'.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue809_02 [
	"testing session methods package properties (subject to same issue as reported against symbol dictionary package properties"

	"https://github.com/GemTalk/Rowan/issues/809"

	| loadSpec projectName projectNames resolvedProject definedProject componentName component packageName |
	loadSpec := self _loadSpecNamed: 'spec_0004'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	definedProject := resolvedProject defined.
	packageName := projectName, '-New'.
	componentName := 'Core2'.

	definedProject addPackageNamed: packageName toComponentNamed: componentName.
	definedProject 
		gemstoneSetUseSessionMethodsForExtensionsForUser: 'SystemUser' 
			to: true
			forPackageNamed: projectName, '-Core2';
		gemstoneSetUseSessionMethodsForExtensionsForUser: 'DataCurator' 
			to: true
			forPackageNamed: packageName;
		gemstoneSetUseSessionMethodsForExtensionsForUser: RwLoadSpecificationV2 _gemstoneAllUsersName
			to: true
			forPackageNamed: packageName;
		yourself.
	component := definedProject componentNamed: componentName.

"validate"
	self 
		assert: (definedProject gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName forUser: 'Bozo')
		equals: true.	"existing package, unkown user ... should match allusers"
	self 
		assert: (definedProject gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName forUser: 'SystemUser')
		equals: true.	"existing package, no entry for SystemUser and this package"	
	self 
		assert: (definedProject  gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName forUser: 'DataCurator')
		equals: true.	"existing package, entry for DataCurator"
	self 
		assert: (definedProject gemstoneUseSessionMethodsForExtensionsForPackageNamed: projectName, '-Core2'  forUser: 'SystemUser')
		equals: true.	"existing package, entry for SystemUser"
	self 
		assert: (definedProject gemstoneUseSessionMethodsForExtensionsForPackageNamed: projectName, '-Core2X'  forUser: 'SystemUser')
		equals: (definedProject gemstoneDefaultUseSessionMethodsForExtensionsForUser: 'SystemUser').	"unknown package, should match default"
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue809_03 [
	"testing symbol dictionary package properties"

	"https://github.com/GemTalk/Rowan/issues/809"

	| loadSpec projectName projectNames resolvedProject definedProject componentName component packageName |
	loadSpec := self _loadSpecNamed: 'spec_0004'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	definedProject := resolvedProject defined.
	packageName := projectName, '-New'.
	componentName := 'Core2'.

	definedProject addPackageNamed: packageName toComponentNamed: componentName.
	definedProject 
		gemstoneSetSymbolDictNameForUser: 'SystemUser' 
			to: 'SystemUserDict' 
			forPackageNamed: projectName, '-Core2';
		gemstoneSetSymbolDictNameForUser: 'DataCurator' 
			to: 'DataCuratorDict' 
			forPackageNamed: packageName;
		gemstoneSetSymbolDictNameForUser: RwLoadSpecificationV2 _gemstoneAllUsersName
			to: 'AllUsersDict' 
			forPackageNamed: packageName;
		yourself.
	component := definedProject componentNamed: componentName.

"validate"
	self 
		assert: (definedProject gemstoneSymbolDictNameForPackageNamed: packageName forUser: 'Bozo')
		equals: 'AllUsersDict'.	"existing package, unkown user ... should match allusers"
	self 
		assert: (definedProject  gemstoneSymbolDictNameForPackageNamed: packageName forUser: 'SystemUser')
		equals: 'AllUsersDict'.	"existing package, no entry for SystemUser and this package"	
	self 
		assert: (definedProject  gemstoneSymbolDictNameForPackageNamed: packageName forUser: 'DataCurator')
		equals: 'DataCuratorDict'.	"existing package, entry for DataCurator"
	self 
		assert: (definedProject gemstoneSymbolDictNameForPackageNamed: projectName, '-Core2'  forUser: 'SystemUser')
		equals: 'SystemUserDict'.	"existing package, entry for SystemUser"
	self 
		assert: (definedProject gemstoneSymbolDictNameForPackageNamed: projectName, '-Core2X'  forUser: 'SystemUser')
		equals: (definedProject gemstoneDefaultSymbolDictNameForUser: 'SystemUser').	"unknown package, should match default"
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue822 [
	"https://github.com/GemTalk/Rowan/issues/822"

	| loadSpec projectName projectNames resolvedProject loadedProjects definedProject baselinePackageGroupNames |
	loadSpec := self _loadSpecNamed: 'spec_0061'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	definedProject := (loadedProjects projectNamed: projectName) defined.

"validate"
	baselinePackageGroupNames := #( 'groups/Examples' 'groups/Loader' 'groups/Projects' 'groups/Rowan' 'groups/Tests' 'groups/WorkGroup_dkh').
	self assert: definedProject packageGroupNames sort equals: baselinePackageGroupNames sort.

	definedProject addPackageGroupNamed: 'groups/XXX' condition: 'common' comment: 'hello'.

	self 
		assert: definedProject packageGroupNames sort 
		equals:( baselinePackageGroupNames, #( 'groups/XXX'  )) sort.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue838_01 [
	"load spec_0001 using project alias, clone/resolve spec_0002 using project alias, 
		switch git repository of spec_0001 project to point at at spec_0002 checkout,
		load project and repository should point at spec_0002"

	"https://github.com/dalehenrich/Rowan/issues/838"

	| loadSpec_0001 loadSpec_0002 loadedProjects projectNames projectsHome project
		loadedSpecs projectName resolvedSpecs newLoadedSpecs |
	loadSpec_0001 := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0001.ston'.
	loadSpec_0002 := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0002.ston'.
	projectsHome := '$ROWAN_PROJECTS_SANDBOX' asFileReference / 'isolated'.
	projectsHome ensureDeleteAll.

	loadSpec_0001
		projectsHome: projectsHome;
		projectAlias: loadSpec_0001 projectName, '0001';
		yourself.

	loadSpec_0002
		projectsHome: projectsHome;
		projectAlias: loadSpec_0002 projectName, '0002';
		yourself.
	projectNames := { loadSpec_0001 projectName . loadSpec_0002 projectName }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"load spec_0078"
	loadedProjects := loadSpec_0001	load.		"loaded project"
	resolvedSpecs := loadSpec_0002 resolve. 	"clone project"

"change spec_0001 to point at spec_0002 checkout"
	projectName := loadSpec_0001 projectName.
	project := loadedProjects projectNamed: projectName.
	loadedSpecs := project  loadedLoadSpecifications.
	(loadedSpecs specForProjectNamed: projectName)
		gitUrl: (resolvedSpecs specForProjectNamed: projectName) gitUrl.
	loadedProjects := loadedSpecs load.

"validate"
	self 
		assert: 
			(loadedProjects collect: [:proj | 
				proj gitUrl asRwUrl pathString]) 
		equals: 
			(Dictionary new 
				at: projectName put: (projectsHome / loadSpec_0002 projectAlias) pathString; 
				yourself).

	newLoadedSpecs := project  loadedLoadSpecifications.
	self 
		assert: 
			(newLoadedSpecs collect: [:ls | 
				ls gitUrl asRwUrl pathString]) 
		equals: 
			(Dictionary new 
				at: projectName put: (projectsHome / loadSpec_0002 projectAlias) pathString; 
				yourself)
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue838_02 [
	"load spec_0073 using project alias (0073A), clone/resolve spec_0073 using project alias (0073B), 
		use gitUrl: and projectsHome: to point spec_00073A project at
		spec_0073B checkout and load ... all repositories should point at 0073B"

	"https://github.com/dalehenrich/Rowan/issues/838"

	| loadSpec_0073A loadSpec_0073B loadedProjects projectsHome project loadedSpecs projectName resolvedSpecs 
		newLoadedSpecs expected0073A expected0073B |
	loadSpec_0073A := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0073.ston'.
	loadSpec_0073B := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0073.ston'.
	projectsHome := '$ROWAN_PROJECTS_SANDBOX' asFileReference / 'isolated'.
	projectsHome ensureDeleteAll.

	loadSpec_0073A
		projectsHome: projectsHome;
		projectAlias: loadSpec_0073A projectName, '0073A';
		yourself.

	loadSpec_0073B
		projectsHome: projectsHome;
		projectAlias: loadSpec_0073B projectName, '0073B';
		yourself.

	{ loadSpec_0073A projectName . loadSpec_0073B projectName }
		do: [:baseName |
			| projectNames |
			projectNames := { baseName . baseName, '_1' . baseName, '_2' .  baseName, '_3'}.
			projectNames do: [:pn | 
				(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
					ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ] ].

"clone spec_0073B"
	resolvedSpecs := loadSpec_0073B resolve. 	

"load spec_0078"
	loadedProjects := loadSpec_0073A	load.

"validate"
	projectName := loadSpec_0073A projectName.
	expected0073A :=  Dictionary new 
				at: projectName put: (projectsHome / loadSpec_0073A projectAlias) pathString; 
				at: projectName, '_1' put: (projectsHome / loadSpec_0073A projectAlias / 'rowan_1') pathString; 
				at: projectName, '_2' put: (projectsHome / loadSpec_0073A projectAlias / 'rowan_2') pathString; 
				at: projectName, '_3' put: (projectsHome / loadSpec_0073A projectAlias / 'rowan_3') pathString; 
				yourself.
	self 
		assert: 
			(loadedProjects collect: [:proj | 
				proj gitUrl asRwUrl pathString]) 
		equals: expected0073A.

"change spec_0073A to point at spec_0073B checkout"
	projectName := loadSpec_0073B projectName.
	project := loadedProjects projectNamed: projectName.
	loadedSpecs := project  loadedLoadSpecifications.
	loadedSpecs namesAndEntitiesDo: [:pName :ls |
		ls gitUrl: (resolvedSpecs specForProjectNamed: pName) gitUrl ].
"validate"
	expected0073B := Dictionary new 
				at: projectName put: (projectsHome / loadSpec_0073B projectAlias) pathString; 
				at: projectName, '_1' put: (projectsHome / loadSpec_0073B projectAlias / 'rowan_1') pathString; 
				at: projectName, '_2' put: (projectsHome / loadSpec_0073B projectAlias / 'rowan_2') pathString; 
				at: projectName, '_3' put: (projectsHome / loadSpec_0073B projectAlias / 'rowan_3') pathString; 
				yourself.
	self 
		assert: 
			(resolvedSpecs collect: [:ls | 
				ls gitUrl asRwUrl pathString]) 
		equals: expected0073B.
	self 
		assert: 
			(loadedSpecs collect: [:ls | 
				ls gitUrl asRwUrl pathString]) 
		equals: expected0073B.

"load"
	loadedProjects := loadedSpecs load.

"validate"
	self 
		assert: 
			(loadedProjects collect: [:proj | 
				proj gitUrl asRwUrl pathString]) 
		equals: expected0073B.
	newLoadedSpecs := project  loadedLoadSpecifications.
	self 
		assert: 
			(newLoadedSpecs collect: [:ls | 
				ls gitUrl asRwUrl pathString]) 
		equals: expected0073B
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue838_03 [
	"load spec_0073 using project alias (0073A), clone/resolve spec_0073 using project alias (0073B), 
		use gitRepository:projectsHome: to point one of the embedded projects at
		spec_0073B checkout and load ... validate ... load primary project .. validate ... 
		reload (reread all from disk) primary project ... validate"

	"https://github.com/dalehenrich/Rowan/issues/838"

	| loadSpec_0073A loadSpec_0073B loadedProjects projectsHome project loadedSpecs projectName resolvedSpecs 
		newLoadedSpecs expected0073A expected0073B oldLoadedProjects |
	loadSpec_0073A := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0073.ston'.
	loadSpec_0073B := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0073.ston'.
	projectsHome := '$ROWAN_PROJECTS_SANDBOX' asFileReference / 'isolated'.
	projectsHome ensureDeleteAll.

	loadSpec_0073A
		projectsHome: projectsHome;
		projectAlias: loadSpec_0073A projectName, '0073A';
		yourself.

	loadSpec_0073B
		projectsHome: projectsHome;
		projectAlias: loadSpec_0073B projectName, '0073B';
		yourself.

	{ loadSpec_0073A projectName . loadSpec_0073B projectName }
		do: [:baseName |
			| projectNames |
			projectNames := { baseName . baseName, '_1' . baseName, '_2' .  baseName, '_3'}.
			projectNames do: [:pn | 
				(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
					ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ] ].

"clone spec_0073B"
	resolvedSpecs := loadSpec_0073B resolve. 	

"load spec_0078"
	loadedProjects := loadSpec_0073A	load.

"validate"
	projectName := loadSpec_0073A projectName.
	expected0073A :=  Dictionary new 
				at: projectName put: (projectsHome / loadSpec_0073A projectAlias) pathString; 
				at: projectName, '_1' put: (projectsHome / loadSpec_0073A projectAlias / 'rowan_1') pathString; 
				at: projectName, '_2' put: (projectsHome / loadSpec_0073A projectAlias / 'rowan_2') pathString; 
				at: projectName, '_3' put: (projectsHome / loadSpec_0073A projectAlias / 'rowan_3') pathString; 
				yourself.
	self 
		assert: 
			(loadedProjects collect: [:proj | 
				proj gitUrl asRwUrl pathString]) 
		equals: expected0073A.

"change spec for only project RowanSampl90073A to point at spec_0073B checkout"
	projectName := loadSpec_0073B projectName.
	project := loadedProjects projectNamed: projectName.
	loadedSpecs := project  loadedLoadSpecifications.
	(loadedSpecs specForProjectNamed: projectName, '_2')
		gitUrl: (resolvedSpecs specForProjectNamed: projectName, '_2') gitUrl.

"validate"
	expected0073B := Dictionary new 
				at: projectName put: (projectsHome / loadSpec_0073A projectAlias) pathString; 
				at: projectName, '_1' put: (projectsHome / loadSpec_0073A projectAlias / 'rowan_1') pathString; 
				at: projectName, '_2' put: (projectsHome / loadSpec_0073B projectAlias / 'rowan_2') pathString; 
				at: projectName, '_3' put: (projectsHome / loadSpec_0073A projectAlias / 'rowan_3') pathString; 
				yourself.

	self 
		assert: 
			(loadedSpecs collect: [:ls | 
				ls gitUrl asRwUrl pathString]) 
		equals: expected0073B.
"
load"
	loadedProjects := loadedSpecs load.

"validate"
	self 
		assert: 
			(loadedProjects collect: [:proj | 
				proj gitUrl asRwUrl pathString]) 
		equals: expected0073B.
	newLoadedSpecs := project  loadedLoadSpecifications.
	self 
		assert: 
			(newLoadedSpecs collect: [:ls | 
				ls gitUrl asRwUrl pathString]) 
		equals: expected0073B.

"load primary project - only primary project loaded without affecting required projects"
	oldLoadedProjects := loadedProjects.
	loadedProjects := (loadedProjects projectNamed: projectName) defined read resolveProject loadAsDefined.

"validate"
	self assert: loadedProjects size = 1.
	self 
		assert: 
			(loadedProjects projectNamed: projectName) gitUrl asRwUrl pathString 
		equals: (expected0073A at: projectName).
	self 
		assert: 
			(oldLoadedProjects collect: [:proj | 
				proj gitUrl asRwUrl pathString]) 
		equals: expected0073B.

"reload primary project - reread all projects from disk (restore to original)"
	loadedProjects := (loadedProjects projectNamed: projectName) reload.

"validate"
	self 
		assert: 
			(loadedProjects collect: [:proj | 
				proj gitUrl asRwUrl pathString]) 
		equals: expected0073A
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue840_01 [
	"clone spec_0073 standard load for a project with embedded projects ... 
		confirms that standard load works correctly as prelude to doing a load 
		of an embedded project using a project alias"

	"https://github.com/dalehenrich/Rowan/issues/840"

	| loadSpec projectName projectNames projectsHome loadSpecs embeddedPrefix |
	loadSpec := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0073.ston'.
	'$ROWAN_PROJECTS_SANDBOX' asFileReference ensureDeleteAllChildren.
	projectsHome := '$ROWAN_PROJECTS_SANDBOX' asFileReference / 'isolated'.
	projectsHome ensureDeleteAll.

	projectName := loadSpec projectName.
	projectNames := {projectName. projectName, '_1' . projectName, '_2' .  projectName, '_3' . }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve projects - expected"
	loadSpecs := loadSpec
		projectsHome: projectsHome;
		resolve.

"validate"
	embeddedPrefix := (projectsHome / projectName) pathString.
	self 
		assert: 
			(loadSpecs collect: [:ls | 
				ls gitUrl asRwUrl pathString]) 
		equals: 
			(Dictionary new 
				at: projectName put: (projectsHome / loadSpec projectName) pathString; 
				at: projectName, '_1' put: embeddedPrefix, '/rowan_1'; 
				at: projectName, '_2' put: embeddedPrefix, '/rowan_2'; 
				at: projectName, '_3' put: embeddedPrefix, '/rowan_3'; 
				yourself)
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue840_02 [
	"clone spec_0073 using a project alias for the primary project ... alias should be propagated to all embedded projects"

	"https://github.com/dalehenrich/Rowan/issues/840"

	| loadSpec projectName projectNames projectsHome loadSpecs embeddedPrefix |
	loadSpec := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0073.ston'.
	'$ROWAN_PROJECTS_SANDBOX' asFileReference ensureDeleteAllChildren.
	projectsHome := '$ROWAN_PROJECTS_SANDBOX' asFileReference / 'isolated'.
	projectsHome ensureDeleteAll.

	projectName := loadSpec projectName.
	projectNames := {projectName. projectName, '_1' . projectName, '_2' .  projectName, '_3' . }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve projects - expected"
	loadSpecs := loadSpec
		projectsHome: projectsHome;
		projectAlias: projectName, '0073';
		resolve.

"validate"
	embeddedPrefix := (projectsHome / loadSpec projectAlias) pathString.
	self 
		assert: 
			(loadSpecs collect: [:ls | 
				ls gitUrl asRwUrl pathString]) 
		equals: 
			(Dictionary new 
				at: projectName put: (projectsHome / loadSpec projectAlias) pathString; 
				at: projectName, '_1' put: embeddedPrefix, '/rowan_1'; 
				at: projectName, '_2' put: embeddedPrefix, '/rowan_2'; 
				at: projectName, '_3' put: embeddedPrefix, '/rowan_3'; 
				yourself)
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue840_03 [
	"load spec_0073 using a project alias and validate that the gitRoot is correct for the embedded project"

	"https://github.com/dalehenrich/Rowan/issues/840"

	| loadSpec_0073A loadedProjects projectsHome project loadedSpecs projectName primary embedded embeddedProjectName embeddedPrefix |
	loadSpec_0073A := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0073.ston'.
	projectsHome := '$ROWAN_PROJECTS_SANDBOX' asFileReference / 'isolated'.
	projectsHome ensureDeleteAll.

	loadSpec_0073A
		projectsHome: projectsHome;
		projectAlias: loadSpec_0073A projectName, '0073A';
		yourself.

	{ loadSpec_0073A projectName }
		do: [:baseName |
			| projectNames |
			projectNames := { baseName . baseName, '_1' . baseName, '_2' .  baseName, '_3'}.
			projectNames do: [:pn | 
				(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
					ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ] ].

"load spec_0078A"
	loadedProjects := loadSpec_0073A load.		"loaded project"

"extract the loaded specs"
	projectName := loadSpec_0073A projectName.
	embeddedProjectName := loadSpec_0073A projectName, '_1'.
	project := loadedProjects projectNamed: projectName.
	loadedSpecs := project  loadedLoadSpecifications.

"validate primary load spec gitUrl"
	self 
		assert: (primary := (loadedProjects projectNamed: projectName) gitRoot pathString)
		equals:  (embedded := (loadedSpecs specForProjectNamed: projectName) gitUrl asRwUrl pathString).

"validate gitRoot for primary and embedded"
	self 
		assert: (primary := (loadedProjects projectNamed: projectName) gitRoot pathString)
		equals: (embedded := (loadedProjects projectNamed: embeddedProjectName) gitRoot pathString).

"validate embedded load spec gitUrl"
	embeddedPrefix := (projectsHome / loadSpec_0073A projectAlias) pathString.
	self 
		assert: 
			(loadedSpecs collect: [:ls | 
				ls gitUrl asRwUrl pathString]) 
		equals: 
			(Dictionary new 
				at: projectName put: (projectsHome / loadSpec_0073A projectAlias) pathString; 
				at: projectName, '_1' put: embeddedPrefix, '/rowan_1'; 
				at: projectName, '_2' put: embeddedPrefix, '/rowan_2'; 
				at: projectName, '_3' put: embeddedPrefix, '/rowan_3'; 
				yourself).
false ifTrue: [ 
	projectName := 'RowanSample9_1'.
	self 
		assert: (loadedProjects projectNamed: embeddedProjectName) gitRoot pathString
		equals: (loadedSpecs specForProjectNamed: projectName) gitUrl asRwUrl pathString.
 ]
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
	resolvedProject := loadSpec resolveProject.

"load project soec_008"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

"spec_009"
	loadSpec := self _loadSpecNamed: 'spec_0009'.
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec1 resolveProject.

"now set the diskUrl for loadeSpec2 to the repository root of loadSpec1"
	loadSpec2 := loadSpec1 copy.
	loadSpec2 
		gitUrl: nil;
		diskUrl: 'file:', resolvedProject repositoryRoot pathString.

	loadSpec2 resolveProject.
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
	project := loadedProjects projectNamed: projectName.
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
	resolvedProject := loadSpec resolveProject.
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
	resolvedProject := loadSpec resolveProject.
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.

"unload project"
	(Rowan projectNamed: projectName) unload
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0000 [
	"should red from disk without error"

	|  specUrl loadSpecification resolvedProject |
	specUrl := 'https://raw.githubusercontent.com/dalehenrich/' , self class _rowanSample9ProjectName
				, '/master/specs/spec_0000.ston'.
	loadSpecification := (RwSpecification fromUrl: specUrl)
		projectsHome: self _testRowanProjectsSandbox;
		yourself.

	(self _testRowanProjectsSandbox / loadSpecification projectName) ensureDeleteAll.

	resolvedProject := loadSpecification resolveProject. "project is read from disk on resolve"
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0001_A [
	"a UserDefinedError occurred (error 2318), reason:halt, The component 'Core' is undefined"

	"functional variant of testSpec_0001_B"

	| rowanProject specUrl loadSpecification resolvedProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrl := 'https://raw.githubusercontent.com/dalehenrich/' , self class _rowanSample9ProjectName
				, '/master/specs/spec_0000.ston'.
	loadSpecification := (RwSpecification fromUrl: specUrl)
		projectsHome: self _testRowanProjectsSandbox;
		yourself.

	(self _testRowanProjectsSandbox / loadSpecification projectName) ensureDeleteAll.

	resolvedProject := loadSpecification resolveProject. "project is read from disk on resolve"
	resolvedProject
		addPackagesNamed: {(loadSpecification projectName , '-Core')}
		toComponentNamed: 'Core'.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0001_B [
	"a UserDefinedError occurred (error 2318), reason:halt, The component 'Core' is undefined"

	"testSpec_0001_A behavies correctly"

	| rowanProject specUrl loadSpecification resolvedProject resolvedRepository oldPolicy |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrl := 'https://raw.githubusercontent.com/dalehenrich/' , self class _rowanSample9ProjectName
				, '/master/specs/spec_0000.ston'.
	loadSpecification := (RwSpecification fromUrl: specUrl)
		projectsHome: self _testRowanProjectsSandbox;
		repositoryResolutionPolicy: #'strict';
		yourself.	"force #strict policy to ensure that the revision is checkout out in the repository"

	(self _testRowanProjectsSandbox / loadSpecification projectName) ensureDeleteAll.


	resolvedProject := loadSpecification resolveProject.

	resolvedRepository := resolvedProject repository.
	resolvedRepository checkout: 'master'.

	oldPolicy := loadSpecification repositoryResolutionPolicy.
	[ 
	"force #strict policy to ensure that the revision is checkout out in the repolicy"
	loadSpecification repositoryResolutionPolicy: #'strict'.
	resolvedProject := loadSpecification resolveProject ]
		ensure: [ loadSpecification repositoryResolutionPolicy: oldPolicy ].

	resolvedProject
		addPackagesNamed: {(loadSpecification projectName , '-Core')}
		toComponentNamed: 'Core'
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0001_C [
	"load with empty component names in load spec ... should end up with no packages"

	| loadSpec projectName resolvedProject loadedProjects requiredLoadSpecs project |
	loadSpec := self _loadSpecNamed: 'spec_0001'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
	self assert: (Rowan globalNamed: projectName, 'Class1') notNil.
	self assert: (Rowan globalNamed: projectName, 'TestCase') notNil.
	self 
		assert: project packageNames asArray sort 
		equals: {projectName, '-Core' . projectName, '-Tests'} sort.

"remove Core component and reload"
	requiredLoadSpecs := (loadedProjects projectNamed: projectName) loadedLoadSpecifications.
	(requiredLoadSpecs specForProjectNamed: projectName) removeComponentNames: {'Core'}.

"load"
	requiredLoadSpecs load.

"validate"
	self assert: (Rowan globalNamed: projectName, 'Class1') isNil.
	self assert: (Rowan globalNamed: projectName, 'TestCase') isNil.
	self assert: project packageNames isEmpty.
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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

"load primer project"
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects projectNamed: projectName.
	self assert: project audit isEmpty.
	self _runTestSuiteForProject: project.

	loadSpec := self _loadSpecNamed: 'spec_0003'.	"primer -- spec_0001 with instancesInvariant"

"resolve"
	resolvedProject := loadSpec resolveProject.
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
	project := loadedProjects projectNamed: projectName.
	self _runTestSuiteForProject: project.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0003 [
	"spec_0003 should load cleanly"


	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0003'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0004 [
	"spec_0004 should load cleanly"


	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0004'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0005 [
	"spec_0005 should load cleanly"


	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0005'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0006 [
	"spec_0006 should load cleanly"


	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0006'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0007 [
	"spec_0007 should load cleanly"


	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0007'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects
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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

"load project -- expect error"
	errorHit := false.
	[ loadedProjects := resolvedProject load ] 
		on: Error 
		do: [:ex | 
			self assert: ex description equals: ('UserDefinedError: The extension class named ''', projectName, 'Class1'' cannot be found in this symbol dictionary (#''RowanSample9_3''). Packages that extend a class must be loaded into the symbol dictionary in which the class is installed.').
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
	resolvedProject := loadSpec resolveProject.

"load project -- expect error"
	errorHit := false.
	[ loadedProjects := resolvedProject load ] 
		on: Error 
		do: [:ex | 
			self assert: ex description equals: ('UserDefinedError: The extension class named ''', projectName, 'Class1'' cannot be found in this symbol dictionary (#''RowanSample9_1''). Packages that extend a class must be loaded into the symbol dictionary in which the class is installed.').
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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.	"load project"
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
	resolvedProject := loadSpec resolveProject.	"load project"
	loadedProjects := resolvedProject load.	"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects.
	self
		_validateComponent: (resolvedProject componentNamed: 'Core')
			componentNames:
				{'subs/Core1'.
				'subs/Core2'.
				'tests/Tests'}
			packageNames: {projectName, '-Core'}
			projectNames: {};
		_validateComponent: (resolvedProject componentNamed: 'tests/Tests')
			componentNames: {}
			packageNames: {projectName, '-Tests'}
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
	resolvedProject := loadSpec resolveProject.	"load project"
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
	resolvedProject := loadSpec resolveProject.

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

	| loadSpec projectName resolvedProject loadedProjects loadFailed |
	loadSpec := self _loadSpecNamed: 'spec_0023'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadFailed := false.
	[loadedProjects := resolvedProject load ] 
		on: Error
		do: [:ex |
			self assert: ex description equals: 'a CompileError occurred (error 1001), undefined symbol  ', projectName, 'Class3'.
			loadFailed := true ].
	self assert: loadFailed
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
		resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects
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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"remove tests from the conditional attributes and resolve -- RowanSample9TestCase1 should not be loaded"
	loadSpec customConditionalAttributes: #().
	resolvedProject := loadSpec resolveProject.

"load project"
	self assert: (Rowan globalNamed: projectName, 'TestCase1') notNil.
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self assert: (Rowan globalNamed: projectName, 'TestCase1') isNil.
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
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"remove tests from the conditional attributes and resolve -- RowanSample9TestCase1 should not be loaded"
	loadSpec customConditionalAttributes: #().
	resolvedProject := loadSpec resolveProject.

"load project"
	{projectName, 'TestCase1' . projectName, 'TestCase2' . projectName, 'TestCase3'} do: [:className |
		self assert: (Rowan globalNamed: className) notNil ].

	loadedProjects := resolvedProject load.

	{projectName, 'TestCase1' . projectName, 'TestCase2' . projectName, 'TestCase3'} do: [:className |
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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	{ projectName, 'SpecsSharedClass4' . projectName, 'Specsv1Class5' . projectName, 'Specsv2Class5'} do: [:className |
		self assert: (Rowan globalNamed: className) notNil ].

	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"remove v1 from the conditional attributes and resolve -- RowanSample9Specsv1Class5 should not be loaded"
	loadSpec customConditionalAttributes remove: 'v1'.
	resolvedProject := loadSpec resolveProject.
"load project"
	loadedProjects := resolvedProject load.
"validate"
	{ projectName, 'Specsv1Class5' }do: [:className |
		self assert: (Rowan globalNamed: className) isNil ].
	{ projectName, 'SpecsSharedClass4' . projectName, 'Specsv2Class5'} do: [:className |
		self assert: (Rowan globalNamed: className) notNil ].
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"remove v2 from the conditional attributes, add v1 back  and resolve -- RowanSample9Specsv1Class5 should not be loaded"
	loadSpec customConditionalAttributes remove: 'v2'.
	loadSpec customConditionalAttributes add: 'v1'.
	resolvedProject := loadSpec resolveProject.
"load project"
	loadedProjects := resolvedProject load.
"validate"
	{ projectName, 'Specsv2Class5' } do: [:className |
		self assert: (Rowan globalNamed: className) isNil ].
	{ projectName, 'SpecsSharedClass4' . projectName, 'Specsv1Class5'} do: [:className |
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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects  projectNamed: projectName.
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
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects projectNamed: projectName.
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
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects projectNamed: projectName.
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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	project := loadedProjects projectNamed: projectName.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
		{  Rowan platformConditionalAttributes . #( 'tests') }.
		{ Rowan platformConditionalAttributes . #( 'tests' 'alt1' ) }.
		{ Rowan platformConditionalAttributes . #( 'tests' 'alt2' ) }.
		{ Rowan platformConditionalAttributes . #( 'tests' 'alt3' ) }.
	} do: [:ar |
		| platformAttributes conditionalAttributes |
		platformAttributes := ar at: 1.
		conditionalAttributes := ar at: 2.
		resolvedProject := loadSpec resolve: conditionalAttributes platformAttributes: platformAttributes.
		loadedProjects := resolvedProject load.
		self
			_standard_validate: resolvedProject
			loadedProjects: loadedProjects
			expectedProjectNames: projectNames.
		conditionalAttributes size > 1
			ifTrue: [
				"sanity check in case of massive load failure"
				self assert: ((Rowan globalNamed: projectName, 'Class1') selectors includes: (conditionalAttributes at: 2) asSymbol) ] ].
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
		{ Rowan platformConditionalAttributes . #('tests' 'alt1' 'platformA') }.
		{ Rowan platformConditionalAttributes . #('tests' 'alt1' 'platformB') }.
		{ Rowan platformConditionalAttributes . #('tests' 'alt1' 'platformC') }.
	} do: [:ar |
		| platformAttributes conditionalAttributes |
		platformAttributes := ar at: 1.
		conditionalAttributes := ar at: 2.
		resolvedProject := loadSpec resolve: conditionalAttributes platformAttributes: platformAttributes.
		loadedProjects := resolvedProject load.
		self
			_standard_validate: resolvedProject
			loadedProjects: loadedProjects
			expectedProjectNames: projectNames.
		(conditionalAttributes at: 3) = 'platformC'
			ifTrue: [
				"sanity check in case of massive load failure"
				self assert: ((Rowan globalNamed: projectName, 'TestCase') selectors includes: #testPlatformC) ]
			ifFalse: [
				"sanity check in case of massive load failure"
				self assert: ((Rowan globalNamed: projectName, 'Class1') selectors includes: (conditionalAttributes at: 3) asSymbol) ]].
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
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
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
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames_1.

"checkout spec_0060 and reload"
	project checkout: 'spec_0060'.

"reload project"
	loadedProjects := project reload. "can afford to take required projects into account"

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames_2.
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0061 [
	"spec_0061 should load cleanly"

	"package groups example"

	| loadSpec projectName projectNames resolvedProject loadedProjects project |
	loadSpec := self _loadSpecNamed: 'spec_0061'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self _validate_spec_0061: project
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0062 [
	"spec_0062 should load cleanly -- component structure is different than spec_0061, but using same set of packages. "

	"package groups example"

	| loadSpec projectName projectNames resolvedProject loadedProjects project |
	loadSpec := self _loadSpecNamed: 'spec_0062'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self _validate_spec_0061: project
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0063 [
	"spec_0063 should load cleanly -- component structure is different than spec_0061 and spec_0062, but using same set of packages. "

	"package groups example"

	| loadSpec projectName projectNames resolvedProject loadedProjects project |
	loadSpec := self _loadSpecNamed: 'spec_0063'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self _validate_spec_0063: project
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0064_1 [
	"spec_0064 should load cleanly, including RowanSample10"

	"https://github.com/GemTalk/Rowan/issues/680"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName className1 |
	loadSpec := self _loadSpecNamed: 'spec_0064'.

	projectName := loadSpec projectName.
	requiredProjectName :=  'RowanSample10'.
	projectNames := {projectName}.
	className1 := requiredProjectName , 'Class1'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	(self _testRowanProjectsSandbox / requiredProjectName) ensureDeleteAll.

"resolve project"
	resolvedProject := loadSpec resolveProject.

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
	requiredProjectName :=  'RowanSample10'.
	projectNames := {projectName}.
	className1 := requiredProjectName , 'Class1'.

	{projectName . requiredProjectName} do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	(self _testRowanProjectsSandbox / requiredProjectName) ensureDeleteAll.

"resolve project"
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	requiredProjectName :=  'RowanSample10'.
	projectNames := {projectName}.
	className1 := requiredProjectName , 'Class1'.

	{projectName . requiredProjectName} do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	(self _testRowanProjectsSandbox / requiredProjectName) ensureDeleteAll.

"change default symbol dictionary"
	loadSpec gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2.

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := (loadedProjects projectNamed: projectName) defined.

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
	resolvedProject := loadSpec resolveProject.

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
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0064_4 [
	"Extended test case for #680 ... basically testSpec_0064_3 with NO new class version"

	"https://github.com/GemTalk/Rowan/issues/680"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName className1 project packageName |
	loadSpec := self _loadSpecNamed: 'spec_0064'.

	projectName := loadSpec projectName.
	requiredProjectName :=  'RowanSample10'.
	projectNames := {projectName}.
	className1 := requiredProjectName , 'Class1'.

	{projectName . requiredProjectName} do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	(self _testRowanProjectsSandbox / requiredProjectName) ensureDeleteAll.

"change default symbol dictionary"
	loadSpec gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName2.

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := (loadedProjects projectNamed: projectName) defined.

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
	resolvedProject := loadSpec resolveProject.

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
]

{ #category : 'tests' }
RwRowanSample9Test >> testSpec_0064_5 [
	"Extended test case for #680 ... basically testSpec_0064_3 with ONLY new class version (no method changes and no symbol dictionary change)"

	"https://github.com/GemTalk/Rowan/issues/680"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName className1 project packageName |
	loadSpec := self _loadSpecNamed: 'spec_0064'.

	projectName := loadSpec projectName.
	requiredProjectName :=  'RowanSample10'.
	projectNames := {projectName}.
	className1 := requiredProjectName , 'Class1'.

	{projectName . requiredProjectName} do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	(self _testRowanProjectsSandbox / requiredProjectName) ensureDeleteAll.

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := (loadedProjects projectNamed: projectName) defined.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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
	resolvedProject := loadSpec resolveProject.

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

	| loadSpec_0002 loadSpec_0003 projectName projectNames resolvedSpecs_0002 resolvedSpecs_0003 resolvedSpec_0002 resolvedSpec_0003 project loadedProjects commitId loadedSpecs |
	loadSpec_0002 := (self _loadSpecNamed: 'spec_0002')
		projectAlias: 'RowanSample9_0002';
		yourself.
	loadSpec_0003 := (self _loadSpecNamed: 'spec_0003')
		projectAlias: 'RowanSample9_0003';
		yourself.

	projectName := loadSpec_0002 projectName.
	self assert: projectName equals: loadSpec_0003 projectName.
	projectNames := { projectName }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	{'RowanSample9_0002' . 'RowanSample9_0003'} 
		do: [:repoName | 
			(self _testRowanProjectsSandbox / repoName) ensureDeleteAll ].

"resolve specs (clone)"
	resolvedSpecs_0002 := loadSpec_0002 resolve.
	resolvedSpecs_0003 := loadSpec_0003 resolve.

"load spec_0002"
	loadedProjects := resolvedSpecs_0002 load.
	project := Rowan projectNamed: projectName.

"validate"
	self _standard_validate_loadedProjects: loadedProjects.

"validate"
	resolvedSpec_0002 := resolvedSpecs_0002 specForProjectNamed: projectName.
	resolvedSpec_0003 := resolvedSpecs_0003 specForProjectNamed: projectName.

	self deny: resolvedSpec_0002 gitUrl = resolvedSpec_0003 gitUrl.

"load project from spec_0003 checkout: changing gitUrl of loaded project"
	loadedSpecs := project loadedLoadSpecifications.
	(loadedSpecs specForProjectNamed: projectName)
		gitUrl: resolvedSpec_0003 gitUrl.
	loadedProjects := loadedSpecs load.

"Validate"
	self _standard_validate_loadedProjects: loadedProjects.
	self assert: project asDefinition repositoryRoot = project repositoryRoot.
	self assert: resolvedSpec_0003 gitUrl equals: project gitUrl.
	self deny: resolvedSpec_0002 gitUrl = resolvedSpec_0003 gitUrl.
	self assert: (Rowan globalNamed: projectName, 'Class1') bar equals: 'bar'.	"bar method not present in spec_0001"
	self assert: project loadSpecification gitUrl equals: resolvedSpec_0003 gitUrl.

"reload project1 from spec_0003 checkout: using readOnlyRepository:commitId: to define root of loaded project"
	commitId := project commitId.
	project asReadOnlyRepositoryWithCommitId: commitId.
	self assert: project commitId equals: commitId.

"reload"
	loadedProjects := project reload.

"Validate"
	self _standard_validate_loadedProjects: loadedProjects.
	self assert: project asDefinition repositoryRoot = project repositoryRoot.
	self assert: resolvedSpec_0003 gitUrl equals: project readOnlyDiskUrl.
	self deny: resolvedSpec_0002 gitUrl = resolvedSpec_0003 gitUrl.
	self assert: (Rowan globalNamed: projectName, 'Class1') bar equals: 'bar'.	"bar method not present in spec_0001"
	self deny: project commitId equals: commitId.	"commitId is not expected to survive reload"

"reload project1 from spec_0003 checkout: using diskUrl: to define root of loaded project"
	loadedSpecs := project loadedLoadSpecifications.
	(loadedSpecs specForProjectNamed: project name)
		diskUrl: resolvedSpec_0003 gitUrl.
	loadedProjects := loadedSpecs load.

"Validate"
	self _standard_validate_loadedProjects: loadedProjects.
	self assert: project asDefinition repositoryRoot = project repositoryRoot.
	self assert: resolvedSpec_0003 gitUrl equals: project diskUrl.
	self deny: resolvedSpec_0002 gitUrl = resolvedSpec_0003 diskUrl.
	self assert: (Rowan globalNamed: projectName, 'Class1') bar equals: 'bar'.	"bar method not present in spec_0001"
	self assert: project loadSpecification diskUrl equals: resolvedSpec_0003 gitUrl.
]

{ #category : 'tests' }
RwRowanSample9Test >> testUnmanagedSpec_0057_01 [
	"based on spec_0057 after loading, do a filein (with removeAllMethods) and run unmanaged 
		report and an audit. When we remove methods, we remove the methods and compile new
		methods bypassing Rowan (non-standard filein format), creating audit errors and ending up 
		with unpackaged methods"

	| loadSpec projectName loadedProjects project audit packageName1 packageName2 className1 className2 |
	loadSpec := self _loadSpecNamed: 'spec_0057'.
	projectName := loadSpec projectName.

	packageName1 := projectName, '-Tests'.
	packageName2 := projectName, '-Core'.
	className1 := projectName, 'TestCase'.
	className2 := projectName, 'Class1'.

	loadedProjects := self 
		_testUnmanagedSpec_0057: loadSpec 
		useStandardMethodFilein: true
		removeAllMethods: true
		packageName1: packageName1 
		packageName2: packageName2 
		className1: className1 
		className2: className2.

"validate"
	project := loadedProjects projectNamed: projectName.
	self _runTestSuiteForProject: project.
	audit := project audit.
	self assert: audit isEmpty. "empty audit"

"run reports"
	{
	(Rowan gemstoneTools topaz
		unmanagedClassesAndMethodsReportForSymbolDictionaries: Rowan image symbolList) .
	(Rowan gemstoneTools topaz
		unmanagedClassesAndMethodsReportForSymbolDictionary:
			(Rowan image
				symbolDictNamed: (project gemstoneSymbolDictNameForPackageNamed: packageName1))) .
	(Rowan gemstoneTools topaz
		unmanagedClassesAndMethodsReportForClass: (Rowan globalNamed: className1)) .
	}
		do: [ :details | 
			details
				unmanagedMethodsDo: [
				"no unpackaged methods"
				self assert: false details: 'Unexpected unmanaged class or methods' ] ]
]

{ #category : 'tests' }
RwRowanSample9Test >> testUnmanagedSpec_0057_02 [
	"based on spec_0057 after loading, do a filein (withOUT removeAllMethods) and run unmanaged report and an audit"

	| loadSpec projectName loadedProjects project audit packageName1 packageName2 className1 className2 classAudit |
	loadSpec := self _loadSpecNamed: 'spec_0057'.
	projectName := loadSpec projectName.

	packageName1 := projectName, '-Tests'.
	packageName2 := projectName, '-Core'.
	className1 := projectName, 'TestCase'.
	className2 := projectName, 'Class1'.

	loadedProjects := self 
		_testUnmanagedSpec_0057: loadSpec 
		useStandardMethodFilein: false "force unpackaged methods"
		removeAllMethods: true
		packageName1: packageName1 
		packageName2: packageName2 
		className1: className1 
		className2: className2.

"validate"
	project := loadedProjects projectNamed: projectName.
	self _runTestSuiteForProject: project.
	audit := project audit.

	classAudit := ((audit at: packageName2) at: className2)
		collect: [ :each | each message ].
	self assert: classAudit size = 2.
	self assert: (classAudit includes: 'Missing loaded method: ', className2, '>>foo').
	self assert: (classAudit includes: 'Missing loaded method: ', className2, ' class>>bar').

	classAudit := ((audit at: packageName1) at: className1)
		collect: [ :each | each message ].
	self assert: classAudit size = 1.
	self assert: (classAudit includes: 'Missing loaded method: ', className1, '>>test').
]

{ #category : 'tests' }
RwRowanSample9Test >> testUnmanagedSpec_0057_03 [
	"based on spec_0057 after loading, do a filein (with removeAllMethods) and run unmanaged 
		report and an audit. When we remove methods, we remove the methods and compile new
		methods bypassing Rowan (non-standard filein format), creating audit errors and ending up 
		with unpackaged methods"

	| loadSpec projectName loadedProjects project audit packageName1 packageName2 className1 className2 |
	loadSpec := self _loadSpecNamed: 'spec_0057'.
	projectName := loadSpec projectName.

	packageName1 := projectName, '-Tests'.
	packageName2 := projectName, '-Core'.
	className1 := projectName, 'TestCase'.
	className2 := projectName, 'Class1'.

	loadedProjects := self 
		_testUnmanagedSpec_0057: loadSpec 
		useStandardMethodFilein: true
		removeAllMethods: false
		packageName1: packageName1 
		packageName2: packageName2 
		className1: className1 
		className2: className2.

"validate"
	project := loadedProjects projectNamed: projectName.
	self _runTestSuiteForProject: project.
	audit := project audit.
	self assert: audit isEmpty. "empty audit"

"run reports"
	{
	(Rowan gemstoneTools topaz
		unmanagedClassesAndMethodsReportForSymbolDictionaries: Rowan image symbolList) .
	(Rowan gemstoneTools topaz
		unmanagedClassesAndMethodsReportForSymbolDictionary:
			(Rowan image
				symbolDictNamed: (project gemstoneSymbolDictNameForPackageNamed: packageName1))) .
	(Rowan gemstoneTools topaz
		unmanagedClassesAndMethodsReportForClass: (Rowan globalNamed: className1)) .
	}
		do: [ :details | 
			details
				unmanagedMethodsDo: [ 
				"no unpackaged methods"
				self assert: false details: 'Unexpected unmanaged class or methods' ] ]
]

{ #category : 'tests' }
RwRowanSample9Test >> testUnmanagedSpec_0057_04 [
	"based on spec_0057 after loading, do a filein leaving the original packaged methods intact, but forcing unpackaged methods to be created by bypassing the Rowan methods"

	| loadSpec projectName loadedProjects project audit packageName1 packageName2 className1 className2 classAudit |
	loadSpec := self _loadSpecNamed: 'spec_0057'.
	projectName := loadSpec projectName.

	packageName1 := projectName, '-Tests'.
	packageName2 := projectName, '-Core'.
	className1 := projectName, 'TestCase'.
	className2 := projectName, 'Class1'.

	loadedProjects := self 
		_testUnmanagedSpec_0057: loadSpec 
		useStandardMethodFilein: false
		removeAllMethods: false
		packageName1: packageName1 
		packageName2: packageName2 
		className1: className1 
		className2: className2.

"validate"
	project := loadedProjects projectNamed: projectName.
	self _runTestSuiteForProject: project.
	audit := project audit.

	classAudit := ((audit at: packageName2) at: className2)
		collect: [ :each | each message ].
	self assert: classAudit size = 3.
	self assert: (classAudit includes: 'Missing loaded method: ', className2, '>>foo').
	self assert: (classAudit includes: 'Missing loaded method: ', className2, ' class>>bar').
	self assert: (classAudit includes: 'Compiled method is not identical to loaded method: ', className2, '>>foo').

	classAudit := ((audit at: packageName1) at: className1)
		collect: [ :each | each message ].
	self assert: classAudit size = 2.
	self assert: (classAudit includes: 'Missing loaded method: ', className1, '>>test').
	self assert: (classAudit includes: 'Compiled method is not identical to loaded method: ', className1, '>>test').
]
