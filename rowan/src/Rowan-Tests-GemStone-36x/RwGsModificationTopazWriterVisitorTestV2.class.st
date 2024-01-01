Class {
	#name : 'RwGsModificationTopazWriterVisitorTestV2',
	#superclass : 'RwProjectReaderWriterTest',
	#category : 'Rowan-Tests-GemStone-36x'
}

{ #category : 'private' }
RwGsModificationTopazWriterVisitorTestV2 >> _expectedGsModificationTopazWriterFileContents_A1 [

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class Declarations
! Generated file, do not Edit

doit
(Array
	subclass: ''Issue361Class2''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core'';
		comment: ''this is a comment'';
		immediateInvariant.
true.
', percent, '

doit
(Object
	subclass: ''Issue361Class1''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core'';
		immediateInvariant.
true.
', percent, '

doit
(Issue361Class1
	subclass: ''Issue361Class3''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core'';
		immediateInvariant.
true.
', percent, '

! Class implementation for ''Issue361Class2''

!		Instance methods for ''Issue361Class2''

category: ''accessing''
method: Issue361Class2
method3 ^3
', percent, '

! Class implementation for ''Issue361Class1''

!		Class methods for ''Issue361Class1''

category: ''accessing''
classmethod: Issue361Class1
method2 ^2
', percent, '

! Class implementation for ''Issue361Class3''

!		Class methods for ''Issue361Class3''

category: ''accessing''
classmethod: Issue361Class3
method4 ^4
', percent, '

! Class extensions for ''Issue361Class1''

!		Instance methods for ''Issue361Class1''

category: ''*issue361-extension1''
method: Issue361Class1
method1 ^1
', percent, '

! Class extensions for ''Issue361Class2''

!		Instance methods for ''Issue361Class2''

category: ''*issue361-extension2''
method: Issue361Class2
method5 ^5
', percent, '

'
]

{ #category : 'private' }
RwGsModificationTopazWriterVisitorTestV2 >> _expectedGsModificationTopazWriterFileContents_A2 [

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class Declarations
! Generated file, do not Edit

doit
(Array
	subclass: ''Issue361Class2''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core'';
		comment: ''this is a comment'';
		immediateInvariant.
true.
', percent, '

removeallmethods Issue361Class2
removeallclassmethods Issue361Class2

doit
(Object
	subclass: ''Issue361Class1''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core'';
		immediateInvariant.
true.
', percent, '

removeallmethods Issue361Class1
removeallclassmethods Issue361Class1

doit
(Issue361Class1
	subclass: ''Issue361Class3''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core'';
		immediateInvariant.
true.
', percent, '

removeallmethods Issue361Class3
removeallclassmethods Issue361Class3

! Class implementation for ''Issue361Class2''

!		Instance methods for ''Issue361Class2''

category: ''accessing''
method: Issue361Class2
method3 ^3
', percent, '

! Class implementation for ''Issue361Class1''

!		Class methods for ''Issue361Class1''

category: ''accessing''
classmethod: Issue361Class1
method2 ^2
', percent, '

! Class implementation for ''Issue361Class3''

!		Class methods for ''Issue361Class3''

category: ''accessing''
classmethod: Issue361Class3
method4 ^4
', percent, '

! Class extensions for ''Issue361Class1''

!		Instance methods for ''Issue361Class1''

category: ''*issue361-extension1''
method: Issue361Class1
method1 ^1
', percent, '

! Class extensions for ''Issue361Class2''

!		Instance methods for ''Issue361Class2''

category: ''*issue361-extension2''
method: Issue361Class2
method5 ^5
', percent, '

'
]

{ #category : 'private' }
RwGsModificationTopazWriterVisitorTestV2 >> _expectedGsTopazWriterFileContents_B1_Issue361Core [

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class Declarations
! Generated file, do not Edit

doit
(Array
	subclass: ''Issue361Class2''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core'';
		comment: ''this is a comment'';
		immediateInvariant.
true.
', percent, '

doit
(Object
	subclass: ''Issue361Class1''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core'';
		immediateInvariant.
true.
', percent, '

doit
(Issue361Class1
	subclass: ''Issue361Class3''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core'';
		immediateInvariant.
true.
', percent, '

! Class implementation for ''Issue361Class2''

!		Instance methods for ''Issue361Class2''

category: ''accessing''
method: Issue361Class2
method3 ^3
', percent, '

! Class implementation for ''Issue361Class1''

!		Class methods for ''Issue361Class1''

category: ''accessing''
classmethod: Issue361Class1
method2 ^2
', percent, '

! Class implementation for ''Issue361Class3''

!		Class methods for ''Issue361Class3''

category: ''accessing''
classmethod: Issue361Class3
method4 ^4
', percent, '

'
]

{ #category : 'private' }
RwGsModificationTopazWriterVisitorTestV2 >> _expectedGsTopazWriterFileContents_B1_Issue361Extension1 [

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class extensions for ''Issue361Class1''

!		Instance methods for ''Issue361Class1''

category: ''*issue361-extension1''
method: Issue361Class1
method1 ^1
', percent, '

'
]

{ #category : 'private' }
RwGsModificationTopazWriterVisitorTestV2 >> _expectedGsTopazWriterFileContents_B1_Issue361Extension2 [

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class extensions for ''Issue361Class2''

!		Instance methods for ''Issue361Class2''

category: ''*issue361-extension2''
method: Issue361Class2
method5 ^5
', percent, '

'
]

{ #category : 'private' }
RwGsModificationTopazWriterVisitorTestV2 >> _expectedGsTopazWriterFileContents_B3_Issue361Core [
	^ self _expectedGsTopazWriterFileContents_B1_Issue361Core
]

{ #category : 'private' }
RwGsModificationTopazWriterVisitorTestV2 >> _expectedGsTopazWriterFileContents_B3_Issue361Extension1 [

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class extensions for ''Issue361Class1''

!		Instance methods for ''Issue361Class1''

removeallmethods Issue361Class1
removeallclassmethods Issue361Class1

category: ''*issue361-extension1''
method: Issue361Class1
method1 ^1
', percent, '

'
]

{ #category : 'private' }
RwGsModificationTopazWriterVisitorTestV2 >> _expectedGsTopazWriterFileContents_B3_Issue361Extension2 [

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class extensions for ''Issue361Class2''

!		Instance methods for ''Issue361Class2''

removeallmethods Issue361Class2
removeallclassmethods Issue361Class2

category: ''*issue361-extension2''
method: Issue361Class2
method5 ^5
', percent, '

'
]

{ #category : 'private' }
RwGsModificationTopazWriterVisitorTestV2 >> _standardProjectSetDefinition: projectsHome [
	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition 
		className1 className2 className3 classExtensionDefinition projectSetDefinition 
		packageName3 componentName |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 

	(projectsHome / projectName) ensureDeleteAll.

"create definitions"
	componentName := 'Core'.
	projectDefinition := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addNewComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 . packageName3 }
			toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName 
			forPackageNamed: packageName1;
		gemstoneSetSymbolDictName: self _symbolDictionaryName 
			forPackageNamed: packageName2;
		resolve;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName1.

	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2').
	packageDefinition addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: 'Array'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: 'this is a comment'
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3').
	packageDefinition addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className3
		super: className1
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method4'
					protocol: 'accessing'
					source: 'method4 ^4').
	packageDefinition addClassDefinition: classDefinition.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: '*', packageName2 asLowercase
					source: 'method1 ^1').
	packageDefinition := projectDefinition packageNamed: packageName2.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	packageDefinition := projectDefinition packageNamed: packageName3.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className2.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: '*', packageName3 asLowercase
					source: 'method5 ^5').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

	^ projectSetDefinition
]

{ #category : 'running' }
RwGsModificationTopazWriterVisitorTestV2 >> setUp [
	super setUp.
	GsModificationTopazWriterVisitor stopTrackingClassNames
]

{ #category : 'running' }
RwGsModificationTopazWriterVisitorTestV2 >> tearDown [
	GsModificationTopazWriterVisitor stopTrackingClassNames.
	super tearDown
]

{ #category : 'tests' }
RwGsModificationTopazWriterVisitorTestV2 >> testGsModificationTopazWriter_A1 [
	"straight fileout with no removeall*methods"

	| projectName packageName1 packageName2 className1 className2 className3 projectSetDefinition projectSetModification visitor repositoryRootPath x stream y packageName3 projectsHome |
	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'.
	className2 := 'Issue361Class2'.
	className3 := 'Issue361Class3'.
	projectsHome := self _testRowanProjectsSandbox.

	projectSetDefinition := self _standardProjectSetDefinition: projectsHome.

	repositoryRootPath := projectsHome / projectName / 'topaz'.
	self _markForCleanup: projectsHome / projectName.
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition
		compareAgainstBase: RwProjectSetDefinition new.
	visitor := GsModificationTopazWriterVisitor new
		repositoryRootPath: repositoryRootPath;
		topazFilename: projectName;
		yourself.
	visitor visit: projectSetModification.

	stream := ZnBufferedReadStream on: visitor topazFileReference readStream.
	x := stream contents.
	y := self _expectedGsModificationTopazWriterFileContents_A1.
	self assert: x = y
]

{ #category : 'tests' }
RwGsModificationTopazWriterVisitorTestV2 >> testGsModificationTopazWriter_A2 [
	"fileout includes removeall*methods after class definitions"

	| projectName packageName1 packageName2 className1 className2 className3 projectSetDefinition projectSetModification visitor repositoryRootPath x stream y packageName3 projectsHome |
	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'.
	className2 := 'Issue361Class2'.
	className3 := 'Issue361Class3'.
	projectsHome := self _testRowanProjectsSandbox.

	projectSetDefinition := self _standardProjectSetDefinition: projectsHome.

	repositoryRootPath := projectsHome / projectName / 'topaz'.
	self _markForCleanup: projectsHome / projectName.
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition
		compareAgainstBase: RwProjectSetDefinition new.
	[ 
	GsModificationTopazWriterVisitor startTrackingClassNames.
	visitor := GsModificationTopazWriterVisitor new
		repositoryRootPath: repositoryRootPath;
		topazFilename: projectName;
		yourself.
	visitor visit: projectSetModification ]
		ensure: [ GsModificationTopazWriterVisitor stopTrackingClassNames ].

	stream := ZnBufferedReadStream on: visitor topazFileReference readStream.
	x := stream contents.
	y := self _expectedGsModificationTopazWriterFileContents_A2.
	self assert: x = y
]

{ #category : 'tests' }
RwGsModificationTopazWriterVisitorTestV2 >> testGsModificationTopazWriter_A3 [
	"fileout excludes removeall*methods after class definitions, using excludeRemoveAllMethods: overriding class tracking"

	| projectName packageName1 packageName2 className1 className2 className3 projectSetDefinition projectSetModification visitor repositoryRootPath x stream y packageName3 projectsHome |
	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'.
	className2 := 'Issue361Class2'.
	className3 := 'Issue361Class3'.
	projectsHome := self _testRowanProjectsSandbox.

	projectSetDefinition := self _standardProjectSetDefinition: projectsHome.

	repositoryRootPath := projectsHome / projectName / 'topaz'.
	self _markForCleanup: projectsHome / projectName.
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition
		compareAgainstBase: RwProjectSetDefinition new.
	[ 
	GsModificationTopazWriterVisitor startTrackingClassNames.
	visitor := GsModificationTopazWriterVisitor new
		excludeRemoveAllMethods: true;
		repositoryRootPath: repositoryRootPath;
		topazFilename: projectName;
		yourself.
	visitor visit: projectSetModification ]
		ensure: [ GsModificationTopazWriterVisitor stopTrackingClassNames ].

	stream := ZnBufferedReadStream on: visitor topazFileReference readStream.
	x := stream contents.
	y := self _expectedGsModificationTopazWriterFileContents_A1.
	self assert: x = y
]

{ #category : 'tests' }
RwGsModificationTopazWriterVisitorTestV2 >> testGsModificationTopazWriter_B1 [
	"fileout each package in a separate file with no removeall*methods, with a single GsModificationTopazWriterVisitor instance"

	| projectName packageName1 packageName2 className1 className2 className3 projectSetDefinition projectSetModification visitor repositoryRootPath x stream y packageName3 projectsHome |
	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'.
	className2 := 'Issue361Class2'.
	className3 := 'Issue361Class3'.
	projectsHome := self _testRowanProjectsSandbox.

	projectSetDefinition := self _standardProjectSetDefinition: projectsHome.

	repositoryRootPath := projectsHome / projectName / 'topaz'.
	self _markForCleanup: projectsHome / projectName.
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition
		compareAgainstBase: RwProjectSetDefinition new.
	visitor := GsModificationTopazWriterVisitor new
		repositoryRootPath: repositoryRootPath;
		topazFilenamePackageNamesMap:
				(Dictionary new
						at: packageName1 put: {packageName1};
						at: packageName2 put: {packageName2};
						at: packageName3 put: {packageName3};
						yourself) yourself.
	visitor visit: projectSetModification.

	{packageName1.
	packageName2.
	packageName3}
		do: [ :packageName | 
			stream := ZnBufferedReadStream
				on:
					(repositoryRootPath / packageName , visitor filenameExtension) readStream.
			x := stream contents.
			y := self
				perform:
					#'_expectedGsTopazWriterFileContents_B1_' , (packageName copyWithout: $-).
			self assert: x = y ]
]

{ #category : 'tests' }
RwGsModificationTopazWriterVisitorTestV2 >> testGsModificationTopazWriter_B2 [
	"fileout each package in a separate file with no removeall*methods, with a GsModificationTopazWriterVisitor instance for each package"

	| projectName packageName1 packageName2 className1 className2 className3 projectSetDefinition projectSetModification visitor repositoryRootPath x stream y packageName3 projectsHome package1 package2 package3 projectDefinition |
	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'.
	className2 := 'Issue361Class2'.
	className3 := 'Issue361Class3'.
	projectsHome := self _testRowanProjectsSandbox.

	projectSetDefinition := self _standardProjectSetDefinition: projectsHome.

	repositoryRootPath := projectsHome / projectName / 'topaz'.
	self _markForCleanup: projectsHome / projectName.
	repositoryRootPath ensureCreateDirectory.

	projectDefinition := (projectSetDefinition projectNamed: projectName)
		_projectDefinition.
	package1 := projectDefinition packages removeKey: packageName1.
	package2 := projectDefinition packages removeKey: packageName2.
	package3 := projectDefinition packages removeKey: packageName3.
	{package1.
	package2.
	package3}
		do: [ :package | 
			visitor := GsModificationTopazWriterVisitor new
				repositoryRootPath: repositoryRootPath;
				topazFilename: package name.
			projectDefinition _addPackage: package.
			projectSetModification := projectSetDefinition
				compareAgainstBase: RwProjectSetDefinition new.
			visitor visit: projectSetModification.
			projectDefinition removePackageNamed: package name ].

	{packageName1.
	packageName2.
	packageName3}
		do: [ :packageName | 
			stream := ZnBufferedReadStream
				on:
					(repositoryRootPath / packageName , visitor filenameExtension) readStream.
			x := stream contents.
			y := self
				perform:
					#'_expectedGsTopazWriterFileContents_B1_' , (packageName copyWithout: $-).
			self assert: x = y ]
]

{ #category : 'tests' }
RwGsModificationTopazWriterVisitorTestV2 >> testGsModificationTopazWriter_B3 [
	"fileout each package in a separate file with removeall*methods for one extension method package"

	| projectName packageName1 packageName2 className1 className2 className3 projectSetDefinition projectSetModification visitor repositoryRootPath x stream y packageName3 projectsHome package1 package2 package3 projectDefinition |
	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'.
	className2 := 'Issue361Class2'.
	className3 := 'Issue361Class3'.
	projectsHome := self _testRowanProjectsSandbox.

	projectSetDefinition := self _standardProjectSetDefinition: projectsHome.

	repositoryRootPath := projectsHome / projectName / 'topaz'.
	self _markForCleanup: projectsHome / projectName.
	repositoryRootPath ensureCreateDirectory.

	projectDefinition := (projectSetDefinition projectNamed: projectName)
		_projectDefinition.
	package1 := projectDefinition packages removeKey: packageName1.
	package2 := projectDefinition packages removeKey: packageName2.
	package3 := projectDefinition packages removeKey: packageName3.
	{package1.
	package2.
	package3}
		do: [ :package | 
			visitor := GsModificationTopazWriterVisitor new
				repositoryRootPath: repositoryRootPath;
				topazFilename: package name.
			projectDefinition _addPackage: package.
			projectSetModification := projectSetDefinition
				compareAgainstBase: RwProjectSetDefinition new.
			visitor visit: projectSetModification.
			projectDefinition removePackageNamed: package name.
			GsModificationTopazWriterVisitor isTrackingClassNames
				ifFalse: [ GsModificationTopazWriterVisitor startTrackingClassNames ] ].
	GsModificationTopazWriterVisitor stopTrackingClassNames.

	{packageName1.
	packageName2.
	packageName3}
		do: [ :packageName | 
			stream := ZnBufferedReadStream
				on:
					(repositoryRootPath / packageName , visitor filenameExtension) readStream.
			x := stream contents.
			y := self
				perform:
					#'_expectedGsTopazWriterFileContents_B3_' , (packageName copyWithout: $-).
			self assert: x = y ]
]
