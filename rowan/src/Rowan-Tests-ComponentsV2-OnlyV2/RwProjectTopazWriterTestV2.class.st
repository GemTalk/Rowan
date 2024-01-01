Class {
	#name : 'RwProjectTopazWriterTestV2',
	#superclass : 'RwProjectReaderWriterTest',
	#category : 'Rowan-Tests-ComponentsV2-OnlyV2'
}

{ #category : 'private' }
RwProjectTopazWriterTestV2 >> _expectedGsTopazWriterFileContents_A [

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
	options: #( #logCreation )
)
		category: ''Issue361-Core'';
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
	options: #( #logCreation )
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
	options: #( #logCreation )
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

category: ''*issue361-extension''
method: Issue361Class1
method1 ^1
', percent, '

'
]

{ #category : 'private' }
RwProjectTopazWriterTestV2 >> _expectedGsTopazWriterFileContents_A_export [

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
	options: #( #logCreation )
)
		category: ''Issue361-Core'';
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
	options: #( #logCreation )
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
	options: #( #logCreation )
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

category: ''*issue361-extension''
method: Issue361Class1
method1 ^1
', percent, '

'
]

{ #category : 'private' }
RwProjectTopazWriterTestV2 >> _expectedGsTopazWriterFileContents_B [

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

category: ''*issue361-extension2''
method: Issue361Class1
method5 ^5
', percent, '

'
]

{ #category : 'private' }
RwProjectTopazWriterTestV2 >> _expectedGsTopazWriterFileContents_C_Issue361Core [

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
RwProjectTopazWriterTestV2 >> _expectedGsTopazWriterFileContents_C_Issue361Core_export [

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

'
]

{ #category : 'private' }
RwProjectTopazWriterTestV2 >> _expectedGsTopazWriterFileContents_C_Issue361Extension1 [

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
RwProjectTopazWriterTestV2 >> _expectedGsTopazWriterFileContents_C_Issue361Extension1_export [
	^ self _expectedGsTopazWriterFileContents_C_Issue361Extension1
]

{ #category : 'private' }
RwProjectTopazWriterTestV2 >> _expectedGsTopazWriterFileContents_C_Issue361Extension2 [

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class extensions for ''Issue361Class1''

!		Instance methods for ''Issue361Class1''

category: ''*issue361-extension2''
method: Issue361Class1
method5 ^5
', percent, '

'
]

{ #category : 'private' }
RwProjectTopazWriterTestV2 >> _expectedGsTopazWriterFileContents_C_Issue361Extension2_export [
	^ self _expectedGsTopazWriterFileContents_C_Issue361Extension2
]

{ #category : 'tests' }
RwProjectTopazWriterTestV2 >> testGsTopazWriter_A [

	"simple case with only two packages and a single set of extensions for a class"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition 
		className1 className2 className3 classExtensionDefinition projectSetDefinition 
		projectSetModification visitor repositoryRootPath x stream y componentName projectsHome |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 
	projectsHome := self _testRowanProjectsSandbox.

	(projectsHome / projectName) ensureDeleteAll.

"create definitions"
	componentName := 'Core'.
	projectDefinition := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addNewComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 }
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
		comment: ''
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

"diff"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

"write"
	repositoryRootPath := projectsHome / projectName / 'topaz'.
	self _markForCleanup: (projectsHome / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitorV2 new
		excludeRemoveAllMethods: true;
		logCreation: true;
		repositoryRootPath: repositoryRootPath;
		topazFilename: projectName;
		yourself.
	visitor visit: projectSetModification.

	stream := ZnBufferedReadStream on: visitor topazFileReference readStream.
	x := stream contents.
	y := self _expectedGsTopazWriterFileContents_A.
	self assert: x = y
]

{ #category : 'tests' }
RwProjectTopazWriterTestV2 >> testGsTopazWriter_A_export [

	"simple case with only two packages and a single set of extensions for a class"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition 
		className1 className2 className3 topazFileReference classExtensionDefinition 
		repositoryRootPath x stream y componentName projectsHome |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 
	projectsHome := self _testRowanProjectsSandbox.

	(projectsHome / projectName) ensureDeleteAll.

"create definitions"
	componentName := 'Core'.
	projectDefinition := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addNewComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 }
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
		comment: ''
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

"export"
	repositoryRootPath := projectsHome / projectName / 'topaz'.
	topazFileReference := repositoryRootPath asFileReference / projectName, 'gs'.
	self _markForCleanup: (projectsHome / projectName).
	repositoryRootPath ensureCreateDirectory.
	projectDefinition 
		exportTopazFormatTo: topazFileReference
 		logClassCreation: true
		excludeClassInitializers: false
		excludeRemoveAllMethods: false.

"validate"
	stream := ZnBufferedReadStream on: topazFileReference readStream.
	x := stream contents.
	y := self _expectedGsTopazWriterFileContents_A_export.
	self assert: x = y
]

{ #category : 'tests' }
RwProjectTopazWriterTestV2 >> testGsTopazWriter_B [

	"ramp up to multiple class extensions from multiple packages for multiple classes"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition 
		className1 className2 className3 classExtensionDefinition projectSetDefinition 
		projectSetModification visitor repositoryRootPath x stream y packageName3 componentName
		projectsHome |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 
	projectsHome := self _testRowanProjectsSandbox.

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

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: '*', packageName3 asLowercase
					source: 'method5 ^5').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

"diff"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

"write"
	repositoryRootPath := projectsHome / projectName / 'topaz'.
	self _markForCleanup: (projectsHome / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitorV2 new
		excludeRemoveAllMethods: true;
		repositoryRootPath: repositoryRootPath;
		topazFilename: projectName;
		yourself.
	visitor visit: projectSetModification.

	stream := ZnBufferedReadStream on: visitor topazFileReference readStream.
	x := stream contents.
	y := self _expectedGsTopazWriterFileContents_B.
	self assert: x = y
]

{ #category : 'tests' }
RwProjectTopazWriterTestV2 >> testGsTopazWriter_C [

	"ramp up to multiple class extensions from multiple packages for multiple classes"

	"write each package into a separate topaz file"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition 
		className1 className2 className3 classExtensionDefinition projectSetDefinition 
		projectSetModification visitor repositoryRootPath x stream y packageName3 componentName 
		projectsHome |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 
	projectsHome := self _testRowanProjectsSandbox.

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
		comment: ''
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

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: '*', packageName3 asLowercase
					source: 'method5 ^5').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

"diff"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

"write"
	repositoryRootPath := projectsHome / projectName / 'topaz'.
	self _markForCleanup: (projectsHome / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitorV2 new
		excludeRemoveAllMethods: true;
		repositoryRootPath: repositoryRootPath;
		topazFilenamePackageNamesMap: 
			(Dictionary new
				at: packageName1 put: {packageName1};
				at: packageName2 put: {packageName2};
				at: packageName3 put: {packageName3};
				yourself)
		yourself.
	visitor visit: projectSetModification.

	{ packageName1. packageName2. packageName3 }
		do: [:packageName |
			stream := ZnBufferedReadStream on: (repositoryRootPath / packageName, visitor filenameExtension) readStream.
			x := stream contents.
			y := self perform: #'_expectedGsTopazWriterFileContents_C_', (packageName copyWithout: $-).
			self assert: x = y ].
]

{ #category : 'tests' }
RwProjectTopazWriterTestV2 >> testGsTopazWriter_C_export [

	"ramp up to multiple class extensions from multiple packages for multiple classes"

	"write each package into a separate topaz file"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition 
		className1 className2 className3 topazFileReference classExtensionDefinition repositoryRootPath 
		x stream y packageName3 componentName projectsHome |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 
	projectsHome := self _testRowanProjectsSandbox.

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
		comment: ''
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

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: '*', packageName3 asLowercase
					source: 'method5 ^5').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

"export"
	repositoryRootPath := projectsHome / projectName / 'topaz'.
	topazFileReference := repositoryRootPath asFileReference / projectName.
	self _markForCleanup: (projectsHome / projectName).
	repositoryRootPath ensureCreateDirectory.
	projectDefinition 
		exportTopazFormatTo: topazFileReference
			logClassCreation: false
			excludeClassInitializers: false
			excludeRemoveAllMethods: false
			usingPackageNamesMap: (Dictionary new
				at: packageName1 put: {packageName1};
				at: packageName2 put: {packageName2};
				at: packageName3 put: {packageName3};
				yourself).

"validate"

	{ packageName1. packageName2. packageName3 }
		do: [:packageName |
			stream := ZnBufferedReadStream on: (repositoryRootPath / packageName, 'gs') readStream.
			x := stream contents.
			y := self perform: #'_expectedGsTopazWriterFileContents_C_', (packageName copyWithout: $-), '_export'.
			self assert: x = y ].
]

{ #category : 'tests' }
RwProjectTopazWriterTestV2 >> testGsTopazWriter_D [

	"ramp up to multiple class extensions from multiple packages for multiple classes"

	"write each package into a separate topaz file"

	"include class initialization methods"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition 
		className1 className2 className3 classExtensionDefinition projectSetDefinition 
		projectSetModification visitor repositoryRootPath x stream y packageName3 componentName
		projectsHome |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 
	projectsHome := self _testRowanProjectsSandbox.

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
					source: 'method2 ^2');
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'initialize'
					protocol: 'initialization'
					source: 'initialize "noop"').
	packageDefinition addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: 'Array'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
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

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: '*', packageName3 asLowercase
					source: 'method5 ^5').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className2.
	classExtensionDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'initialize'
					protocol: '*', packageName3 asLowercase
					source: 'initialize "another noop"').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.


"diff"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

"write"
	repositoryRootPath := projectsHome / projectName / 'topaz'.
	self _markForCleanup: (projectsHome / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitorV2 new
		excludeRemoveAllMethods: true;
		repositoryRootPath: repositoryRootPath;
		topazFilenamePackageNamesMap: 
			(Dictionary new
				at: packageName1 put: {packageName1};
				at: packageName2 put: {packageName2};
				at: packageName3 put: {packageName3};
				yourself)
		yourself.
	visitor visit: projectSetModification.

	{ packageName1. packageName2. packageName3 }
		do: [:packageName |
			stream := ZnBufferedReadStream on: (repositoryRootPath / packageName, visitor filenameExtension) readStream.
			x := stream contents.
			y := self perform: #'_expectedGsTopazWriterFileContents_D_', (packageName copyWithout: $-).
			self assert: x = y ].
]

{ #category : 'tests' }
RwProjectTopazWriterTestV2 >> testGsTopazWriter_error [

	"try to write to multiple files when no configs are available"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition 
		className1 className2 className3 classExtensionDefinition projectSetDefinition 
		projectSetModification visitor repositoryRootPath packageName3 componentName projectsHome |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 
	projectsHome := self _testRowanProjectsSandbox.

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
		comment: ''
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

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: '*', packageName3 asLowercase
					source: 'method5 ^5').
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

"diff"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

"write"
	repositoryRootPath := projectsHome / projectName / 'topaz'.
	self _markForCleanup: (projectsHome / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitorV2 new
				repositoryRootPath: repositoryRootPath;
				topazFilenameComponentMap: (Dictionary new
						at: 'rowan_bootstrap_app' put: {{'Kernel'}. {}};
						at: 'rowan_bootstrap_globals' put: {{'Globals'}. {}};
						at: 'rowan_bootstrap_user_globals' put: {{'UserGlobals'}. {}};
						yourself);
				yourself.
	self should: [ visitor visit: projectSetModification ] raise: Error
]

{ #category : 'tests' }
RwProjectTopazWriterTestV2 >> testGsTopazWriter_MultiProject_A [

	"ramp up to multiple class extensions from multiple packages and multiple projects"

	"write a single topaz file"

	"include class initialization methods"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition 
		className1 className2 className3 classExtensionDefinition projectSetDefinition 
		projectSetModification visitor repositoryRootPath x stream y packageName3 componentName
		projectsHome |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 
	componentName := 'Core'.
	projectsHome := self _testRowanProjectsSandbox.

	(projectsHome / projectName) ensureDeleteAll.

	projectSetDefinition := RwProjectSetDefinition new.

	{1 . 2 } do: [:projectIndex |
		| projectExt |
		projectExt := '_', projectIndex asString.

"create definitions"
		projectDefinition := RwResolvedProjectV2 new
			projectName: projectName, projectExt;
			projectsHome: projectsHome;
			gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
			addNewComponentNamed: componentName;
			addPackagesNamed: { 
				packageName1, projectExt . 
				packageName2, projectExt . 
				packageName3, projectExt}
				toComponentNamed: componentName;
			gemstoneSetSymbolDictName: self _symbolDictionaryName 
				forPackageNamed: packageName1, projectExt;
			gemstoneSetSymbolDictName: self _symbolDictionaryName 
				forPackageNamed: packageName2, projectExt;
			resolve;
			yourself.

		packageDefinition := projectDefinition packageNamed: packageName1, projectExt.

		classDefinition := RwClassDefinition
			newForClassNamed: className1, projectExt
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1, projectExt
			comment: ''
			pools: #()
			type: 'normal'.
		classDefinition
			addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'method2'
						protocol: 'accessing'
						source: 'method2 ^2');
			addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: 'initialization'
						source: 'initialize "noop"').
		packageDefinition addClassDefinition: classDefinition.

		classDefinition := RwClassDefinition
			newForClassNamed: className2, projectExt
			super: 'Array'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1, projectExt
			comment: ''
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
			newForClassNamed: className3, projectExt
			super: className1
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1, projectExt
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

		classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1, projectExt.
		classExtensionDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'method1'
						protocol: '*', (packageName2, projectExt) asLowercase
						source: 'method1 ^1').
		packageDefinition := projectDefinition packageNamed: packageName2, projectExt.
		packageDefinition addClassExtensionDefinition: classExtensionDefinition.

		packageDefinition := projectDefinition packageNamed: packageName3, projectExt.

		classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1, projectExt.
		classExtensionDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'method5'
						protocol: '*', (packageName3, projectExt) asLowercase
						source: 'method5 ^5').
		packageDefinition addClassExtensionDefinition: classExtensionDefinition.

		classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className2, projectExt.
		classExtensionDefinition
			addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: '*', (packageName3, projectExt) asLowercase
						source: 'initialize "another noop"').
		packageDefinition addClassExtensionDefinition: classExtensionDefinition.

		projectSetDefinition addDefinition: projectDefinition ].

"export"
	repositoryRootPath := projectsHome / projectName / 'topaz'.
	self _markForCleanup: (projectsHome / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitorV2 new
		excludeRemoveAllMethods: true;
		repositoryRootPath: repositoryRootPath;
		topazFilename: projectName;
		yourself.
	visitor visit: projectSetModification.

"validate"
	stream := ZnBufferedReadStream on: visitor topazFileReference readStream.
	x := stream contents.
	y := self _expectedGsTopazWriterFileContents_Multi_A.
	self assert: x = y
]

{ #category : 'tests' }
RwProjectTopazWriterTestV2 >> testGsTopazWriter_MultiProject_B [

	"ramp up to multiple class extensions from multiple packages and multiple projects"

	"write each package pair into a separate topaz file"

	"include class initialization methods"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition 
		className1 className2 className3 classExtensionDefinition projectSetDefinition
		projectSetModification visitor repositoryRootPath x stream y packageName3 componentName
		projectsHome |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 
	componentName := 'Core'.
	projectsHome := self _testRowanProjectsSandbox.

	projectSetDefinition := RwProjectSetDefinition new.

	(projectsHome / projectName) ensureDeleteAll.

	{1 . 2 } do: [:projectIndex |
		| projectExt |
		projectExt := '_', projectIndex asString.

"create definitions"
		projectDefinition := RwResolvedProjectV2 new
			projectName: projectName, projectExt;
			projectsHome: projectsHome;
			gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
			addNewComponentNamed: componentName;
			addPackagesNamed: { 
				packageName1, projectExt . 
				packageName2, projectExt . 
				packageName3, projectExt}
				toComponentNamed: componentName;
			gemstoneSetSymbolDictName: self _symbolDictionaryName 
				forPackageNamed: packageName1, projectExt;
			gemstoneSetSymbolDictName: self _symbolDictionaryName 
				forPackageNamed: packageName2, projectExt;
			resolve;
			yourself.

		packageDefinition := projectDefinition packageNamed: packageName1, projectExt.

		classDefinition := RwClassDefinition
			newForClassNamed: className1, projectExt
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1, projectExt
			comment: ''
			pools: #()
			type: 'normal'.
		classDefinition
			addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'method2'
						protocol: 'accessing'
						source: 'method2 ^2');
			addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: 'initialization'
						source: 'initialize "noop"').
		packageDefinition addClassDefinition: classDefinition.

		classDefinition := RwClassDefinition
			newForClassNamed: className2, projectExt
			super: 'Array'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1, projectExt
			comment: ''
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
			newForClassNamed: className3, projectExt
			super: className1
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1, projectExt
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

		classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1, projectExt.
		classExtensionDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'method1'
						protocol: '*', (packageName2, projectExt) asLowercase
						source: 'method1 ^1').
		packageDefinition := projectDefinition packageNamed: packageName2, projectExt.
		packageDefinition addClassExtensionDefinition: classExtensionDefinition.

		packageDefinition := projectDefinition packageNamed: packageName3, projectExt.

		classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1, projectExt.
		classExtensionDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'method5'
						protocol: '*', (packageName3, projectExt) asLowercase
						source: 'method5 ^5').
		packageDefinition addClassExtensionDefinition: classExtensionDefinition.

		classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className2, projectExt.
		classExtensionDefinition
			addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: '*', (packageName3, projectExt) asLowercase
						source: 'initialize "another noop"').
		packageDefinition addClassExtensionDefinition: classExtensionDefinition.

		projectSetDefinition addDefinition: projectDefinition ].

"export"
	repositoryRootPath := projectsHome / projectName / 'topaz'.
	self _markForCleanup: (projectsHome / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitorV2 new
		excludeRemoveAllMethods: true;
		repositoryRootPath: repositoryRootPath;
		topazFilenamePackageNamesMap: 
			(Dictionary new
				at: packageName1 put: {packageName1, '_1' . packageName1, '_2' };
				at: packageName2 put: {packageName2, '_1' . packageName2, '_2' };
				at: packageName3 put: {packageName3, '_1' . packageName3, '_2' };
				yourself)
		yourself.
	visitor visit: projectSetModification.

"validate"
	{ packageName1. packageName2. packageName3 }
		do: [:packageName |
			stream := ZnBufferedReadStream on: (repositoryRootPath / packageName, visitor filenameExtension) readStream.
			x := stream contents.
			y := self perform: #'_expectedGsTopazWriterFileContents_MULTI_B_', (packageName copyWithout: $-).

			self assert: x size = y size.
			1 to: x size do: [:i |
				| a b |
				a := x at: i.
				b := y at: i.
				self assert: a = b ].

			self assert: x = y ].
]
