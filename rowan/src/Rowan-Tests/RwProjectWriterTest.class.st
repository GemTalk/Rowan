Class {
	#name : 'RwProjectWriterTest',
	#superclass : 'RwBrowserToolTest',
	#instVars : [
		'filesystem',
		'toDelete',
		'projectsToDelete'
	],
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwProjectWriterTest >> _createFileSystem [
	^ FileSystem "disk "currentMemoryFileSystem
]

{ #category : 'private' }
RwProjectWriterTest >> _expectedGsTopazWriterFileContents_A [

	^ '! Class Declarations

doit
(Array
	subclass: ''Issue361Class2''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Object
	subclass: ''Issue361Class1''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Issue361Class1
	subclass: ''Issue361Class3''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core'';
		comment: '''';
		immediateInvariant.
true.
%

! Class implementation for ''Issue361Class2''

!		Instance methods for ''Issue361Class2''

category: ''accessing''
method: Issue361Class2
method3 ^3
%

! Class implementation for ''Issue361Class1''

!		Class methods for ''Issue361Class1''

category: ''accessing''
classmethod: Issue361Class1
method2 ^2
%

! Class implementation for ''Issue361Class3''

!		Class methods for ''Issue361Class3''

category: ''accessing''
classmethod: Issue361Class3
method4 ^4
%

! Class extensions for ''Issue361Class1''

!		Instance methods for ''Issue361Class1''

category: ''*issue361-extension''
method: Issue361Class1
method1 ^1
%

'
]

{ #category : 'private' }
RwProjectWriterTest >> _expectedGsTopazWriterFileContents_B [

	^ '! Class Declarations

doit
(Array
	subclass: ''Issue361Class2''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Object
	subclass: ''Issue361Class1''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Issue361Class1
	subclass: ''Issue361Class3''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core'';
		comment: '''';
		immediateInvariant.
true.
%

! Class implementation for ''Issue361Class2''

!		Instance methods for ''Issue361Class2''

category: ''accessing''
method: Issue361Class2
method3 ^3
%

! Class implementation for ''Issue361Class1''

!		Class methods for ''Issue361Class1''

category: ''accessing''
classmethod: Issue361Class1
method2 ^2
%

! Class implementation for ''Issue361Class3''

!		Class methods for ''Issue361Class3''

category: ''accessing''
classmethod: Issue361Class3
method4 ^4
%

! Class extensions for ''Issue361Class1''

!		Instance methods for ''Issue361Class1''

category: ''*issue361-extension1''
method: Issue361Class1
method1 ^1
%

category: ''*issue361-extension2''
method: Issue361Class1
method5 ^5
%

'
]

{ #category : 'private' }
RwProjectWriterTest >> _expectedGsTopazWriterFileContents_C_Issue361Core [

	^ '! Class Declarations

doit
(Array
	subclass: ''Issue361Class2''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Object
	subclass: ''Issue361Class1''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Issue361Class1
	subclass: ''Issue361Class3''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core'';
		comment: '''';
		immediateInvariant.
true.
%

! Class implementation for ''Issue361Class2''

!		Instance methods for ''Issue361Class2''

category: ''accessing''
method: Issue361Class2
method3 ^3
%

! Class implementation for ''Issue361Class1''

!		Class methods for ''Issue361Class1''

category: ''accessing''
classmethod: Issue361Class1
method2 ^2
%

! Class implementation for ''Issue361Class3''

!		Class methods for ''Issue361Class3''

category: ''accessing''
classmethod: Issue361Class3
method4 ^4
%

'
]

{ #category : 'private' }
RwProjectWriterTest >> _expectedGsTopazWriterFileContents_C_Issue361Extension1 [

	^ '! Class extensions for ''Issue361Class1''

!		Instance methods for ''Issue361Class1''

category: ''*issue361-extension1''
method: Issue361Class1
method1 ^1
%

'
]

{ #category : 'private' }
RwProjectWriterTest >> _expectedGsTopazWriterFileContents_C_Issue361Extension2 [

	^ '! Class extensions for ''Issue361Class1''

!		Instance methods for ''Issue361Class1''

category: ''*issue361-extension2''
method: Issue361Class1
method5 ^5
%

'
]

{ #category : 'private' }
RwProjectWriterTest >> _expectedGsTopazWriterFileContents_D_Issue361Core [

	^ '! Class Declarations

doit
(Array
	subclass: ''Issue361Class2''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Object
	subclass: ''Issue361Class1''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Issue361Class1
	subclass: ''Issue361Class3''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core'';
		comment: '''';
		immediateInvariant.
true.
%

! Class implementation for ''Issue361Class2''

!		Instance methods for ''Issue361Class2''

category: ''accessing''
method: Issue361Class2
method3 ^3
%

! Class implementation for ''Issue361Class1''

!		Class methods for ''Issue361Class1''

category: ''initialization''
classmethod: Issue361Class1
initialize "noop"
%

category: ''accessing''
classmethod: Issue361Class1
method2 ^2
%

! Class implementation for ''Issue361Class3''

!		Class methods for ''Issue361Class3''

category: ''accessing''
classmethod: Issue361Class3
method4 ^4
%

! Class Initialization

run
Issue361Class1 initialize.
%

'
]

{ #category : 'private' }
RwProjectWriterTest >> _expectedGsTopazWriterFileContents_D_Issue361Extension1 [

	^ '! Class extensions for ''Issue361Class1''

!		Instance methods for ''Issue361Class1''

category: ''*issue361-extension1''
method: Issue361Class1
method1 ^1
%

'
]

{ #category : 'private' }
RwProjectWriterTest >> _expectedGsTopazWriterFileContents_D_Issue361Extension2 [

	^ '! Class extensions for ''Issue361Class1''

!		Instance methods for ''Issue361Class1''

category: ''*issue361-extension2''
method: Issue361Class1
method5 ^5
%

! Class extensions for ''Issue361Class2''

!		Class methods for ''Issue361Class2''

category: ''*issue361-extension2''
classmethod: Issue361Class2
initialize "another noop"
%

! Class Initialization

run
Issue361Class2 initialize.
%

'
]

{ #category : 'private' }
RwProjectWriterTest >> _expectedGsTopazWriterFileContents_Multi_A [

	^ '! Class Declarations

doit
(Array
	subclass: ''Issue361Class2_1''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_1'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Array
	subclass: ''Issue361Class2_2''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_2'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Issue361Class1
	subclass: ''Issue361Class3_1''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_1'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Issue361Class1
	subclass: ''Issue361Class3_2''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_2'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Object
	subclass: ''Issue361Class1_1''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_1'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Object
	subclass: ''Issue361Class1_2''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_2'';
		comment: '''';
		immediateInvariant.
true.
%

! Class implementation for ''Issue361Class2_1''

!		Instance methods for ''Issue361Class2_1''

category: ''accessing''
method: Issue361Class2_1
method3 ^3
%

! Class implementation for ''Issue361Class2_2''

!		Instance methods for ''Issue361Class2_2''

category: ''accessing''
method: Issue361Class2_2
method3 ^3
%

! Class implementation for ''Issue361Class3_1''

!		Class methods for ''Issue361Class3_1''

category: ''accessing''
classmethod: Issue361Class3_1
method4 ^4
%

! Class implementation for ''Issue361Class3_2''

!		Class methods for ''Issue361Class3_2''

category: ''accessing''
classmethod: Issue361Class3_2
method4 ^4
%

! Class implementation for ''Issue361Class1_1''

!		Class methods for ''Issue361Class1_1''

category: ''initialization''
classmethod: Issue361Class1_1
initialize "noop"
%

category: ''accessing''
classmethod: Issue361Class1_1
method2 ^2
%

! Class implementation for ''Issue361Class1_2''

!		Class methods for ''Issue361Class1_2''

category: ''initialization''
classmethod: Issue361Class1_2
initialize "noop"
%

category: ''accessing''
classmethod: Issue361Class1_2
method2 ^2
%

! Class extensions for ''Issue361Class1_1''

!		Instance methods for ''Issue361Class1_1''

category: ''*issue361-extension1_1''
method: Issue361Class1_1
method1 ^1
%

category: ''*issue361-extension2_1''
method: Issue361Class1_1
method5 ^5
%

! Class extensions for ''Issue361Class1_2''

!		Instance methods for ''Issue361Class1_2''

category: ''*issue361-extension1_2''
method: Issue361Class1_2
method1 ^1
%

category: ''*issue361-extension2_2''
method: Issue361Class1_2
method5 ^5
%

! Class extensions for ''Issue361Class2_1''

!		Class methods for ''Issue361Class2_1''

category: ''*issue361-extension2_1''
classmethod: Issue361Class2_1
initialize "another noop"
%

! Class extensions for ''Issue361Class2_2''

!		Class methods for ''Issue361Class2_2''

category: ''*issue361-extension2_2''
classmethod: Issue361Class2_2
initialize "another noop"
%

! Class Initialization

run
Issue361Class1_1 initialize.
Issue361Class1_2 initialize.
Issue361Class2_1 initialize.
Issue361Class2_2 initialize.
%

'
]

{ #category : 'private' }
RwProjectWriterTest >> _expectedGsTopazWriterFileContents_MULTI_B_Issue361Core [

	^ '! Class Declarations

doit
(Array
	subclass: ''Issue361Class2_1''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_1'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Array
	subclass: ''Issue361Class2_2''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_2'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Issue361Class1
	subclass: ''Issue361Class3_1''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_1'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Issue361Class1
	subclass: ''Issue361Class3_2''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_2'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Object
	subclass: ''Issue361Class1_1''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_1'';
		comment: '''';
		immediateInvariant.
true.
%

doit
(Object
	subclass: ''Issue361Class1_2''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_2'';
		comment: '''';
		immediateInvariant.
true.
%

! Class implementation for ''Issue361Class2_1''

!		Instance methods for ''Issue361Class2_1''

category: ''accessing''
method: Issue361Class2_1
method3 ^3
%

! Class implementation for ''Issue361Class2_2''

!		Instance methods for ''Issue361Class2_2''

category: ''accessing''
method: Issue361Class2_2
method3 ^3
%

! Class implementation for ''Issue361Class3_1''

!		Class methods for ''Issue361Class3_1''

category: ''accessing''
classmethod: Issue361Class3_1
method4 ^4
%

! Class implementation for ''Issue361Class3_2''

!		Class methods for ''Issue361Class3_2''

category: ''accessing''
classmethod: Issue361Class3_2
method4 ^4
%

! Class implementation for ''Issue361Class1_1''

!		Class methods for ''Issue361Class1_1''

category: ''initialization''
classmethod: Issue361Class1_1
initialize "noop"
%

category: ''accessing''
classmethod: Issue361Class1_1
method2 ^2
%

! Class implementation for ''Issue361Class1_2''

!		Class methods for ''Issue361Class1_2''

category: ''initialization''
classmethod: Issue361Class1_2
initialize "noop"
%

category: ''accessing''
classmethod: Issue361Class1_2
method2 ^2
%

! Class Initialization

run
Issue361Class1_1 initialize.
Issue361Class1_2 initialize.
%

'
]

{ #category : 'private' }
RwProjectWriterTest >> _expectedGsTopazWriterFileContents_MULTI_B_Issue361Extension1 [

	^ '! Class extensions for ''Issue361Class1_1''

!		Instance methods for ''Issue361Class1_1''

category: ''*issue361-extension1_1''
method: Issue361Class1_1
method1 ^1
%

! Class extensions for ''Issue361Class1_2''

!		Instance methods for ''Issue361Class1_2''

category: ''*issue361-extension1_2''
method: Issue361Class1_2
method1 ^1
%

'
]

{ #category : 'private' }
RwProjectWriterTest >> _expectedGsTopazWriterFileContents_MULTI_B_Issue361Extension2 [

	^ '! Class extensions for ''Issue361Class1_1''

!		Instance methods for ''Issue361Class1_1''

category: ''*issue361-extension2_1''
method: Issue361Class1_1
method5 ^5
%

! Class extensions for ''Issue361Class1_2''

!		Instance methods for ''Issue361Class1_2''

category: ''*issue361-extension2_2''
method: Issue361Class1_2
method5 ^5
%

! Class extensions for ''Issue361Class2_1''

!		Class methods for ''Issue361Class2_1''

category: ''*issue361-extension2_1''
classmethod: Issue361Class2_1
initialize "another noop"
%

! Class extensions for ''Issue361Class2_2''

!		Class methods for ''Issue361Class2_2''

category: ''*issue361-extension2_2''
classmethod: Issue361Class2_2
initialize "another noop"
%

! Class Initialization

run
Issue361Class2_1 initialize.
Issue361Class2_2 initialize.
%

'
]

{ #category : 'private' }
RwProjectWriterTest >> _markForCleanup: anObject [
	toDelete add: (filesystem resolve: anObject)
]

{ #category : 'private' }
RwProjectWriterTest >> _markForProjectCleanup: aProjectName [
	projectsToDelete add: aProjectName
]

{ #category : 'private' }
RwProjectWriterTest >> _projectSetDefinitionForStructureWriters_A [

	"multiple class extensions from multiple packages for multiple classes"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition className1 className2 className3
		classExtensionDefinition projectSetDefinition projectSetModification visitor repositoryRootPath packageName3 |

	projectName := 'Issue361'.
	self _markForProjectCleanup: projectName.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 

"create definitions"
	projectDefinition := (RwProjectDefinition
		newForDiskBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName1;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName2;
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
	packageDefinition addClassExtension: classExtensionDefinition.

	packageDefinition := projectDefinition packageNamed: packageName3.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: '*', packageName3 asLowercase
					source: 'method5 ^5').
	packageDefinition addClassExtension: classExtensionDefinition.

"project set"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

	^ projectSetDefinition
]

{ #category : 'running' }
RwProjectWriterTest >> setUp [
	super setUp.
	filesystem := self _createFileSystem.
	toDelete := OrderedCollection new.
	projectsToDelete := OrderedCollection new.
]

{ #category : 'running' }
RwProjectWriterTest >> tearDown [
	toDelete
		select: [ :path | filesystem exists: path ]
		thenDo: [ :path | filesystem delete: path ].
	projectsToDelete
		do: [ :projectName | Rowan projectTools delete deleteProjectNamed: projectName ].
	super tearDown
]

{ #category : 'tests' }
RwProjectWriterTest >> testFiletreeWriter_A [

	"multiple class extensions from multiple packages for multiple classes"

	| projectName projectSetDefinition projectDefinition projectSetModification visitor repositoryRootPath |
"project set"
	projectSetDefinition := self _projectSetDefinitionForStructureWriters_A.
	projectName := projectSetDefinition projectNames at: 1.
	projectDefinition := projectSetDefinition projectNamed: projectName ifAbsent: [].

"set project up for using filesystem"
	repositoryRootPath := filesystem workingDirectory / projectName.
	self _markForCleanup: (filesystem workingDirectory / projectName).

	projectDefinition repositoryRootPath: repositoryRootPath.
	Rowan projectTools create createProjectFor: projectDefinition format: 'filetree'.	"create repository structure on filesystem"

"write project set to tonel"
	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwModificationFiletreeWriterVisitor new
		repositoryRootPath: repositoryRootPath;
		yourself.
[
	visitor visit: projectSetModification.
] on: Error do: [:ex |
	toDelete
		select: [ :path | filesystem exists: path ]
		thenDo: [ :path |
self error: 'halt here'.
 (filesystem resolve: path) deleteAll ].
].
"validation -- need to have reader implemented before doing validation"
self halt
]

{ #category : 'tests' }
RwProjectWriterTest >> testGsTopazWriter_A [

	"simple case with only two packages and a single set of extensions for a class"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition className1 className2 className3
		classExtensionDefinition projectSetDefinition projectSetModification visitor repositoryRootPath x stream y |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 

"create definitions"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName1;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName2;
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
	packageDefinition addClassExtension: classExtensionDefinition.

"diff"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

"write"
	repositoryRootPath := filesystem workingDirectory / projectName / 'topaz'.
	self _markForCleanup: (filesystem workingDirectory / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitor new
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
RwProjectWriterTest >> testGsTopazWriter_B [

	"ramp up to multiple class extensions from multiple packages for multiple classes"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition className1 className2 className3
		classExtensionDefinition projectSetDefinition projectSetModification visitor repositoryRootPath x stream y
		packageName3 |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 

"create definitions"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName1;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName2;
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
	packageDefinition addClassExtension: classExtensionDefinition.

	packageDefinition := projectDefinition packageNamed: packageName3.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: '*', packageName3 asLowercase
					source: 'method5 ^5').
	packageDefinition addClassExtension: classExtensionDefinition.

"diff"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

"write"
	repositoryRootPath := filesystem workingDirectory / projectName / 'topaz'.
	self _markForCleanup: (filesystem workingDirectory / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitor new
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
RwProjectWriterTest >> testGsTopazWriter_C [

	"ramp up to multiple class extensions from multiple packages for multiple classes"

	"write each package into a separate topaz file"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition className1 className2 className3
		classExtensionDefinition projectSetDefinition projectSetModification visitor repositoryRootPath x stream y
		packageName3 |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 

"create definitions"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName1;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName2;
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
	packageDefinition addClassExtension: classExtensionDefinition.

	packageDefinition := projectDefinition packageNamed: packageName3.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: '*', packageName3 asLowercase
					source: 'method5 ^5').
	packageDefinition addClassExtension: classExtensionDefinition.

"diff"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

"write"
	repositoryRootPath := filesystem workingDirectory / projectName / 'topaz'.
	self _markForCleanup: (filesystem workingDirectory / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitor new
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
RwProjectWriterTest >> testGsTopazWriter_D [

	"ramp up to multiple class extensions from multiple packages for multiple classes"

	"write each package into a separate topaz file"

	"include class initialization methods"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition className1 className2 className3
		classExtensionDefinition projectSetDefinition projectSetModification visitor repositoryRootPath x stream y
		packageName3 |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 

"create definitions"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName1;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName2;
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
	packageDefinition addClassExtension: classExtensionDefinition.

	packageDefinition := projectDefinition packageNamed: packageName3.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: '*', packageName3 asLowercase
					source: 'method5 ^5').
	packageDefinition addClassExtension: classExtensionDefinition.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className2.
	classExtensionDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'initialize'
					protocol: '*', packageName3 asLowercase
					source: 'initialize "another noop"').
	packageDefinition addClassExtension: classExtensionDefinition.


"diff"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

"write"
	repositoryRootPath := filesystem workingDirectory / projectName / 'topaz'.
	self _markForCleanup: (filesystem workingDirectory / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitor new
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
RwProjectWriterTest >> testGsTopazWriter_error [

	"try to write to multiple files when no configs are available"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition className1 className2 className3
		classExtensionDefinition projectSetDefinition projectSetModification visitor repositoryRootPath x stream y
		packageName3 |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 

"create definitions"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName1;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName2;
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
	packageDefinition addClassExtension: classExtensionDefinition.

	packageDefinition := projectDefinition packageNamed: packageName3.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: '*', packageName3 asLowercase
					source: 'method5 ^5').
	packageDefinition addClassExtension: classExtensionDefinition.

"diff"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

"write"
	repositoryRootPath := filesystem workingDirectory / projectName / 'topaz'.
	self _markForCleanup: (filesystem workingDirectory / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitor new
				repositoryRootPath: repositoryRootPath;
				topazFilenameConfigsMap: (Dictionary new
						at: 'rowan_bootstrap_app' put: {{'Kernel'}. {}};
						at: 'rowan_bootstrap_globals' put: {{'Globals'}. {}};
						at: 'rowan_bootstrap_user_globals' put: {{'UserGlobals'}. {}};
						yourself);
				yourself.
	self should: [ visitor visit: projectSetModification ] raise: Error
]

{ #category : 'tests' }
RwProjectWriterTest >> testGsTopazWriter_MultiProject_A [

	"ramp up to multiple class extensions from multiple packages and multiple projects"

	"write a single topaz file"

	"include class initialization methods"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition className1 className2 className3
		classExtensionDefinition projectSetDefinition projectSetModification visitor repositoryRootPath x stream y
		packageName3 |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 

	projectSetDefinition := RwProjectSetDefinition new.

	{1 . 2 } do: [:projectIndex |
		| projectExt |
		projectExt := '_', projectIndex asString.

"create definitions"
		projectDefinition := (RwProjectDefinition
			newForGitBasedProjectNamed: projectName, projectExt)
			addPackageNamed: packageName1, projectExt;
			addPackageNamed: packageName2, projectExt;
			addPackageNamed: packageName3, projectExt;
			setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName1, projectExt;
			setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName2, projectExt;
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
		packageDefinition addClassExtension: classExtensionDefinition.

		packageDefinition := projectDefinition packageNamed: packageName3, projectExt.

		classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1, projectExt.
		classExtensionDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'method5'
						protocol: '*', (packageName3, projectExt) asLowercase
						source: 'method5 ^5').
		packageDefinition addClassExtension: classExtensionDefinition.

		classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className2, projectExt.
		classExtensionDefinition
			addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: '*', (packageName3, projectExt) asLowercase
						source: 'initialize "another noop"').
		packageDefinition addClassExtension: classExtensionDefinition.

		projectSetDefinition addDefinition: projectDefinition ].

"export"
	repositoryRootPath := filesystem workingDirectory / projectName / 'topaz'.
	self _markForCleanup: (filesystem workingDirectory / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitor new
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
RwProjectWriterTest >> testGsTopazWriter_MultiProject_B [

	"ramp up to multiple class extensions from multiple packages and multiple projects"

	"write each package pair into a separate topaz file"

	"include class initialization methods"

	|  projectName packageName1 packageName2 projectDefinition classDefinition packageDefinition className1 className2 className3
		classExtensionDefinition projectSetDefinition projectSetModification visitor repositoryRootPath x stream y
		packageName3 |

	projectName := 'Issue361'.
	packageName1 := 'Issue361-Core'.
	packageName2 := 'Issue361-Extension1'.
	packageName3 := 'Issue361-Extension2'.
	className1 := 'Issue361Class1'. 
	className2 := 'Issue361Class2'. 
	className3 := 'Issue361Class3'. 

	projectSetDefinition := RwProjectSetDefinition new.

	{1 . 2 } do: [:projectIndex |
		| projectExt |
		projectExt := '_', projectIndex asString.

"create definitions"
		projectDefinition := (RwProjectDefinition
			newForGitBasedProjectNamed: projectName, projectExt)
			addPackageNamed: packageName1, projectExt;
			addPackageNamed: packageName2, projectExt;
			addPackageNamed: packageName3, projectExt;
			setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName1, projectExt;
			setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName2, projectExt;
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
		packageDefinition addClassExtension: classExtensionDefinition.

		packageDefinition := projectDefinition packageNamed: packageName3, projectExt.

		classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1, projectExt.
		classExtensionDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'method5'
						protocol: '*', (packageName3, projectExt) asLowercase
						source: 'method5 ^5').
		packageDefinition addClassExtension: classExtensionDefinition.

		classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className2, projectExt.
		classExtensionDefinition
			addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: '*', (packageName3, projectExt) asLowercase
						source: 'initialize "another noop"').
		packageDefinition addClassExtension: classExtensionDefinition.

		projectSetDefinition addDefinition: projectDefinition ].

"export"
	repositoryRootPath := filesystem workingDirectory / projectName / 'topaz'.
	self _markForCleanup: (filesystem workingDirectory / projectName).
	repositoryRootPath ensureCreateDirectory.

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitor new
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

{ #category : 'tests' }
RwProjectWriterTest >> testTonelWriter_A [

	"multiple class extensions from multiple packages for multiple classes"

	| projectName projectSetDefinition projectDefinition projectSetModification visitor repositoryRootPath |
"project set"
	projectSetDefinition := self _projectSetDefinitionForStructureWriters_A.
	projectName := projectSetDefinition projectNames at: 1.
	projectDefinition := projectSetDefinition projectNamed: projectName ifAbsent: [].

"set project up for using filesystem"
	repositoryRootPath := filesystem workingDirectory / projectName.
	self _markForCleanup: (filesystem workingDirectory / projectName).

	projectDefinition repositoryRootPath: repositoryRootPath.
	Rowan projectTools create createProjectFor: projectDefinition format: 'tonel'.	"create repository structure on filesystem"

"write project set to tonel"
	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwModificationTonelWriterVisitor new
		repositoryRootPath: repositoryRootPath;
		yourself.
	visitor visit: projectSetModification.

"validation -- need to have reader implemented before doing validation"
]
