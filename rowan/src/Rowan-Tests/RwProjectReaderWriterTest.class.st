Class {
	#name : 'RwProjectReaderWriterTest',
	#superclass : 'RwBrowserToolTest',
	#instVars : [
		'filesystem',
		'toDelete',
		'projectsToDelete'
	],
	#category : 'Rowan-Tests'
}

{ #category : 'testing' }
RwProjectReaderWriterTest class >> isAbstract [
  "Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

  ^ self sunitName = #'RwProjectReaderWriterTest'
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _createFileSystem [
	^ FileSystem disk "currentMemoryFileSystem"
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _expectedGsTopazWriterFileContents_A [

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
		comment: '''';
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
		comment: '''';
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
		comment: '''';
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
RwProjectReaderWriterTest >> _expectedGsTopazWriterFileContents_B [

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
		comment: '''';
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
		comment: '''';
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
		comment: '''';
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
RwProjectReaderWriterTest >> _expectedGsTopazWriterFileContents_C_Issue361Core [

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
		comment: '''';
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
		comment: '''';
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
		comment: '''';
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
RwProjectReaderWriterTest >> _expectedGsTopazWriterFileContents_C_Issue361Extension1 [

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
RwProjectReaderWriterTest >> _expectedGsTopazWriterFileContents_C_Issue361Extension2 [

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
RwProjectReaderWriterTest >> _expectedGsTopazWriterFileContents_D_Issue361Core [

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

category: ''initialization''
classmethod: Issue361Class1
initialize "noop"
', percent, '

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

! Class Initialization

run
Issue361Class1 initialize.
true
', percent, '
'
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _expectedGsTopazWriterFileContents_D_Issue361Extension1 [

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
RwProjectReaderWriterTest >> _expectedGsTopazWriterFileContents_D_Issue361Extension2 [

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class extensions for ''Issue361Class1''

!		Instance methods for ''Issue361Class1''

category: ''*issue361-extension2''
method: Issue361Class1
method5 ^5
', percent, '

! Class extensions for ''Issue361Class2''

!		Class methods for ''Issue361Class2''

category: ''*issue361-extension2''
classmethod: Issue361Class2
initialize "another noop"
', percent, '

! Class Initialization

run
Issue361Class2 initialize.
true
', percent, '
'
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _expectedGsTopazWriterFileContents_Multi_A [

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class Declarations
! Generated file, do not Edit

doit
(Array
	subclass: ''Issue361Class2_1''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core_1'';
		immediateInvariant.
true.
', percent, '

doit
(Array
	subclass: ''Issue361Class2_2''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core_2'';
		immediateInvariant.
true.
', percent, '

doit
(Issue361Class1
	subclass: ''Issue361Class3_1''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core_1'';
		immediateInvariant.
true.
', percent, '

doit
(Issue361Class1
	subclass: ''Issue361Class3_2''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core_2'';
		immediateInvariant.
true.
', percent, '

doit
(Object
	subclass: ''Issue361Class1_1''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core_1'';
		immediateInvariant.
true.
', percent, '

doit
(Object
	subclass: ''Issue361Class1_2''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core_2'';
		immediateInvariant.
true.
', percent, '

! Class implementation for ''Issue361Class2_1''

!		Instance methods for ''Issue361Class2_1''

category: ''accessing''
method: Issue361Class2_1
method3 ^3
', percent, '

! Class implementation for ''Issue361Class2_2''

!		Instance methods for ''Issue361Class2_2''

category: ''accessing''
method: Issue361Class2_2
method3 ^3
', percent, '

! Class implementation for ''Issue361Class3_1''

!		Class methods for ''Issue361Class3_1''

category: ''accessing''
classmethod: Issue361Class3_1
method4 ^4
', percent, '

! Class implementation for ''Issue361Class3_2''

!		Class methods for ''Issue361Class3_2''

category: ''accessing''
classmethod: Issue361Class3_2
method4 ^4
', percent, '

! Class implementation for ''Issue361Class1_1''

!		Class methods for ''Issue361Class1_1''

category: ''initialization''
classmethod: Issue361Class1_1
initialize "noop"
', percent, '

category: ''accessing''
classmethod: Issue361Class1_1
method2 ^2
', percent, '

! Class implementation for ''Issue361Class1_2''

!		Class methods for ''Issue361Class1_2''

category: ''initialization''
classmethod: Issue361Class1_2
initialize "noop"
', percent, '

category: ''accessing''
classmethod: Issue361Class1_2
method2 ^2
', percent, '

! Class extensions for ''Issue361Class1_1''

!		Instance methods for ''Issue361Class1_1''

category: ''*issue361-extension1_1''
method: Issue361Class1_1
method1 ^1
', percent, '

category: ''*issue361-extension2_1''
method: Issue361Class1_1
method5 ^5
', percent, '

! Class extensions for ''Issue361Class1_2''

!		Instance methods for ''Issue361Class1_2''

category: ''*issue361-extension1_2''
method: Issue361Class1_2
method1 ^1
', percent, '

category: ''*issue361-extension2_2''
method: Issue361Class1_2
method5 ^5
', percent, '

! Class extensions for ''Issue361Class2_1''

!		Class methods for ''Issue361Class2_1''

category: ''*issue361-extension2_1''
classmethod: Issue361Class2_1
initialize "another noop"
', percent, '

! Class extensions for ''Issue361Class2_2''

!		Class methods for ''Issue361Class2_2''

category: ''*issue361-extension2_2''
classmethod: Issue361Class2_2
initialize "another noop"
', percent, '

! Class Initialization

run
Issue361Class1_1 initialize.
Issue361Class1_2 initialize.
Issue361Class2_1 initialize.
Issue361Class2_2 initialize.
true
', percent, '
'
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _expectedGsTopazWriterFileContents_MULTI_B_Issue361Core [

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class Declarations
! Generated file, do not Edit

doit
(Array
	subclass: ''Issue361Class2_1''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core_1'';
		immediateInvariant.
true.
', percent, '

doit
(Array
	subclass: ''Issue361Class2_2''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core_2'';
		immediateInvariant.
true.
', percent, '

doit
(Issue361Class1
	subclass: ''Issue361Class3_1''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core_1'';
		immediateInvariant.
true.
', percent, '

doit
(Issue361Class1
	subclass: ''Issue361Class3_2''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core_2'';
		immediateInvariant.
true.
', percent, '

doit
(Object
	subclass: ''Issue361Class1_1''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core_1'';
		immediateInvariant.
true.
', percent, '

doit
(Object
	subclass: ''Issue361Class1_2''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #()
)
		category: ''Issue361-Core_2'';
		immediateInvariant.
true.
', percent, '

! Class implementation for ''Issue361Class2_1''

!		Instance methods for ''Issue361Class2_1''

category: ''accessing''
method: Issue361Class2_1
method3 ^3
', percent, '

! Class implementation for ''Issue361Class2_2''

!		Instance methods for ''Issue361Class2_2''

category: ''accessing''
method: Issue361Class2_2
method3 ^3
', percent, '

! Class implementation for ''Issue361Class3_1''

!		Class methods for ''Issue361Class3_1''

category: ''accessing''
classmethod: Issue361Class3_1
method4 ^4
', percent, '

! Class implementation for ''Issue361Class3_2''

!		Class methods for ''Issue361Class3_2''

category: ''accessing''
classmethod: Issue361Class3_2
method4 ^4
', percent, '

! Class implementation for ''Issue361Class1_1''

!		Class methods for ''Issue361Class1_1''

category: ''initialization''
classmethod: Issue361Class1_1
initialize "noop"
', percent, '

category: ''accessing''
classmethod: Issue361Class1_1
method2 ^2
', percent, '

! Class implementation for ''Issue361Class1_2''

!		Class methods for ''Issue361Class1_2''

category: ''initialization''
classmethod: Issue361Class1_2
initialize "noop"
', percent, '

category: ''accessing''
classmethod: Issue361Class1_2
method2 ^2
', percent, '

! Class Initialization

run
Issue361Class1_1 initialize.
Issue361Class1_2 initialize.
true
', percent, '
'
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _expectedGsTopazWriterFileContents_MULTI_B_Issue361Extension1 [

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class extensions for ''Issue361Class1_1''

!		Instance methods for ''Issue361Class1_1''

category: ''*issue361-extension1_1''
method: Issue361Class1_1
method1 ^1
', percent, '

! Class extensions for ''Issue361Class1_2''

!		Instance methods for ''Issue361Class1_2''

category: ''*issue361-extension1_2''
method: Issue361Class1_2
method1 ^1
', percent, '

'
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _expectedGsTopazWriterFileContents_MULTI_B_Issue361Extension2 [

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class extensions for ''Issue361Class1_1''

!		Instance methods for ''Issue361Class1_1''

category: ''*issue361-extension2_1''
method: Issue361Class1_1
method5 ^5
', percent, '

! Class extensions for ''Issue361Class1_2''

!		Instance methods for ''Issue361Class1_2''

category: ''*issue361-extension2_2''
method: Issue361Class1_2
method5 ^5
', percent, '

! Class extensions for ''Issue361Class2_1''

!		Class methods for ''Issue361Class2_1''

category: ''*issue361-extension2_1''
classmethod: Issue361Class2_1
initialize "another noop"
', percent, '

! Class extensions for ''Issue361Class2_2''

!		Class methods for ''Issue361Class2_2''

category: ''*issue361-extension2_2''
classmethod: Issue361Class2_2
initialize "another noop"
', percent, '

! Class Initialization

run
Issue361Class2_1 initialize.
Issue361Class2_2 initialize.
true
', percent, '
'
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _markForCleanup: aFileReference [
	toDelete add: aFileReference
]

{ #category : 'private' }
RwProjectReaderWriterTest >> _markForProjectCleanup: aProjectName [
	projectsToDelete add: aProjectName
]

{ #category : 'running' }
RwProjectReaderWriterTest >> setUp [
	super setUp.
	filesystem := self _createFileSystem.
	toDelete := OrderedCollection new.
	projectsToDelete := OrderedCollection new.
]

{ #category : 'running' }
RwProjectReaderWriterTest >> tearDown [
	toDelete
		select: [ :aFileRef | aFileRef exists ]
		thenDo: [ :aFileRef | aFileRef deleteAll ].
	projectsToDelete
		do: [ :projectName | 
		(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
			ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].
	super tearDown
]
