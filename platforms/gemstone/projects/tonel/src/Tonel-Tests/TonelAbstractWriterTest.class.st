Class {
	#name : 'TonelAbstractWriterTest',
	#superclass : 'TonelAbstractTest',
	#instVars : [
		'directory'
	],
	#category : 'Tonel-Tests'
}

{ #category : 'Testing' }
TonelAbstractWriterTest class >> isAbstract [
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #TonelAbstractWriterTest

]

{ #category : 'tests' }
TonelAbstractWriterTest >> expectedMCMockASubclassClassSt [
	^ 'Class {
	#name : #MCMockASubclass,
	#superclass : #MCMockClassA,
	#instVars : [
		''x''
	],
	#classVars : [
		''Y''
	],
	#category : #MonticelloMocks
}

{ #category : #''as yet unclassified'' }
MCMockASubclass class >> initialize [

	InitializationOrder := InitializationOrder
		ifNil: [ -100 ] "let the test fail"
		ifNotNil: [ InitializationOrder + 1.]
]

{ #category : #''as yet unclassified'' }
MCMockASubclass >> variables [
	^ x + Y + MCMockClassA
]

{ #category : #''as yet unclassified'' }
MCMockASubclass >> variables2 [
	^ ivar + CVar
]
'

]

{ #category : 'tests' }
TonelAbstractWriterTest >> expectedMCMockClassAClassSt [
	^ '"
This is a mock class. The Monticello tests manipulated it to simulate a developer modifying code in the image.
"
Class {
	#name : #MCMockClassA,
	#superclass : #MCMock,
	#instVars : [
		''ivar''
	],
	#classVars : [
		''CVar'',
		''InitializationOrder''
	],
	#category : #MonticelloMocks
}

{ #category : #''as yet unclassified'' }
MCMockClassA class >> cVar [
	^ CVar
]

{ #category : #''as yet unclassified'' }
MCMockClassA class >> initializationOrder [

	^ InitializationOrder
]

{ #category : #''as yet unclassified'' }
MCMockClassA class >> initialize [
	CVar := #initialized.
	InitializationOrder := 1.

]

{ #category : #''as yet unclassified'' }
MCMockClassA class >> one [

	^ 1
]

{ #category : #''as yet unclassified'' }
MCMockClassA class >> touchCVar [
	CVar := #touched
]

{ #category : #numeric }
MCMockClassA >> a [
	^ ''a2''
]

{ #category : #numeric }
MCMockClassA >> b [
	^ ''b1''
]

{ #category : #numeric }
MCMockClassA >> c [
	^ ''c1''
]

{ #category : #numeric }
MCMockClassA >> d [
	^ ''d''
]

{ #category : #boolean }
MCMockClassA >> falsehood [
	^ false
]

{ #category : #boolean }
MCMockClassA >> moreTruth [

	^ true
]

{ #category : #numeric }
MCMockClassA >> one [
	^ 1
]

{ #category : #''drag\''n\''drop'' }
MCMockClassA >> q [
]

{ #category : #boolean }
MCMockClassA >> truth [
	^ true
]

{ #category : #numeric }
MCMockClassA >> two [
	^ 2
]
'

]

{ #category : 'tests' }
TonelAbstractWriterTest >> expectedMCSnapshotTestExtensionSt [
	^ 'Extension { #name : #MCSnapshotTest }

{ #category : #''*MonticelloMocks'' }
MCSnapshotTest >> mockClassExtension [

	"I change the protocol of this method to resolve the failing test: MCChangeNotificationTest >> testExtMethodModified. This test basically test that when we modified an extension method, the extended package is marked as ''modified''. The problem is that Monticello treat differently a classic method from an extension method, and this only by checking if the protocol name start with a star. Therefore, if the protocol does not match the extending package name, the extending package name will never be notified, and the test will fail. "
]
'

]

{ #category : 'mocks' }
TonelAbstractWriterTest >> mockSnapshot [

	self subclassResponsibility

]

{ #category : 'running' }
TonelAbstractWriterTest >> setUp [
  directory := nil.
  super setUp

]

{ #category : 'tests' }
TonelAbstractWriterTest >> testWriteSnapshot [
  | writer dir nl packageDir snapshot |
  dir := self directory.
  writer := self writerClass on: dir.
  snapshot := self mockSnapshot.
  snapshot dynamicInstVarAt: #'packageName' put: 'MonticelloMocks'.
  writer writeSnapshot: snapshot.
  self assert: (self directoryNamed: 'MonticelloMocks' existsIn: dir).
  packageDir := self directoryNamed: 'MonticelloMocks' in: dir.
  self
    assert: (self fileNamesIn: packageDir)
    equals:
      #('MCMockASubclass.class.st' 'MCMockClassA.class.st' 'MCMockClassB.class.st' 'MCMockClassD.class.st' 'MCMockClassE.class.st' 'MCMockClassF.class.st' 'MCMockClassG.class.st' 'MCMockClassH.class.st' 'MCMockClassI.class.st' 'MCSnapshotTest.extension.st' 'package.st').
  nl := TonelWriter lineEnding.
  self
    assert:
      (self contentsOfFileNamed: 'MCMockClassA.class.st' inDirectory: packageDir)
    equals: (self expectedMCMockClassAClassSt withLineEndings: nl).
  self
    assert:
      (self contentsOfFileNamed: 'MCMockASubclass.class.st' inDirectory: packageDir)
    equals: (self expectedMCMockASubclassClassSt withLineEndings: nl).
  self
    assert:
      (self contentsOfFileNamed: 'MCSnapshotTest.extension.st' inDirectory: packageDir)
    equals: (self expectedMCSnapshotTestExtensionSt withLineEndings: nl)

]

{ #category : 'private' }
TonelAbstractWriterTest >> writerClass [

  self subclassResponsibility

]
