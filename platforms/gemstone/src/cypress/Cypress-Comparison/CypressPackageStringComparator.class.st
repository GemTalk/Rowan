"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressPackageStringComparator',
	#superclass : 'Object',
	#instVars : [
		'directoryPackageMap',
		'diskTimestamps',
		'diskSnapshots',
		'imageSnapshots',
		'snapshotDifferences',
		'currentPackageName',
		'currentOperations'
	],
	#category : 'Cypress-Comparison'
}

{ #category : 'instance creation' }
CypressPackageStringComparator class >> comparingPackageNamed: packageName fromDirectory: aDirectory [

	^(self new)
		comparingPackageNamed: packageName fromDirectory: aDirectory;
		yourself

]

{ #category : 'instance creation' }
CypressPackageStringComparator class >> forCypress [

	^(self new)
		comparingPackages: #('Cypress-Definitions' 'Cypress-Mocks' 'Cypress-Structure' 'Cypress-Tests' 'Cypress-GemStoneFileServer' 'Cypress-Comparison')
			fromDirectory: '/opt/git/CypressReferenceImplementation/';
		yourself

]

{ #category : 'instance creation' }
CypressPackageStringComparator class >> new [

	^super new
		initialize;
		yourself

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> addAddition: aCypressModification to: aCollection [
  aCypressModification definition
    classDefinition: [ :classDefinition | self addClassAddition: aCypressModification to: aCollection ]
    methodDefinition: [ :methodDefinition | self addMethodAddition: aCypressModification to: aCollection ]

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> addClassAddition: aCypressModification to: aCollection [
  aCollection
    add:
      {(aCypressModification definition details).
      #'addition'.
      #'class'.
      (aCypressModification definition classDefinitionString)}

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> addClassModification: aCypressModification to: aCollection [
  aCollection
    add:
      {(aCypressModification modification details).
      #'modification'.
      #'class'.
      (aCypressModification obsoletion classDefinitionString).
      (aCypressModification modification classDefinitionString)}

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> addClassRemoval: aCypressModification to: aCollection [
  aCollection
    add:
      {(aCypressModification definition details).
      #'removal'.
      #'class'.
      (aCypressModification definition classDefinitionString)}

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> addMethodAddition: aCypressModification to: aCollection [
  aCollection
    add:
      {(aCypressModification definition details).
      #'addition'.
      #'method'.
      (aCypressModification definition source)}

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> addMethodModification: aCypressModification to: aCollection [
  aCollection
    add:
      {(aCypressModification modification details).
      #'modification'.
      #'method'.
      (aCypressModification obsoletion source).
      (aCypressModification modification source)}

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> addMethodRemoval: aCypressModification to: aCollection [
  aCollection
    add:
      {(aCypressModification definition details).
      #'removal'.
      #'method'.
      (aCypressModification definition source)}

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> addModification: aCypressModification to: aCollection [
  aCypressModification modification
    classDefinition: [ :classDefinition | self addClassModification: aCypressModification to: aCollection ]
    methodDefinition: [ :methodDefinition | self addMethodModification: aCypressModification to: aCollection ]

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> addRemoval: aCypressModification to: aCollection [
  aCypressModification definition
    classDefinition: [ :classDefinition | self addClassRemoval: aCypressModification to: aCollection ]
    methodDefinition: [ :methodDefinition | self addMethodRemoval: aCypressModification to: aCollection ]

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> applyAddition: aCypressAddition [
  self addAddition: aCypressAddition to: self currentOperations

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> applyModification: aCypressModification [
  self addModification: aCypressModification to: self currentOperations

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> applyRemoval: aCypressRemoval [
  self addRemoval: aCypressRemoval to: self currentOperations

]

{ #category : 'comparing' }
CypressPackageStringComparator >> compare [

	diskSnapshots keys do: [:packageName |
		self resetCurrentForPackage: packageName.
		self currentPatchOperations do: [:each | each applyTo: self].
	].
	self resetCurrentForPackage: nil.

]

{ #category : 'initializing' }
CypressPackageStringComparator >> comparingPackages: someNames fromDirectory: aDirectory [
  (directoryPackageMap at: aDirectory ifAbsentPut: [ OrderedCollection new ])
    addAll: someNames.
  someNames
    do: [ :packageName | 
      | reader |
      reader := (CypressFileSystemRepository on: aDirectory) reader
        readPackageStructureForPackageNamed: packageName.
      diskSnapshots at: packageName put: reader packageStructure snapshot.
      imageSnapshots
        at: packageName
        put: (CypressPackageDefinition named: packageName) snapshot ]

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> currentDiskSnapshot [

	^diskSnapshots at: currentPackageName

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> currentImageSnapshot [

	^imageSnapshots at: currentPackageName

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> currentOperations [
  (self dynamicInstVarAt: #'currentOperations')
    ifNil: [ self updateCurrentOperations ].
  ^ self dynamicInstVarAt: #'currentOperations'

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> currentOperations: aDictionary [
  self dynamicInstVarAt: #'currentOperations' put: aDictionary

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> currentPatchOperations [

	^(CypressPatch fromBase: self currentDiskSnapshot toTarget: self currentImageSnapshot) operations.

]

{ #category : 'comparing' }
CypressPackageStringComparator >> getDifferences [

	self compare.
	^self snapshotDifferences

]

{ #category : 'initializing - private' }
CypressPackageStringComparator >> initialize [
  directoryPackageMap := Dictionary new.
  diskTimestamps := Dictionary new.
  diskSnapshots := Dictionary new.
  imageSnapshots := Dictionary new.
  snapshotDifferences := Dictionary new
    at: 'operations' put: Dictionary new;
    yourself

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> resetCurrentForPackage: aStringOrNil [
  currentPackageName := aStringOrNil.
  self currentOperations: nil

]

{ #category : 'accessing' }
CypressPackageStringComparator >> snapshotDifferences [

	^snapshotDifferences

]

{ #category : 'comparing - private' }
CypressPackageStringComparator >> updateCurrentOperations [
  self
    currentOperations:
      ((snapshotDifferences at: 'operations')
        at: currentPackageName
        ifAbsentPut: [ OrderedCollection new ])

]
