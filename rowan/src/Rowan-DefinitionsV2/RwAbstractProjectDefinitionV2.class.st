Class {
	#name : 'RwAbstractProjectDefinitionV2',
	#superclass : 'RwDefinition',
	#instVars : [
		'packages',
		'components',
		'comment'
	],
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'private' }
RwAbstractProjectDefinitionV2 >> _addPackage: aPackageDefinition [
	^ self
		_addPackage: aPackageDefinition
		ifPresent: [ self error: 'Duplicate package' ]
]

{ #category : 'private' }
RwAbstractProjectDefinitionV2 >> _addPackage: aPackageDefinition ifPresent: presentBlock [
	| key |
	key := aPackageDefinition key.
	(packages includesKey: key)
		ifTrue: [ ^ presentBlock value ].
	^ packages at: key put: aPackageDefinition
]

{ #category : 'private' }
RwAbstractProjectDefinitionV2 >> _projectDefinition [
	^ self
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> comment [

	^ comment
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> comment: aString [

	comment := aString
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> componentNamed: aComponentName [

	^ self componentNamed: aComponentName ifAbsent: [ self error: 'The component named ', aComponentName printString, ' was not found' ]
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> componentNamed: aComponentName ifAbsent: absentBlock [
	^ self components componentNamed: aComponentName ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> components [
	^ components
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> components: aComponentDefinitionDictionary [
	components := aComponentDefinitionDictionary
]

{ #category : 'initialization' }
RwAbstractProjectDefinitionV2 >> initialize [
	super initialize.
	packages := Dictionary new.
	components := RwResolvedLoadComponentsV2 new
]

{ #category : 'testing' }
RwAbstractProjectDefinitionV2 >> isEmpty [
	"Answer true if this definition does not actually define anything."

	^super isEmpty & packages isEmpty
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> packageNamed: aString [

	^ self
		packageNamed: aString
		ifAbsent: [ self error: 'A package named ' , aString printString , ' was not found.' ]
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> packageNamed: aString ifAbsent: absentBlock [

	^ self packages at: aString ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> packageNames [
  ^ self packages keys asArray
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> packages [

	^packages
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> packages: aPackageDefinitionDictionary [

	packages := aPackageDefinitionDictionary
]

{ #category : 'copying' }
RwAbstractProjectDefinitionV2 >> postCopy [

	| oldPackages |
	super postCopy.
	oldPackages := packages.
	packages := Dictionary new.
	oldPackages keysAndValuesDo: [:key : value | packages at: key put: value copy ] .
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> projectDefinitionPlatformConditionalAttributes [
	^ properties
		at: RwLoadedProject _projectDefinitionPlatformConditionalAttributesKey
		ifAbsent: [  ]
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> projectDefinitionPlatformConditionalAttributes: platformConditionalAtttributesOrNil [
	platformConditionalAtttributesOrNil
		ifNil: [ 
			^ properties
				removeKey:
					RwLoadedProject _projectDefinitionPlatformConditionalAttributesKey
				ifAbsent: [  ] ].
	^ properties
		at: RwLoadedProject _projectDefinitionPlatformConditionalAttributesKey
		put: platformConditionalAtttributesOrNil
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> removeComponentNamed: aComponentName [
	^ self components removeComponentNamed: aComponentName
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> removePackage: aPackageDefinition [
	| key |
	key := aPackageDefinition key.
	(packages includesKey: key)
		ifFalse: [ self error: 'Package not present.' ].
	^ packages removeKey: key
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> removePackageNamed: packageName [
	^ self removePackage: (self packageNamed: packageName)
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> removeProjectNamed: aProjectName [
	^ self components removeProjectNamed: aProjectName
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self repositoryRootPath asFileReference
]
