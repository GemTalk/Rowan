Class {
	#name : 'RwAbstractProjectDefinitionV2',
	#superclass : 'RwDefinition',
	#instVars : [
		'packages',
		'components',
		'comment'
	],
	#category : 'Rowan-Components'
}

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> _addPackage: aPackageDefinition [

	| key |
	key := aPackageDefinition key.
	(packages includesKey: key) ifTrue: [self error: 'Duplicate package'].
	^ packages at: key put: aPackageDefinition
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> _projectDefinition [
	^ self
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> addOrUpdatePackage: aPackageDefinition [

	| key |
	key := aPackageDefinition key.
	packages at: key put: aPackageDefinition
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> canCommit [

	^ self projectRef canCommit
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

	^ self components 
		at: aComponentName 
		ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> components [
	^ components
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> components: aComponentDefinitionDictionary [
	components := aComponentDefinitionDictionary
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> componentsPath [

	^ self projectRef componentsPath
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> componentsPath: aDirectoryPathString [

	self projectRef componentsPath: aDirectoryPathString
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> groupNames [

	^ self projectRef groupNames
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> groupNames: aDirectoryPathString [

	self projectRef groupNames: aDirectoryPathString
]

{ #category : 'initialization' }
RwAbstractProjectDefinitionV2 >> initialize [
	super initialize.
	packages := Dictionary new.
	components := Dictionary new
]

{ #category : 'testing' }
RwAbstractProjectDefinitionV2 >> isEmpty [
	"Answer true if this definition does not actually define anything."

	^super isEmpty & packages isEmpty
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container."

	^self projectRef projectName
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> loadedCommitId [

	^ self projectRef loadedCommitId
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

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> packagesPath [

	^ self projectRef packagesPath
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> packagesPath: aDirectoryPathString [

	self projectRef packagesPath: aDirectoryPathString
]

{ #category : 'copying' }
RwAbstractProjectDefinitionV2 >> postCopy [

	| oldPackages |
	super postCopy.
	oldPackages := packages.
	packages := Dictionary new.
	oldPackages keysAndValuesDo: [:key : value | packages at: key put: value copy ] .
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> projectRef [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> removePackage: aPackageDefinition [

	| key |
	key := aPackageDefinition key.
	(packages includesKey: key) ifFalse: [self error: 'Package not present.'].
	packages removeKey: key

]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> removePackageNamed: packageName [

	^self removePackage: (self packageNamed: packageName)

]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self repositoryRootPath asFileReference
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> repositoryRootPath [

	^ self projectRef repositoryRootPath
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> repositoryRootPath: rootRepoPath [

	self projectRef repositoryRootPath: rootRepoPath
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> specsPath [

	^ self projectRef specsPath
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> specsPath: aDirectoryPathString [

	self projectRef specsPath: aDirectoryPathString

]
