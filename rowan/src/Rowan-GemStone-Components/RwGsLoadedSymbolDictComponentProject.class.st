Class {
	#name : 'RwGsLoadedSymbolDictComponentProject',
	#superclass : 'RwLoadedProject',
	#category : 'Rowan-GemStone-Components'
}

{ #category : 'instance creation' }
RwGsLoadedSymbolDictComponentProject class >> newForProjectReferenceDefinition: aProjectReferenceDefinition [

	^ self new
		initialize;
		initializeForProjectReferenceDefinition: aProjectReferenceDefinition;
		yourself
]

{ #category : 'definitions' }
RwGsLoadedSymbolDictComponentProject >> asDefinition [

	"Create a Definition corresponding to what I define in the image."

	^ RwComponentProjectDefinition
		withProperties: self propertiesForDefinition
		packageDefinitions: self loadedPackageDefinitions
]

{ #category : 'testing' }
RwGsLoadedSymbolDictComponentProject >> canCommit [

	^ self projectRef canCommit
]

{ #category : 'commit log' }
RwGsLoadedSymbolDictComponentProject >> commitLog: logLimit [

	^ self projectRef commitLog: logLimit
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> components [

	^ self projectRef components
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictComponentProject >> initializeForLoadSpecification: aLoadSpecification [

	self initializeForName: aLoadSpecification specName.
	handle := RwProjectReferenceDefinition newForSpecification: aLoadSpecification
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictComponentProject >> initializeForProjectReferenceDefinition: aProjectReferenceDefinition [

	self initializeForName: aProjectReferenceDefinition projectAlias.
	handle := aProjectReferenceDefinition copy
]

{ #category : 'properties' }
RwGsLoadedSymbolDictComponentProject >> loadedCommitId [

	^ self projectRef loadedCommitId
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> loadedComponentDefinitions [

	"Create definitions from all of the components I define, and answer the collection of them"

	| components |
	components := self componenents copy.
	components keysAndValuesDo: [:key :value | components at: key put: value copy ].
	^ components
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> loadedConfigurationNames [

	^ self projectRef loadedConfigurationNames
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> loadedConfigurationNames: configNames [

	"noop - project ref component keys is list of loaded config names"

	"https://github.com/GemTalk/Rowan/issues/308"

	"eventually this method will be completely removed/deprecated"

	| x y |
	(x := configNames asArray sort) = (y := self loadedConfigurationNames asArray sort)
		ifFalse: [ self error: 'The configNames are expected to match the component keys' ]
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> loadedGroupNames [

	^ self projectRef loadedGroupNames
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> loadedGroupNames: groupNames [


	self projectRef loadedGroupNames: groupNames
]

{ #category : 'private' }
RwGsLoadedSymbolDictComponentProject >> markPackageDirty [
	"... indicates that receiver needs to mark it's container as dirty .... "

	self markDirty
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> projectOwnerId [

	^ self propertyAt: 'projectOwnerId'
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> projectOwnerId: aString [

	^ self propertyAt: 'projectOwnerId' put: aString
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> projectRef [

	^ self handle
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> projectUrl [

	"Return the projectUrl used to clone the project"

	^ self projectRef projectUrl
]

{ #category : 'definitions' }
RwGsLoadedSymbolDictComponentProject >> propertiesForDefinition [

	| props |
	props := super propertiesForDefinition.
	props at: 'projectRef' put: handle copy.
	^ props
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> repositoryCommitId [

	^ self projectRef repositoryCommitId
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self projectRef repositoryRoot
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> repositoryRootPath [

	^ self repositoryRoot pathString
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> specification [

self deprecated: 'temporary patch .. sender should send direct message to receiver'.
	^ handle
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> symbolDictNameForPackageNamed: packageName [

	^self projectRef symbolDictNameForPackageNamed: packageName
]

{ #category : 'loading' }
RwGsLoadedSymbolDictComponentProject >> updateLoadedCommitId [

	self projectRef updateLoadedCommitId
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> useGit [

	^ self projectRef useGit
]
