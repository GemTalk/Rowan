Class {
	#name : 'RwGsLoadedSymbolDictComponentProjectV2',
	#superclass : 'RwLoadedProject',
	#category : 'Rowan-GemStone-Components'
}

{ #category : 'instance creation' }
RwGsLoadedSymbolDictComponentProjectV2 class >> newForProjectReferenceDefinition: aProjectReferenceDefinition [

	^ self new
		initialize;
		initializeForProjectReferenceDefinition: aProjectReferenceDefinition;
		yourself
]

{ #category : 'definitions' }
RwGsLoadedSymbolDictComponentProjectV2 >> asDefinition [
	"Create a Definition corresponding to what I define in the image."

	^ (RwProjectDefinitionV2
		fromLoadSpecification: handle projectLoadSpecification copy)
		packages: self loadedPackageDefinitions;
		yourself
]

{ #category : 'testing' }
RwGsLoadedSymbolDictComponentProjectV2 >> canCommit [

	^ self projectRef canCommit
]

{ #category : 'commit log' }
RwGsLoadedSymbolDictComponentProjectV2 >> commitLog: logLimit [

	^ self projectRef commitLog: logLimit
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> components [

	^ self projectRef components
]

{ #category : 'properties' }
RwGsLoadedSymbolDictComponentProjectV2 >> defaultSymbolDictName: symDictName [

	self projectRef defaultSymbolDictName: symDictName
]

{ #category : 'properties' }
RwGsLoadedSymbolDictComponentProjectV2 >> defaultUseSessionMethodsForExtensions: aBool [

	self projectRef defaultUseSessionMethodsForExtensions: aBool
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictComponentProjectV2 >> initializeForLoadSpecification: aLoadSpecification [

	self initializeForName: aLoadSpecification specName.
	handle := RwProjectReferenceDefinition newForSpecification: aLoadSpecification
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictComponentProjectV2 >> initializeForProjectReferenceDefinition: aProjectReferenceDefinition [

	self initializeForName: aProjectReferenceDefinition projectAlias.
	handle := aProjectReferenceDefinition copy
]

{ #category : 'properties' }
RwGsLoadedSymbolDictComponentProjectV2 >> loadedCommitId [

	^ self projectRef loadedCommitId
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> loadedComponentDefinitions [

	"Create definitions from all of the components I define, and answer the collection of them"

	| components |
	components := self components copy.
	components keysAndValuesDo: [:key :value | components at: key put: value copy ].
	^ components
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> loadedConfigurationNames [

	^ self projectRef loadedConfigurationNames
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> loadedConfigurationNames: configNames [

	"noop - project ref component keys is list of loaded config names"

	"https://github.com/GemTalk/Rowan/issues/308"

	"eventually this method will be completely removed/deprecated"

	| x y |
	(x := configNames asArray sort) = (y := self loadedConfigurationNames asArray sort)
		ifFalse: [ self error: 'The configNames are expected to match the component keys' ]
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> loadedGroupNames [

	^ self projectRef loadedGroupNames
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> loadedGroupNames: groupNames [


	self projectRef loadedGroupNames: groupNames
]

{ #category : 'private' }
RwGsLoadedSymbolDictComponentProjectV2 >> markPackageDirty [
	"... indicates that receiver needs to mark it's container as dirty .... "

	self markDirty
]

{ #category : 'properties' }
RwGsLoadedSymbolDictComponentProjectV2 >> packageNameToPlatformPropertiesMap: aDictionary [

	^self projectRef packageNameToPlatformPropertiesMap: aDictionary
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> projectOwnerId [

	^ self propertyAt: 'projectOwnerId'
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> projectOwnerId: aString [

	^ self propertyAt: 'projectOwnerId' put: aString
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> projectRef [

	^ self handle
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> projectUrl [

	"Return the projectUrl used to clone the project"

	^ self projectRef projectUrl
]

{ #category : 'definitions' }
RwGsLoadedSymbolDictComponentProjectV2 >> propertiesForDefinition [

	| props |
	props := super propertiesForDefinition.
	props at: 'projectRef' put: handle copy.
	^ props
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> remote [

	^ self projectRef remote
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> repositoryCommitId [

	^ self projectRef repositoryCommitId
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self projectRef repositoryRoot
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> repositoryRootPath [

	^ self repositoryRoot pathString
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> specification [

self deprecated: 'temporary patch .. sender should send direct message to receiver'.
	^ handle
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> symbolDictNameForPackageNamed: packageName [

	^self projectRef symbolDictNameForPackageNamed: packageName
]

{ #category : 'loading' }
RwGsLoadedSymbolDictComponentProjectV2 >> updateLoadedCommitId [

	self projectRef updateLoadedCommitId
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProjectV2 >> useGit [

	^ self projectRef useGit
]
