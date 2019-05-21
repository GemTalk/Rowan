Class {
	#name : 'RwGsLoadedSymbolDictComponentProject',
	#superclass : 'RwLoadedProject',
	#instVars : [
		'loadedComponents'
	],
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
		componentDefinitions: self loadedComponentDefinitions
]

{ #category : 'commit log' }
RwGsLoadedSymbolDictComponentProject >> commitLog: logLimit [

	^ self projectRef commitLog: logLimit
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictComponentProject >> initialize [

	super initialize.
	loadedComponents := KeyValueDictionary new.
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictComponentProject >> initializeForProjectReferenceDefinition: aProjectReferenceDefinition [

	self initializeForName: aProjectReferenceDefinition projectAlias.
	handle := aProjectReferenceDefinition
]

{ #category : 'properties' }
RwGsLoadedSymbolDictComponentProject >> loadedCommitId [

	^ self projectRef loadedCommitId
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> loadedComponentDefinitions [

	"Create definitions from all of the components I define, and answer the collection of them"

	^ self definitionsFor: loadedComponents
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> loadedConfigurationNames: configNames [


	self projectRef loadedConfigurationNames: configNames
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
	props at: 'projectRef' put: handle.
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
