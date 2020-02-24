Class {
	#name : 'RwGsLoadedSymbolDictResolvedProjectV2',
	#superclass : 'RwLoadedProject',
	#category : 'Rowan-GemStone-CoreV2'
}

{ #category : 'instance creation' }
RwGsLoadedSymbolDictResolvedProjectV2 class >> newForResolvedProject: aResolvedProject [
	^ self new
		initialize;
		initializeForResolvedProject: aResolvedProject;
		yourself
]

{ #category : 'definitions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> asDefinition [
	"Create a Definition corresponding to what I define in the image."

	| resolvedProject |
	resolvedProject := (RwResolvedProjectV2
		loadSpecification: handle _loadSpecification copy)
		packages: self loadedPackageDefinitions;
		yourself.
	resolvedProject _projectDefinition
		projectDefinitionSourceProperty:
			RwLoadedProject _projectLoadedDefinitionSourceValue.
	resolvedProject _projectDefinition components: handle _projectStructure copy.	"temporary hack until RwProjectStructure gets defined"
	^ resolvedProject
]

{ #category : 'testing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> canCommit [

	^ self resolvedProject canCommit
]

{ #category : 'commit log' }
RwGsLoadedSymbolDictResolvedProjectV2 >> commitLog: logLimit [

	^ self resolvedProject commitLog: logLimit
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> components [

	^ self resolvedProject components
]

{ #category : 'properties' }
RwGsLoadedSymbolDictResolvedProjectV2 >> defaultSymbolDictName: symDictName [

	self resolvedProject defaultSymbolDictName: symDictName
]

{ #category : 'properties' }
RwGsLoadedSymbolDictResolvedProjectV2 >> defaultUseSessionMethodsForExtensions: aBool [

	self resolvedProject defaultUseSessionMethodsForExtensions: aBool
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictResolvedProjectV2 >> initializeForResolvedProject: aResolvedProject [
	self initializeForName: aResolvedProject projectName.
	handle := aResolvedProject copyForLoadedProject
]

{ #category : 'properties' }
RwGsLoadedSymbolDictResolvedProjectV2 >> loadedCommitId [

	^ self resolvedProject loadedCommitId
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> loadedComponentDefinitions [
	^ self resolvedProject _projectStructure copy
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> loadedConfigurationNames [

	^ self resolvedProject loadedConfigurationNames
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> loadedConfigurationNames: configNames [

	"noop - project ref component keys is list of loaded config names"

	"https://github.com/GemTalk/Rowan/issues/308"

	"eventually this method will be completely removed/deprecated"

	| x y |
	(x := configNames asArray sort) = (y := self loadedConfigurationNames asArray sort)
		ifFalse: [ self error: 'The configNames are expected to match the component keys' ]
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> loadedGroupNames [

	^ self resolvedProject loadedGroupNames
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> loadedGroupNames: groupNames [


	self resolvedProject loadedGroupNames: groupNames
]

{ #category : 'private' }
RwGsLoadedSymbolDictResolvedProjectV2 >> markPackageDirty [
	"... indicates that receiver needs to mark it's container as dirty .... "

	self markDirty
]

{ #category : 'properties' }
RwGsLoadedSymbolDictResolvedProjectV2 >> packageNameToPlatformPropertiesMap: aDictionary [

	^self resolvedProject packageNameToPlatformPropertiesMap: aDictionary
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> platformConditionalAttributes [
	"Answer the platformConditionalAttributes used to load the project"

	^ self resolvedProject platformConditionalAttributes
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> projectOwnerId [

	^ self propertyAt: 'projectOwnerId'
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> projectOwnerId: aString [

	^ self propertyAt: 'projectOwnerId' put: aString
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> projectUrl [

	"Return the projectUrl used to clone the project"

	^ self resolvedProject projectUrl
]

{ #category : 'definitions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> propertiesForDefinition [

	| props |
	props := super propertiesForDefinition.
self halt.
	props at: 'projectRef' put: handle copy.
	^ props
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> remote [

	^ self resolvedProject remote
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> repositoryCommitId [

	^ self resolvedProject repositoryCommitId
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self resolvedProject repositoryRoot
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> repositoryRootPath [

	^ self repositoryRoot pathString
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> resolvedProject [

	^ self handle
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> specification [

self deprecated: 'temporary patch .. sender should send direct message to receiver'.
	^ handle
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> symbolDictNameForPackageNamed: packageName [

	^self resolvedProject symbolDictNameForPackageNamed: packageName
]

{ #category : 'actions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> unload [
	"unload the receiver from the image"

	| resolvedProject |
	resolvedProject := self asDefinition.
	^ resolvedProject _projectDefinition _deleteTool
		deleteProjectDefinition: resolvedProject
]

{ #category : 'loading' }
RwGsLoadedSymbolDictResolvedProjectV2 >> updateLoadedCommitId [

	self resolvedProject updateLoadedCommitId
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> useGit [

	^ self resolvedProject useGit
]
