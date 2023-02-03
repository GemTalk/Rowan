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
		basicLoadSpecification: handle _loadSpecification copy)
		packages: self loadedPackageDefinitions;
		yourself.
	resolvedProject _projectRepository: handle _projectRepository copy.
	resolvedProject _projectSpecification: handle _projectSpecification copy.
	resolvedProject _projectDefinition
		projectDefinitionSourceProperty:
			RwLoadedProject _projectLoadedDefinitionSourceValue;
		projectDefinitionPlatformConditionalAttributes:
				handle projectDefinitionPlatformConditionalAttributes copy;
		yourself.
	resolvedProject _projectDefinition components: handle _projectStructure copy.	"temporary hack until RwProjectStructure gets defined"
	^ resolvedProject
]

{ #category : 'testing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> canCommit [

	^ self resolvedProject canCommit
]

{ #category : 'actions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> checkout: revision [
	^ self resolvedProject repository checkout: revision
]

{ #category : 'queries' }
RwGsLoadedSymbolDictResolvedProjectV2 >> commitId [

	^ self resolvedProject commitId
]

{ #category : 'commit log' }
RwGsLoadedSymbolDictResolvedProjectV2 >> commitLog: logLimit [

	^ self resolvedProject commitLog: logLimit
]

{ #category : 'querying' }
RwGsLoadedSymbolDictResolvedProjectV2 >> componentForPackageNamed: packageName [
	"Answer nil if no component found"

	^ self components componentForPackageNamed: packageName
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> componentNames [

	^ handle componentNames
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> components [

	^ handle _projectStructure
]

{ #category : 'properties' }
RwGsLoadedSymbolDictResolvedProjectV2 >> defaultSymbolDictName: symDictName [

	self resolvedProject defaultSymbolDictName: symDictName
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictResolvedProjectV2 >> initializeForResolvedProject: aResolvedProject [
	self initializeForName: aResolvedProject projectName.
	handle := aResolvedProject copyForLoadedProject
]

{ #category : 'actions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> load [
	"
		load only the receiver into the image. Required projects for the receiver are only loaded if they are not already 
			present in the image.

		To explicitly load the receiver AND required projects, construct a project set containing projects to be loaded 
			and send #load to the project set.
	"

	^ self asDefinition load
]

{ #category : 'actions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> load: instanceMigrator [
	"
		load only the receiver into the image, using the specified instance migrator. Required projects for the receiver are only 
			loaded if they are not already present in the image.
	"

	^ self asDefinition load: instanceMigrator
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
RwGsLoadedSymbolDictResolvedProjectV2 >> loadedGroupNames [

	^ self resolvedProject loadedGroupNames
]

{ #category : 'actions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> loadProjectSet [
	"
		refresh the contents of the receiver from disk and create a project set that includes project definitions of
			required projects, also read from disk. Then load the entire project set.
	"

	^ self asDefinition loadProjectSet
]

{ #category : 'actions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> loadProjectSet: platformConditionalAttributes [
	"
		refresh the contents of the receiver from disk and create a project set that includes project definitions of
			required projects, also read from disk. Then load the entire project set.

		Use the specified platform conditional attributes when reading the receiver from disk.
	"

	^ self asDefinition loadProjectSet: platformConditionalAttributes
]

{ #category : 'actions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> loadProjectSet: platformConditionalAttributes instanceMigrator: instanceMigrator [
	"
		refresh the contents of the receiver from disk and create a project set that includes project definitions of
			required projects, also read from disk. Then load the entire project set, using the specified 
			instance migrator.

		Use the specified platform conditional attributes when reading the receiver from disk.

		Use the instanceMigrator to handle new versions of any classes that may result from the load.
	"

	^ self asDefinition
		loadProjectSet: platformConditionalAttributes
		instanceMigrator: instanceMigrator
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> loadSpecification [

	^ handle _loadSpecification
]

{ #category : 'private' }
RwGsLoadedSymbolDictResolvedProjectV2 >> markPackageDirty [
	"... indicates that receiver needs to mark it's container as dirty .... "

	self markDirty
]

{ #category : 'properties' }
RwGsLoadedSymbolDictResolvedProjectV2 >> packageConvention [

	^ self resolvedProject packageConvention
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> platformConditionalAttributes [
	"Answer the platformConditionalAttributes used to load the project"

	^ self resolvedProject platformConditionalAttributes
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> projectDefinitionPlatformConditionalAttributes [
	"Answer the projectDefinitionPlatformConditionalAttributes used to load the project"

	^ self resolvedProject projectDefinitionPlatformConditionalAttributes
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
	props at: 'projectRef' put: handle copy.
	^ props
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> remote [

	^ self resolvedProject repository remote
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

{ #category : 'querying' }
RwGsLoadedSymbolDictResolvedProjectV2 >> subcomponentsOf: componentName attributes: attributes ifNone: noneBlock [
	| subcomponents |
	subcomponents := self components
		subcomponentsOf: componentName
		resolvedProject: self
		matchBlock: [ :aComponent | aComponent matchesAttributes: attributes ]
		ifNone: [^ noneBlock value ].
	subcomponents isEmpty
		ifTrue: [ ^ noneBlock value ].
	^ subcomponents
]

{ #category : 'querying' }
RwGsLoadedSymbolDictResolvedProjectV2 >> subcomponentsOf: componentName ifNone: noneBlock [
	^ self
		subcomponentsOf: componentName
		attributes: self platformConditionalAttributes
		ifNone: noneBlock
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
