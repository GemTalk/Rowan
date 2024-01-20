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

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> _projectComponents [
	^ handle _projectComponents
]

{ #category : 'comparing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> _projectDefinitionForCompare [
	^ self
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> _projectDefinitionPlatformConditionalAttributes [
	"Answer the projectDefinitionPlatformConditionalAttributes used to load the project"

	^ self resolvedProject _projectDefinitionPlatformConditionalAttributes
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> _projectDefinitionPlatformConditionalAttributes: platformConditionalAttributes [

	^ self resolvedProject _projectDefinitionPlatformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> _projectSpecification [
	^ handle _projectSpecification
]

{ #category : 'private' }
RwGsLoadedSymbolDictResolvedProjectV2 >> _requiredProjectNamesForLoadedProject: visitedSet [
	^ self handle _requiredProjectNamesForLoadedProject: visitedSet
]

{ #category : 'querying' }
RwGsLoadedSymbolDictResolvedProjectV2 >> allComponentsIn: componentNameOrArrayOfNames matchBlock: matchBlock notFound: notFoundBlock do: aBlock [
	| visited theBlock componentNames subs |
	visited := IdentitySet new.
	theBlock := [ :component | 
	(visited includes: component)
		ifFalse: [ 
			aBlock value: component.
			visited add: component.
			subs := self
				subcomponentsOf: component name
				matchBlock: matchBlock
				ifNone: notFoundBlock.
			subs do: theBlock ] ].
	componentNames := componentNameOrArrayOfNames _isArray
		ifTrue: [ componentNameOrArrayOfNames ]
		ifFalse: [ {componentNameOrArrayOfNames} ].
	^ componentNames
		do: [ :componentName | 
			subs := self
				subcomponentsOf: componentName
				matchBlock: matchBlock
				ifNone: notFoundBlock.
			subs do: theBlock ]
]

{ #category : 'querying' }
RwGsLoadedSymbolDictResolvedProjectV2 >> allPackageNamesIn: componentNameOrArrayOfNames [
	"filter out any packageNames that aren't loaded in the image ... assumed to be in a packageGroup, where unloaded package names are tolerated"

	| packageNames visited theBlock attributes componentNames components loadedPackageNames |
	loadedPackageNames := self loadedPackages keys asSet.
	visited := IdentitySet new.
	packageNames := Set new.
	attributes := self conditionalAttributes.
	theBlock := [ :component | 
	(visited includes: component)
		ifFalse: [ 
			packageNames addAll: component packageNames.
			visited add: component.
			self subcomponentsOf: component name attributes: attributes do: theBlock ] ].
	componentNames := componentNameOrArrayOfNames _isArray
		ifTrue: [ componentNameOrArrayOfNames ]
		ifFalse: [ {componentNameOrArrayOfNames} ].
	components := self _projectComponents.
	componentNames
		do: [ :componentName | 
			| theComponent |
			theComponent := components
				componentNamed: componentName
				ifAbsent: [ components packageGroupNamed: componentName ].
			theBlock value: theComponent ].
	^ (packageNames * loadedPackageNames) asArray sort
]

{ #category : 'querying' }
RwGsLoadedSymbolDictResolvedProjectV2 >> allPackageNamesIn: componentNameOrArrayOfNames matchBlock: matchBlock notFound: notFoundBlock [
	| packageNames visited theBlock attributes componentNames |
	visited := IdentitySet new.
	packageNames := Set new.
	attributes := self conditionalAttributes.
	theBlock := [ :component | 
	(visited includes: component)
		ifFalse: [ 
			packageNames addAll: component packageNames.
			visited add: component.
			(self
				subcomponentsOf: component name
				matchBlock: matchBlock
				ifNone: notFoundBlock) do: theBlock ] ].
	componentNames := componentNameOrArrayOfNames _isArray
		ifTrue: [ componentNameOrArrayOfNames ]
		ifFalse: [ {componentNameOrArrayOfNames} ].
	componentNames
		do: [ :componentName | 
			| aComponent |
			aComponent := self _projectComponents
				componentNamed: componentName
				ifAbsent: [ notFoundBlock cull: componentName ].
			packageNames addAll: aComponent packageNames.
			visited add: aComponent.
			(self
				subcomponentsOf: componentName
				matchBlock: matchBlock
				ifNone: notFoundBlock) do: theBlock ].
	^ packageNames asArray sort
]

{ #category : 'definitions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> asDefinition [
	"Create a Definition corresponding to what I define in the image."

	| resolvedProject |
	resolvedProject := (RwResolvedProjectV2
		basicLoadSpecification: handle loadSpecification copy)
		packages: self loadedPackageDefinitions;
		yourself.
	resolvedProject _projectRepository: handle _projectRepository copy.
	resolvedProject _projectSpecification: handle _projectSpecification copy.
	resolvedProject _projectDefinition
		projectDefinitionSourceProperty:
				RwLoadedProject _projectLoadedDefinitionSourceValue;
		_projectDefinitionPlatformConditionalAttributes:
				Rowan platformConditionalAttributes;
		yourself.
	resolvedProject _projectComponents: handle _projectComponents copy.
	^ resolvedProject
]

{ #category : 'repositoryConversion' }
RwGsLoadedSymbolDictResolvedProjectV2 >> asReadOnlyRepositoryWithCommitId: aCommitId [
	"convert the existing repository definition to read only ... no change to repository root"

	self resolvedProject asReadOnlyRepositoryWithCommitId: aCommitId
]

{ #category : 'testing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> canCommit [

	^ self resolvedProject canCommit
]

{ #category : 'actions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> checkout: revision [
	^ self resolvedProject repository checkout: revision
]

{ #category : 'testing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> comment [

	^ self resolvedProject comment
]

{ #category : 'queries' }
RwGsLoadedSymbolDictResolvedProjectV2 >> commitId [

	^ self resolvedProject commitId
]

{ #category : 'queries' }
RwGsLoadedSymbolDictResolvedProjectV2 >> commitId: aString [

	^ self resolvedProject commitId: aString
]

{ #category : 'commit log' }
RwGsLoadedSymbolDictResolvedProjectV2 >> commitLog: logLimit [

	^ self resolvedProject commitLog: logLimit
]

{ #category : 'querying' }
RwGsLoadedSymbolDictResolvedProjectV2 >> componentForPackageNamed: packageName [
	"Answer nil if no component found"

	^ self _projectComponents componentForPackageNamed: packageName
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> componentNamed: aComponentName [
	^ self
		componentNamed: aComponentName
		ifAbsent: [ self error: 'No component named ' , aComponentName printString , ' found' ]
]

{ #category : 'querying' }
RwGsLoadedSymbolDictResolvedProjectV2 >> componentNamed: aComponentName ifAbsent: absentBlock [
	^ self _projectComponents componentNamed: aComponentName ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> componentNames [
	"list of component names from the load specification used to load the project "

	^ handle componentNames
]

{ #category : 'properties' }
RwGsLoadedSymbolDictResolvedProjectV2 >> componentsRoot [
	^ self resolvedProject componentsRoot
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> componentsWithDoits [
	^ self _projectComponents componentsWithDoits
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> conditionalAttributes [

	^ self platformConditionalAttributes, self customConditionalAttributes
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> customConditionalAttributes [
	"Answer the customConditionalAttributes used to load the project"

	^ self resolvedProject customConditionalAttributes
]

{ #category : 'actions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> diskRepositoryRoot: repositoryRootPathString [
	self resolvedProject diskRepositoryRoot: repositoryRootPathString
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> diskUrl [
	^ self resolvedProject diskUrl
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> gemstoneDefaultSymbolDictName [

	^ self resolvedProject gemstoneDefaultSymbolDictName
]

{ #category : 'querying' }
RwGsLoadedSymbolDictResolvedProjectV2 >> gemstoneMethodEnvForPackageNamed: packageName [

	^ handle gemstoneMethodEnvForPackageNamed: packageName
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> gemstoneSymbolDictNameForPackageNamed: packageName [

	^self resolvedProject gemstoneSymbolDictNameForPackageNamed: packageName
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName [
	^ handle gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> gitRoot [
	^ self resolvedProject gitRoot
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> gitUrl [
	^ self resolvedProject gitUrl
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictResolvedProjectV2 >> initializeForResolvedProject: aResolvedProject [
	self initializeForName: aResolvedProject projectName.
	handle := aResolvedProject copyForLoadedProject
]

{ #category : 'comparing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> isEmpty [
	"Used during definition compareAgainstBase:"

	^ super isEmpty & self packagesForCompare isEmpty
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
	^ self resolvedProject _projectComponents copy
]

{ #category : 'actions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> loadProjectSet [
	"
		refresh the contents of the receiver from disk and create a project set that includes project definitions of
			required projects, also read from disk. Then load the entire project set.
	"

	| projectDef |
	projectDef := self asDefinition.
	projectDef _validate: projectDef conditionalAttributes.
	^ Rowan projectTools loadV2
		loadProjectSetDefinition:
			(Rowan projectTools readV2
				readProjectSetForProject: projectDef)
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> loadSpecification [

	^ handle loadSpecification
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

{ #category : 'comparing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> packagesForCompare [
	^ self loadedPackages
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> packagesRoot [
	^ self resolvedProject packagesRoot
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> platformConditionalAttributes [
	"Answer the platformConditionalAttributes used to load the project"

	^ self resolvedProject platformConditionalAttributes
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> projectOwnerId [

	^ self propertyAt: #'projectOwnerId'
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> projectOwnerId: aString [

	^ self propertyAt: #'projectOwnerId' put: aString
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> projectRoots [
	^ self resolvedProject projectRoots
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> projectsHome [
	"projects home specifies the disk location where projects cloned/created by the receiver will be located."

	^ self resolvedProject projectsHome
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> projectsHome: aProjectHomeReferenceOrString [
	"projects home specifies the disk location where projects cloned/created by the receiver will be located."

	self resolvedProject projectsHome: aProjectHomeReferenceOrString
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> projectsRoot [
	^ self resolvedProject projectsRoot
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> projectUrl [

	"Return the projectUrl used to clone the project"

	^ self resolvedProject projectUrl
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> projectVersion [
	^ self resolvedProject projectVersion
]

{ #category : 'comparing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> propertiesForCompare [
	| props|
	props := Dictionary new.
	props
		at: 'name' put: handle projectName;
		at: RwLoadedProject _projectDefinitionSourceKey
			put: RwLoadedProject _projectLoadedDefinitionSourceValue;
		at: RwLoadedProject _projectDefinitionPlatformConditionalAttributesKey
			put: handle _projectDefinitionPlatformConditionalAttributes copy;
		yourself.
	^ props
]

{ #category : 'definitions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> propertiesForDefinition [

	| props |
	props := super propertiesForDefinition.
self halt. "do not checkin ??"
	props at: #'projectRef' put: handle copy.
	^ props
]

{ #category : 'actions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> read [
	"
		read receiver's project definition from disk and return the new project definition
	"

	^ self asDefinition read
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> readOnlyDiskUrl [
	^ self resolvedProject readOnlyDiskUrl
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> remote [

	^ self resolvedProject repository remote
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> repositoryRoot [
	"Root directory of the project. The componentsPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self resolvedProject repositoryRoot
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> repositoryRootPath [

	^ self repositoryRoot pathString
]

{ #category : 'queries' }
RwGsLoadedSymbolDictResolvedProjectV2 >> requiredProjectNames [

	^ self resolvedProject requiredProjectNamesForLoadedProject
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> resolvedProject [

	^ self handle
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> revision [
	^ self resolvedProject revision
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> setPropertiesTo: aDictionary [
	"do not store the platform conditionals ... guarantee that loaded projects always 
		reflect the current system platform conditionals (Rowan platformConditionalAttributes)"

	"https://github.com/GemTalk/Rowan/issues/594"

	super setPropertiesTo: aDictionary.
	self _projectDefinitionPlatformConditionalAttributes: nil
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> specification [

self deprecated: 'temporary patch .. sender should send direct message to receiver'.
	^ handle
]

{ #category : 'querying' }
RwGsLoadedSymbolDictResolvedProjectV2 >> subcomponentsOf: componentName attributes: attributes do: aBlock [
	| subcomponents |
	subcomponents := self _projectComponents
		subcomponentsOf: componentName
		matchBlock: [ :aComponent | aComponent matchesAttributes: attributes ]
		ifNone: [ ^ self ].
	^ subcomponents do: aBlock
]

{ #category : 'querying' }
RwGsLoadedSymbolDictResolvedProjectV2 >> subcomponentsOf: componentName attributes: attributes ifNone: noneBlock [
	| subcomponents |
	subcomponents := self _projectComponents
		subcomponentsOf: componentName
		matchBlock: [ :aComponent | aComponent matchesAttributes: attributes ]
		ifNone: [^ noneBlock value ].
	subcomponents isEmpty
		ifTrue: [ ^ noneBlock value ].
	^ subcomponents
]

{ #category : 'querying' }
RwGsLoadedSymbolDictResolvedProjectV2 >> subcomponentsOf: componentName ifNone: noneBlock [
	"list of direct subcomponents of the given <componentName> ...includes package groups"

	^ self
		subcomponentsOf: componentName
		attributes: self conditionalAttributes
		ifNone: noneBlock
]

{ #category : 'querying' }
RwGsLoadedSymbolDictResolvedProjectV2 >> subcomponentsOf: componentName matchBlock: matchBlock ifNone: noneBlock [
	| aComponent subcomponents |
	subcomponents := {}.
	aComponent := self
		componentOrPackageGroupNamed: componentName
		ifAbsent: [ 
			"noneBlock, if it returns, should answer a component"
			noneBlock cull: componentName ].
	(matchBlock value: aComponent)
		ifFalse: [ 
			"The component is not loadable, so ignore it's subcomponents"
			^ subcomponents ].
	aComponent componentNames
		do: [ :subcomponentName | 
			| subcomponent |
			subcomponent := self
				componentOrPackageGroupNamed: subcomponentName
				ifAbsent: [ 
					"noneBlock, if it returns, should answer a component"
					noneBlock cull: subcomponentName ].
			(matchBlock value: subcomponent)
				ifTrue: [ subcomponents add: subcomponent ] ].
	^ subcomponents
]

{ #category : 'actions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> unload [
	"unload the receiver from the image"

	| resolvedProject |
	resolvedProject := self asDefinition.
	^ Rowan projectTools delete deleteProjectDefinition: resolvedProject
]

{ #category : 'loading' }
RwGsLoadedSymbolDictResolvedProjectV2 >> updateLoadedCommitId [

	self resolvedProject updateLoadedCommitId
]

{ #category : 'actions' }
RwGsLoadedSymbolDictResolvedProjectV2 >> updateLoadSpecWithRepositoryRoot: aLoadSpec [
	"preserve the current repositoryRoot in aLoadSpec ... aLoadSpec should not explicitly attached to loaded project"

	self resolvedProject updateLoadSpecWithRepositoryRoot: aLoadSpec
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictResolvedProjectV2 >> useGit [

	^ self resolvedProject useGit
]
