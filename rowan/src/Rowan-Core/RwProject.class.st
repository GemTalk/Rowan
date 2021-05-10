Class {
	#name : 'RwProject',
	#superclass : 'RwAbstractProject',
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwProject >> _concreteProject [
	^ Rowan image loadedProjectNamed: self name
]

{ #category : 'private' }
RwProject >> _loadedComponents [
	^ self _loadedProject loadedComponentDefinitions
]

{ #category : 'private' }
RwProject >> _loadedProject [

	^ Rowan image loadedProjectNamed: self name
]

{ #category : 'private' }
RwProject >> _loadedProjectIfPresent: presentBlock ifAbsent: absentBlock [

	^ Rowan image
		loadedProjectNamed: self name
		ifPresent: presentBlock
		ifAbsent: absentBlock
]

{ #category : 'private' }
RwProject >> _loadSpecification [
	^ self _loadedProject loadSpecification
]

{ #category : 'accessing' }
RwProject >> _projectDefinitionCustomConditionalAttributes [
	"Answer the projectDefinitionPlatformConditionalAttributes used to load the project"

	^ self _loadedProject _projectDefinitionCustomConditionalAttributes
]

{ #category : 'accessing' }
RwProject >> _projectDefinitionCustomConditionalAttributes:customConditionalAttributes [
	"Answer the projectDefinitionCustomConditionalAttributes used to load the project"

	^ self _loadedProject _projectDefinitionCustomConditionalAttributes: customConditionalAttributes
]

{ #category : 'accessing' }
RwProject >> _projectDefinitionPlatformConditionalAttributes [
	"Answer the projectDefinitionPlatformConditionalAttributes used to load the project"

	^ self _loadedProject _projectDefinitionPlatformConditionalAttributes
]

{ #category : 'accessing' }
RwProject >> _projectDefinitionPlatformConditionalAttributes: platformConditionalAttributes [
	"Answer the projectDefinitionPlatformConditionalAttributes used to load the project"

	^ self _loadedProject _projectDefinitionPlatformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'private' }
RwProject >> _projectRepository [
	^ self _loadedProject handle _projectRepository
]

{ #category : 'private' }
RwProject >> _specification [

	^ self _loadedProject specification
]

{ #category : 'components' }
RwProject >> addLoadComponentNamed: componentName [
	"add a new instance of RwLoadComponent to the project components and add the componentName
		to the load spec (i.e., it will be loaded when the load spec is loaded)"

	^ self addLoadComponentNamed: componentName comment: ''
]

{ #category : 'components' }
RwProject >> addLoadComponentNamed: componentName comment: aString [
	"add a new instance of RwLoadComponent to the project components and add the componentName
		to the load spec (i.e., it will be loaded when the load spec is loaded)"

	| projectDefinition component |
	projectDefinition := self defined.
	component := projectDefinition
		addLoadComponentNamed: componentName
		comment: aString.
	projectDefinition load.
	^ component
]

{ #category : 'components' }
RwProject >> addSubcomponentNamed: componentName condition: condition comment: aString toComponentNamed: toComponentName [
	"Add the named subcomponent with the given condition to the named project and add the new component to the toComponentName component"

	"since we are working with a loaded project here, adding a component 
		with a condition, implies that the condition should be applied to the load specification, 
		thus causing the new component to be loaded"

	| projectDefinition component conditionals |
	projectDefinition := self defined.
	component := projectDefinition
		addSubcomponentNamed: componentName
		condition: condition
		comment: aString
		toComponentNamed: toComponentName.
	conditionals := projectDefinition customConditionalAttributes copy.
	conditionals add: condition.
	projectDefinition customConditionalAttributes: conditionals asSet asArray.
	projectDefinition load.
	^ component
]

{ #category : 'components' }
RwProject >> addSubcomponentNamed: componentName condition: condition toComponentNamed: toComponentName [
	"Add the named subcomponent with the given condition to the named project and add the new component to the toComponentName component"

	^ self
		addSubcomponentNamed: componentName
		condition: condition
		comment: ''
		toComponentNamed: toComponentName
]

{ #category : 'querying' }
RwProject >> allClassNamesFor: componentNameOrArrayOfNames [
	| loadedClassNames |
	loadedClassNames := Set new.
	(self allPackageNamesIn: componentNameOrArrayOfNames)
		do: [ :packageName | 
			(self packageNamed: packageName ifAbsent: [  ])
				ifNotNil: [ :loadedPackage | 
					loadedClassNames
						addAll: loadedPackage definedClassNames;
						addAll: loadedPackage extendedClassNames ] ].
	^ loadedClassNames
]

{ #category : 'querying' }
RwProject >> allPackageNamesIn: componentNameOrArrayOfNames [
	^ self _loadedProject allPackageNamesIn: componentNameOrArrayOfNames
]

{ #category : 'actions' }
RwProject >> asDefinition [

	^ self _loadedProject asDefinition
]

{ #category : 'actions' }
RwProject >> audit [
	"run audit on the receiver"

	^ self _loadedProject audit
]

{ #category : 'properties' }
RwProject >> canCommit [

	^ self _loadedProject canCommit
]

{ #category : 'actions' }
RwProject >> checkout: revision [
	^ self _loadedProject checkout: revision
]

{ #category : 'actions' }
RwProject >> commit: message [
	"commit the repository associated with receiver ..."

	^ self _loadedProject commit: message
]

{ #category : 'querying' }
RwProject >> commitId [

	^ self _loadedProject commitId
]

{ #category : 'querying' }
RwProject >> commitLog: logLimit [

	^ self _loadedProject commitLog: logLimit
]

{ #category : 'querying' }
RwProject >> componentForPackageNamed: packageName [
	"Answer nil if no component found"

	^ self _loadedProject componentForPackageNamed: packageName
]

{ #category : 'components' }
RwProject >> componentNamed: componentName [
	^ self _loadedComponents componentNamed: componentName
]

{ #category : 'querying' }
RwProject >> componentNames [
	"list of component names from the load specification used to load the project "

	^ self _loadedProject componentNames
]

{ #category : 'accessing' }
RwProject >> componentsRoot [
	^ self _loadedProject componentsRoot
]

{ #category : 'accessing' }
RwProject >> currentBranchName [

	| rootPath |
	self useGit
		ifFalse: [ ^ '' ].
	rootPath := self repositoryRootPath.
	rootPath ifNil: [ ^ '' ].
	^ Rowan gitTools gitBranchNameIn:rootPath

]

{ #category : 'accessing' }
RwProject >> customConditionalAttributes: anArray [
	self _loadSpecification customConditionalAttributes: anArray
]

{ #category : 'transitions' }
RwProject >> defined [
	^ RwDefinedFromResolvedProject fromLoadedProject: self
]

{ #category : 'accessing' }
RwProject >> definedClasses [

	^ self _projectTools query classesForProjectNamed: self name
]

{ #category : 'testing' }
RwProject >> existsOnDisk [

	^ Rowan image 
		loadedProjectNamed: self name 
			ifPresent: [:loadedProject |
				self repositoryRoot
					ifNil: [ false ]
					ifNotNil: [:fileRef | fileRef exists ] ]
			ifAbsent: [ false ]
]

{ #category : 'accessing' }
RwProject >> extendedClasses [

	^ self _projectTools query classExtensionsForProjectNamed: self name
]

{ #category : 'git support' }
RwProject >> gitCheckout: branchOrSha [
	"git checkout a branch or sha"

	^ Rowan gitTools gitcheckoutIn: self repositoryRoot with: branchOrSha
]

{ #category : 'git support' }
RwProject >> gitCommit: commitComment [
	"git checkout a branch or sha"

	^ self _loadedProject resolvedProject commit: commitComment
]

{ #category : 'git support' }
RwProject >> gitCreateBranch: branchName [
	"git create a new branch"

	^ Rowan gitTools gitcheckoutIn: self repositoryRoot with: ' -b ', branchName
]

{ #category : 'git support' }
RwProject >> gitLog: logLimit [
	"return `git log` report"

	^ Rowan gitTools gitlogtool: 'HEAD' limit: logLimit gitRepoDirectory: self repositoryRoot pathString
]

{ #category : 'git support' }
RwProject >> gitPullRemote: remoteName branch: branchName [
	"git pull remote and branch name"

	^ Rowan gitTools
		gitpullIn: self repositoryRoot pathString
		remote: remoteName
		branch: branchName
]

{ #category : 'git support' }
RwProject >> gitPushRemote: remoteName branch: branchName [
	"git push remote and branch name"

	^ Rowan gitTools
		gitpushIn: self repositoryRoot pathString
		remote: remoteName
		branch: branchName
]

{ #category : 'git support' }
RwProject >> gitShortStatus [
	"`git status --short` returns an empty string if there is nothing to commit"

	^ Rowan gitTools gitstatusIn: self repositoryRoot pathString with: '--short'
]

{ #category : 'git support' }
RwProject >> gitStatus [
	"return standard `git status` report"

	^ Rowan gitTools gitstatusIn: self repositoryRoot pathString with: ''
]

{ #category : 'testing' }
RwProject >> isDirty [
	"a project is dirty if it has changes that are not written to disk, or it's packages 
		have changes that are not written to  disk."

	^ self _loadedProject isDirty
]

{ #category : 'testing' }
RwProject >> isLoaded [
	^ self _loadedProjectIfPresent: [ true ] ifAbsent: [ false ]
]

{ #category : 'actions' }
RwProject >> load: instanceMigrator [
	"
		load only the receiver into the image, using the specified instance migrator. Required projects for the receiver are only 
			loaded if they are not already present in the image.
	"

	^ self _loadedProject load: instanceMigrator
]

{ #category : 'properties' }
RwProject >> loadedCommitId [

	^ self _loadedProject loadedCommitId
]

{ #category : 'querying' }
RwProject >> loadedComponentNames [
	"list of defined components in the components"

	^ self _loadedComponents componentNames
]

{ #category : 'querying' }
RwProject >> loadedSubcomponentsOf: componentName [
	"list of direct subcomponents of the given <componentName> ...includes package groups"

	^ self loadedSubcomponentsOf: componentName ifNone: [ ^ {} ]
]

{ #category : 'querying' }
RwProject >> loadedSubcomponentsOf: componentName attributes: attributes ifNone: noneBlock [
	^ self _loadedProject
		subcomponentsOf: componentName
		attributes: attributes
		ifNone: noneBlock
]

{ #category : 'querying' }
RwProject >> loadedSubcomponentsOf: componentName ifNone: noneBlock [
	"list of direct subcomponents of the given <componentName> ...includes package groups"

	^ self _loadedProject subcomponentsOf: componentName ifNone: noneBlock
]

{ #category : 'actions' }
RwProject >> loadProjectSet [
	"
		refresh the contents of the receiver from disk and create a project set that includes project definitions of
			required projects, also read from disk. Then load the entire project set.
	"

	^ self _loadedProject loadProjectSet
]

{ #category : 'actions' }
RwProject >> loadProjectSet: customConditionalAttributes [
	"
		refresh the contents of the receiver from disk and create a project set that
			includes project definitions of required projects, also read from disk. Then
			load the entire project set.

		Use the specified customConditionalAttributes when reading the receiver from disk.
	"

	^ self _loadedProject loadProjectSet: customConditionalAttributes
]

{ #category : 'actions' }
RwProject >> loadProjectSet: conditionalAttributes instanceMigrator: instanceMigrator [
	"
		refresh the contents of the receiver from disk and create a project set 
			that includes project definitions of required projects, also read from disk. 
			Then load the entire project set, using the specified instance migrator.

		Use the specified conditional attributes when reading the receiver from disk.

		Use the instanceMigrator to handle new versions of any classes that may result from the load.
	"

	^ self _loadedProject
		loadProjectSet: conditionalAttributes
		instanceMigrator: instanceMigrator
]

{ #category : 'components' }
RwProject >> packageGroupNamed: componentName [
	^ self _loadedComponents packageGroupNamed: componentName
]

{ #category : 'components' }
RwProject >> packageGroupNames [
	^ self _loadedComponents packageGroupNames
]

{ #category : 'querying' }
RwProject >> packageNamed: aString ifAbsent: absentBlock [
	^ self packages
		detect: [ :package | package name = aString ]
		ifNone: absentBlock
]

{ #category : 'accessing' }
RwProject >> packages [

	^ self packageNames collect: [ :packageName | RwPackage newNamed: packageName ]
]

{ #category : 'accessing' }
RwProject >> packagesRoot [
	^ self _concreteProject packagesRoot
]

{ #category : 'accessing' }
RwProject >> projectRoots [
	^ self _concreteProject projectRoots
]

{ #category : 'accessing' }
RwProject >> projectsRoot [
	^ self _concreteProject projectsRoot
]

{ #category : 'accessing' }
RwProject >> projectUrl [

	"Return the projectUrl used to clone the project"

	^ self _loadedProject projectUrl
]

{ #category : 'actions' }
RwProject >> reload [
	"
		load the receiver AND required projects.
	"

	^ Rowan projectTools load loadProjectNamed: self name
]

{ #category : 'actions' }
RwProject >> reloadAndIncludeCustomConditionalAttributes: addedCustomConditionalAttributes [
	"
		load the receiver AND required projects adding the custom conditonal attribures to 
			the receiver's list of conditional attributes.
	"

	^ Rowan projectTools load
		loadProjectNamed: self name
		includeCustomConditionalAttributes: addedCustomConditionalAttributes
]

{ #category : 'actions' }
RwProject >> reloadAndRemoveCustomConditionalAttributes: removedCustomConditionalAttributes [
	"
		load the receiver AND required projects removing the custom conditonal attribures from 
			the receiver's list of conditional attributes.
	"

	^ Rowan projectTools load
		loadProjectNamed: self name
		removeCustomConditionalAttributes: removedCustomConditionalAttributes
]

{ #category : 'properties' }
RwProject >> repositoryRootPath [

	^ self repositoryRoot pathString
]

{ #category : 'actions' }
RwProject >> revert [
	"
		read and reload only the receiver into the image. Required projects for the receiver are only loaded if they are not already 
			present in the image.

		To explicitly revert the receiver AND required projects, <TBD>.
	"

	"should replace places where a projectSet was created for the receiver"

	^ self error: 'Not yet implmented'
]

{ #category : 'actions' }
RwProject >> revert: customConditionalAttributes [
	"
		read the reciever using the customConditionalAttributes and reload only the receiver into the image. Required projects for the receiver are only 
			loaded if they are not already present in the image.
	"

	"should replace places where a projectSet was created for the receiver"

	^ self error: 'Not yet implmented'
]

{ #category : 'actions' }
RwProject >> revert: customConditionalAttributes  instanceMigrator: instanceMigrator [
	"
		read the reciever using the customConditionalAttributes and reload only the receiver into the image, using the specifiied instanceMigrator. Required projects for the 
			receiver are only loaded if they are not already present in the image.
	"

	"should replace places where a projectSet was created for the receiver"

	^ self error: 'Not yet implmented'
]

{ #category : 'properties' }
RwProject >> revision [
	^ self _concreteProject revision
]

{ #category : 'actions' }
RwProject >> testSuite [
	^ Rowan projectTools test testSuiteForProjectNamed: self name
]

{ #category : 'actions' }
RwProject >> unload [
	"unload the loaded project (if present) from the image"

	^ self
		_loadedProjectIfPresent: [ :loadedProject | loadedProject unload ]
		ifAbsent: [  ]
]

{ #category : 'testing' }
RwProject >> useGit [

	^self _loadedProject useGit

]
