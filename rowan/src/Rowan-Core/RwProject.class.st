Class {
	#name : 'RwProject',
	#superclass : 'RwAbstractProject',
	#category : 'Rowan-Core'
}

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

{ #category : 'private' }
RwProject >> _specification [

	^ self _loadedProject specification
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

{ #category : 'properties' }
RwProject >> checkout: revision [
	^ self _loadedProject checkout: revision
]

{ #category : 'properties' }
RwProject >> comment [

	^ self _specification comment
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
	^ self loadedComponents componentNamed: componentName
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
RwProject >> customConditionalAttributes [
	^ self _loadSpecification customConditionalAttributes
]

{ #category : 'accessing' }
RwProject >> customConditionalAttributes: anArray [
	self _loadSpecification customConditionalAttributes: anArray
]

{ #category : 'transitions' }
RwProject >> defined [
	^ RwDefinedProject fromLoadedProject: self
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
RwProject >> load [
	"
		load only the receiver into the image. Required projects for the receiver are only loaded if they are not already 
			present in the image.

		To explicitly load the receiver AND required projects, construct a project set containing projects to be loaded 
			and send #load to the project set.
	"

	^ self _loadedProject load
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

{ #category : 'properties' }
RwProject >> loadedConfigurationNames [
	"Answer the list of configuration names that were explicitly specified when the project was loaded"

	^ self _loadedProject loadedConfigurationNames
]

{ #category : 'properties' }
RwProject >> loadedGroupNames [
	"Answer the list of group names that were explicitly specified when the project was loaded"

	^ self _loadedProject loadedGroupNames
]

{ #category : 'components' }
RwProject >> loadedTopLevelComponents [
	| lc |
	lc := self loadedComponents.
	^ self componentNames
		collect: [ :componentName | lc componentNamed: componentName ]
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
RwProject >> loadProjectSet: platformConditionalAttributes [
	"
		refresh the contents of the receiver from disk and create a project set that includes project definitions of
			required projects, also read from disk. Then load the entire project set.

		Use the specified platform conditional attributes when reading the receiver from disk.
	"

	^ self _loadedProject loadProjectSet: platformConditionalAttributes
]

{ #category : 'actions' }
RwProject >> loadProjectSet: platformConditionalAttributes instanceMigrator: instanceMigrator [
	"
		refresh the contents of the receiver from disk and create a project set that includes project definitions of
			required projects, also read from disk. Then load the entire project set, using the specified 
			instance migrator.

		Use the specified platform conditional attributes when reading the receiver from disk.

		Use the instanceMigrator to handle new versions of any classes that may result from the load.
	"

	^ self _loadedProject
		loadProjectSet: platformConditionalAttributes
		instanceMigrator: instanceMigrator
]

{ #category : 'querying' }
RwProject >> packageNamed: aString ifAbsent: absentBlock [
	^ self packages
		detect: [ :package | package name = aString ]
		ifNone: absentBlock
]

{ #category : 'accessing' }
RwProject >> packageNames [

	^ self _loadedProject packageNames
]

{ #category : 'accessing' }
RwProject >> packages [

	^ self packageNames collect: [ :packageName | RwPackage newNamed: packageName ]
]

{ #category : 'accessing' }
RwProject >> projectDefinitionPlatformConditionalAttributes [

	^ self _loadedProject projectDefinitionPlatformConditionalAttributes
]

{ #category : 'accessing' }
RwProject >> projectUrl [

	"Return the projectUrl used to clone the project"

	^ self _loadedProject projectUrl
]

{ #category : 'properties' }
RwProject >> remote [

	^ self _loadedProject remote
]

{ #category : 'accessing' }
RwProject >> repositoryCommitId [

	^ self _loadedProject repositoryCommitId

]

{ #category : 'properties' }
RwProject >> repositoryRootPath [

	^ self repositoryRoot pathString
]

{ #category : 'components' }
RwProject >> subcomponentsOf: componentName [
	| lc |
	lc := self loadedComponents.
	^ (lc componentNamed: componentName) componentNames
		collect: [ :subcomponentName | lc componentNamed: subcomponentName ]
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
RwProject >> revert: platformConditionalAttributes [
	"
		read the reciever using the platformConditionalAttributes and reload only the receiver into the image. Required projects for the receiver are only 
			loaded if they are not already present in the image.
	"

	"should replace places where a projectSet was created for the receiver"

	^ self error: 'Not yet implmented'
]

{ #category : 'actions' }
RwProject >> revert: platformConditionalAttributes  instanceMigrator: instanceMigrator [
	"
		read the reciever using the platformConditionalAttributes and reload only the receiver into the image, using the specifiied instanceMigrator. Required projects for the 
			receiver are only loaded if they are not already present in the image.
	"

	"should replace places where a projectSet was created for the receiver"

	^ self error: 'Not yet implmented'
]

{ #category : 'actions' }
RwProject >> testSuite [
	^ Rowan projectTools test testSuiteForProjectNamed: self name
]

{ #category : 'components' }
RwProject >> topLevelComponents [
	^ self loadedComponents components values
		select: [ :each | each class == RwSimpleProjectLoadComponentV2 ]
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
