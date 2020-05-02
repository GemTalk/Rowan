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
RwProject >> componentForPackageNamed: packageName [
	"Answer nil if no component found"

	^ self _loadedProject componentForPackageNamed: packageName
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

{ #category : 'exporting' }
RwProject >> exportSpecification [

	self _loadedProject asDefinition exportSpecification
]

{ #category : 'accessing' }
RwProject >> extendedClasses [

	^ self _projectTools query classExtensionsForProjectNamed: self name
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

{ #category : 'project creation' }
RwProject >> register [
	"Create a loaded project based on the receiver's properties in the image."

	self _specification register

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
