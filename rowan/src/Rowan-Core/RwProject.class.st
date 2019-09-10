Class {
	#name : 'RwProject',
	#superclass : 'RowanInterface',
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwProject >> _loadedProject [

	^ Rowan image loadedProjectNamed: self name
]

{ #category : 'private' }
RwProject >> _specification [

	^ self _loadedProject specification
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
RwProject >> comment [

	^ self _specification comment
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

	^self _loadedProject isDirty

]

{ #category : 'actions' }
RwProject >> load [
	"load the receiver into the image"

	^ self _loadedProject load
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

{ #category : 'accessing' }
RwProject >> packageNames [

	^ self _loadedProject packageNames
]

{ #category : 'accessing' }
RwProject >> packages [

	^ self packageNames collect: [ :packageName | RwPackage newNamed: packageName ]
]

{ #category : 'accessing' }
RwProject >> project [

	^ self
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
RwProject >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self _loadedProject repositoryRoot
]

{ #category : 'properties' }
RwProject >> repositoryRootPath [

	^ self repositoryRoot pathString
]

{ #category : 'actions' }
RwProject >> unload [
	"unload the receiver into the image"

	^ self _loadedProject unload
]

{ #category : 'testing' }
RwProject >> useGit [

	^self _loadedProject useGit

]
