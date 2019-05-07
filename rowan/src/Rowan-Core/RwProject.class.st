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

{ #category : 'properties' }
RwProject >> canCommit [

	^ self _specification canCommit
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

	^ self _projectTools query classForProjectNamed: self name
]

{ #category : 'testing' }
RwProject >> existsOnDisk [

	Rowan image loadedProjectNamed: self name ifAbsent: [ ^false ].
	^ self repositoryRoot
		ifNil: [ false ]
		ifNotNil: [:fileRef | fileRef exists ]
]

{ #category : 'accessing' }
RwProject >> extendedClasses [

	^ self _projectTools query classExtensionsForProjectNamed: self name
]

{ #category : 'testing' }
RwProject >> isDirty [

	^self _loadedProject isDirty

]

{ #category : 'properties' }
RwProject >> loadedCommitId [

	^ self _loadedProject loadedCommitId
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

	^ self _specification remote
]

{ #category : 'accessing' }
RwProject >> repositoryCommitId [

	| rootPath |
	self useGit
		ifFalse: [ ^ '' ].
	rootPath := self repositoryRootPath.
	rootPath ifNil: [ ^ '' ].
	^ Rowan gitTools gitcommitShaIn: rootPath

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

{ #category : 'testing' }
RwProject >> useGit [

	^self _loadedProject useGit

]
