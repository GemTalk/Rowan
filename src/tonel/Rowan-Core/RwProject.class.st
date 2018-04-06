Class {
	#name : 'RwProject',
	#superclass : 'RowanInterface',
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwProject >> _loadedProject [

	^ Rowan image loadedProjectNamed: self name
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

	^ Rowan projectTools git gitBranchNameIn: self repositoryRootPath
]

{ #category : 'accessing' }
RwProject >> definedClasses [

	^ self _projectTools query classForProjectNamed: self name
]

{ #category : 'accessing' }
RwProject >> extendedClasses [

	^ self _projectTools query classExtensionsForProjectNamed: self name
]

{ #category : 'properties' }
RwProject >> loadedCommitId [

	^ self _specification loadedCommitId
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

{ #category : 'properties' }
RwProject >> remote [

	^ self _specification remote
]

{ #category : 'accessing' }
RwProject >> repositoryCommitId [

	^ Rowan projectTools git gitcommitShaIn: self repositoryRootPath
]

{ #category : 'properties' }
RwProject >> repositoryRootPath [

	^ self _specification repositoryRootPath
]
