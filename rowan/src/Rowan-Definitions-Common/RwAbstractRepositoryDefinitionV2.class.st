Class {
	#name : 'RwAbstractRepositoryDefinitionV2',
	#superclass : 'RwDefinition',
	#instVars : [
		'projectsHome',
		'repositoryRoot',
		'repositoryUrl',
		'sessionTempsKey'
	],
	#category : 'Rowan-Definitions-Common'
}

{ #category : 'instance creation' }
RwAbstractRepositoryDefinitionV2 class >> newNamed: repositoryName projectsHome: aFileReference repositoryUrl: aRepositoryUrlString [
	^ self new
		name: repositoryName;
		projectsHome: aFileReference;
		repositoryUrl: aRepositoryUrlString;
		yourself
]

{ #category : 'private' }
RwAbstractRepositoryDefinitionV2 >> _safeGetRepositoryRootIfNil: ifNilBlock [
	"When a Rowan extent is created the repository roots for the included projects are not explicitly 
		set, since the disk location is relative to the location of $GEMSTONE and we want to do a late 
		binding on the disk location. The repository instances are owned by SystemUser, so if the 
		first user to log into a system is not SystemUser, the disk locations cannot be lazily set, 
		since the user may not have permission to write to the receiver. Consequently, if the 
		current user cannot write to the repository location we'll resolve the relative location and 
		store the resulting path in session temps."

	^ self _storeRepositoryRootInSessionTemps
		ifTrue: [ SessionTemps current at: self _sessionTempsKey ifAbsentPut: ifNilBlock ]
		ifFalse: [ repositoryRoot ifNil: ifNilBlock ]
]

{ #category : 'private' }
RwAbstractRepositoryDefinitionV2 >> _safeSetRepositoryRoot: pathStringOrReferenceOrNil [
	"When a Rowan extent is created the repository roots for the included projects are not explicitly 
		set, since the disk location is relative to the location of $GEMSTONE and we want to do a late 
		binding on the disk location. The repository instances are owned by SystemUser, so if the 
		first user to log into a system is not SystemUser, the disk locations cannot be lazily set, 
		since the user may not have permission to write to the receiver. Consequently, if the 
		current user cannot write to the repository location we'll resolve the relative location and 
		store the resulting path in session temps."

	^ self _storeRepositoryRootInSessionTemps
		ifTrue: [ 
			pathStringOrReferenceOrNil
				ifNil: [ SessionTemps current removeKey: self _sessionTempsKey ifAbsent: [  ] ]
				ifNotNil: [ 
					SessionTemps current
						at: self _sessionTempsKey
						put: [ pathStringOrReferenceOrNil ] ] ]
		ifFalse: [ repositoryRoot := pathStringOrReferenceOrNil ]
]

{ #category : 'private' }
RwAbstractRepositoryDefinitionV2 >> _sessionTempsKey [
	^ sessionTempsKey
		ifNil: [ sessionTempsKey := ('rwSessionTempsRepositoryKey_' , self asOop printString) asSymbol ]
]

{ #category : 'private' }
RwAbstractRepositoryDefinitionV2 >> _storeRepositoryRootInSessionTemps [
	^ self canBeWritten not
]

{ #category : 'comparing' }
RwAbstractRepositoryDefinitionV2 >> = anObject [
	^ self projectsHome = anObject projectsHome
		and: [ 
			self repositoryRoot = anObject repositoryRoot
				and: [ self repositoryUrl = anObject repositoryUrl ] ]
]

{ #category : 'actions' }
RwAbstractRepositoryDefinitionV2 >> checkAndUpdateRepositoryRevision: aRwProjectLoadSpecificationV2 [
	"noop"
]

{ #category : 'actions' }
RwAbstractRepositoryDefinitionV2 >> diskRepositoryRoot: repositoryRootPathString [
	^ RwDiskRepositoryDefinitionV2
		newNamed: self name
		projectsHome: self projectsHome
		repositoryUrl: 'file:' , repositoryRootPathString
]

{ #category : 'actions' }
RwAbstractRepositoryDefinitionV2 >> gitRepositoryRoot: repositoryRootPathString revision: aString [
	| urlString |
	urlString := 'file:' , repositoryRootPathString.
	^ RwGitRepositoryDefinitionV2
		newNamed: self name
		projectsHome: self projectsHome
		repositoryUrl: urlString
		revision: aString
]

{ #category : 'comparing' }
RwAbstractRepositoryDefinitionV2 >> hash [
	| hashValue |
	hashValue := self projectsHome hash.
	hashValue := hashValue bitXor: self repositoryRoot hash.
	hashValue := hashValue bitXor: self repositoryUrl hash.
	^ hashValue
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> key [

	^properties at: 'name' ifAbsent: [nil]
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> name [

	^ self key
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> name: aString [ 

	properties at: 'name' put: aString
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> projectsHome [
	^ projectsHome
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> projectsHome: aFileReference [
	projectsHome := aFileReference
]

{ #category : 'actions' }
RwAbstractRepositoryDefinitionV2 >> readOnlyRepositoryRoot: repositoryRootPathString commitId: commitId [
	| urlString repo |
	urlString := 'file:' , repositoryRootPathString.
	repo := RwReadOnlyDiskRepositoryDefinitionV2
		newNamed: self name
		projectsHome: self projectsHome
		repositoryUrl: urlString.
	repo commitId: commitId.
	^ repo
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self
		_safeGetRepositoryRootIfNil: [ self _safeSetRepositoryRoot: self projectsHome / self name ]
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> repositoryRoot: pathStringOrReferenceOrNil [
	| pathStringOrReference |
	pathStringOrReference := pathStringOrReferenceOrNil
		ifNil: [ nil ]
		ifNotNil: [ pathStringOrReferenceOrNil asFileReference ].
	^ self _safeSetRepositoryRoot: pathStringOrReference
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> repositoryUrl [
	^ repositoryUrl
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> repositoryUrl: anUrlString [
	repositoryUrl := anUrlString
]

{ #category : 'actions' }
RwAbstractRepositoryDefinitionV2 >> resolve [
	self subclassResponsibility: #'resolve'
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> revision [

	^ ''
]

{ #category : 'testing' }
RwAbstractRepositoryDefinitionV2 >> useGit [
	^ false
]
