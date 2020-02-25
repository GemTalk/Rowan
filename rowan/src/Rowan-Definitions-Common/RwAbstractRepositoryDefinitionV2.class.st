Class {
	#name : 'RwAbstractRepositoryDefinitionV2',
	#superclass : 'RwDefinition',
	#instVars : [
		'projectsHome',
		'repositoryRoot',
		'repositoryUrl'
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

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ repositoryRoot
		ifNil: [ 
			repositoryUrl
				ifNotNil: [ :urlString | 
					| url |
					url := urlString asRwUrl.
					url scheme = 'file'
						ifTrue: [ ^ repositoryRoot := url pathString ] ].
			repositoryRoot := self projectsHome / self name ]
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> repositoryRoot: pathStringOrReference [
	repositoryRoot := pathStringOrReference
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
