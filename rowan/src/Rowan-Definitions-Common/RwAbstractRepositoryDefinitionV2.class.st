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
RwAbstractRepositoryDefinitionV2 >> asReadOnlyRepositoryWithCommitId: aCommitId [
	| urlString repo |
	urlString := 'file:' , self repositoryRoot pathString.
	repo := RwReadOnlyDiskRepositoryDefinitionV2
		newNamed: self name
		projectsHome: self projectsHome
		repositoryUrl: urlString.
	repo commitId: aCommitId.
	^ repo
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
RwAbstractRepositoryDefinitionV2 >> gitRepositoryRoot: repositoryRootPathString relativeRepositoryRoot: aRelativeRepositoryRoot revision: aString [
	"repositoryRootPathString must be string, because we want any env vars to be late bound"

	| urlString |
	repositoryRootPathString isString
		ifFalse: [ self error: 'git repository root must be a string' ].
	urlString := 'file:' , repositoryRootPathString, '/', aRelativeRepositoryRoot.
	^ RwGitRepositoryDefinitionV2
		newNamed: self name
		projectsHome: self projectsHome
		repositoryUrl: urlString
		relativeRepositoryRoot: aRelativeRepositoryRoot
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

	^properties at: #'name' ifAbsent: [nil]
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> name [

	^ self key
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> name: aString [ 

	properties at: #'name' put: aString
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> projectsHome [
	^ projectsHome
		ifNil: [ projectsHome := FileLocator rowanProjectsHome ]
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> projectsHome: aFileReference [
	projectsHome := aFileReference
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The componentsPath, specsPath, and projectsPath are specified relative to the repository root."

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
	"repositoryUrl specifies the repository root for the project (if a file: url)"

	^ repositoryUrl
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> repositoryUrl: anUrlString [
	"repositoryUrl specifies the repository root for the project (if a file: url)"

	repositoryUrl := anUrlString
]

{ #category : 'actions' }
RwAbstractRepositoryDefinitionV2 >> resolveRepository [
	self subclassResponsibility: #'resolve'
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinitionV2 >> revision [

	^ ''
]

{ #category : 'actions' }
RwAbstractRepositoryDefinitionV2 >> updateLoadSpecWithRepositoryRoot: aLoadSpec [
	"preserve the current repositoryRoot in the loadSpec"

	self subclassResponsibility: #'updateLoadSpecWithRepositoryRoot:'
]

{ #category : 'testing' }
RwAbstractRepositoryDefinitionV2 >> useGit [
	^ false
]
