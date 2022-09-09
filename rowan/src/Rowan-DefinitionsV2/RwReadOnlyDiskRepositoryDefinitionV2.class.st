Class {
	#name : 'RwReadOnlyDiskRepositoryDefinitionV2',
	#superclass : 'RwDiskRepositoryDefinitionV2',
	#instVars : [
		'sesstionTempsKey',
		'commitId'
	],
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'private' }
RwReadOnlyDiskRepositoryDefinitionV2 >> _sessionTempsKey [
	^ sesstionTempsKey
		ifNil: [ sesstionTempsKey := ('rwReadOnlyRepositoryKey_' , self asOop printString) asSymbol ]
]

{ #category : 'actions' }
RwReadOnlyDiskRepositoryDefinitionV2 >> asReadOnlyRepositoryWithCommitId: aCommitId [
	self commitId: aCommitId
]

{ #category : 'accessing' }
RwReadOnlyDiskRepositoryDefinitionV2 >> commitId [
	^ commitId ifNil: [ '' ]
]

{ #category : 'accessing' }
RwReadOnlyDiskRepositoryDefinitionV2 >> commitId: aString [
	commitId := aString
]

{ #category : 'testing' }
RwReadOnlyDiskRepositoryDefinitionV2 >> isReadOnly [
	^ true
]

{ #category : 'accessing' }
RwReadOnlyDiskRepositoryDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The componentsPath, specsPath, and projectsPath are specified relative to the repository root."

	^ repositoryRoot
		ifNil: [ 
			repositoryUrl
				ifNil: [ self error: 'For a read only repository, the repositoryUrl must be defined' ]
				ifNotNil: [ :urlString | 
					^ (SessionTemps current
						at: self _sessionTempsKey
						ifAbsentPut: [ 
							| url |
							url := urlString asRwUrl.
							url scheme = 'file'
								ifFalse:
									[ self error: 'For a read only repository, the reposityUrl must be a file: url' ].
										url pathString ]) asFileReference ] ]
]

{ #category : 'accessing' }
RwReadOnlyDiskRepositoryDefinitionV2 >> repositoryRoot: pathStringOrReference [
	SessionTemps current removeKey: self _sessionTempsKey ifAbsent: [  ].
]

{ #category : 'accessing' }
RwReadOnlyDiskRepositoryDefinitionV2 >> repositoryUrl: urlString [
	SessionTemps current removeKey: self _sessionTempsKey ifAbsent: [  ].
	super repositoryUrl: urlString
]

{ #category : 'actions' }
RwReadOnlyDiskRepositoryDefinitionV2 >> updateLoadSpecWithRepositoryRoot: aLoadSpec [
	"preserve the current repositoryRoot in the loadSpec"

	self repositoryUrl
		ifNil: [ aLoadSpec readOnlyDiskUrl: 'file:' , self repositoryRoot pathString ]
		ifNotNil: [ :url | aLoadSpec readOnlyDiskUrl: url ]
]
