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
RwReadOnlyDiskRepositoryDefinitionV2 >> _sesstionTempsKey [
	^ sesstionTempsKey
		ifNil: [ sesstionTempsKey := 'rwReadOnlyRepositoryKey_' , self asOop printString ]
]

{ #category : 'accessing' }
RwReadOnlyDiskRepositoryDefinitionV2 >> commitId [
	^ commitId ifNil: [ '' ]
]

{ #category : 'accessing' }
RwReadOnlyDiskRepositoryDefinitionV2 >> commitId: aString [
	commitId := aString
]

{ #category : 'accessing' }
RwReadOnlyDiskRepositoryDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ repositoryRoot
		ifNil: [ 
			repositoryUrl
				ifNil: [ self error: 'For a readonly repository, the repositoryUrl must be defined' ]
				ifNotNil: [ :urlString | 
					^ (SessionTemps current
						at: self _sesstionTempsKey
						ifAbsentPut: [ 
							| url |
							url := urlString asRwUrl.
							url scheme = 'file'
								ifFalse:
									[ self error: 'For a readonly repository, the reposityUrl must be a file: url' ]
										url pathString ]) asFileReference ] ]
]

{ #category : 'accessing' }
RwReadOnlyDiskRepositoryDefinitionV2 >> repositoryRoot: pathStringOrReference [
	SessionTemps current removeKey: self _sesstionTempsKey ifAbsent: [  ].
	super repositoryRoot: pathStringOrReference
]
