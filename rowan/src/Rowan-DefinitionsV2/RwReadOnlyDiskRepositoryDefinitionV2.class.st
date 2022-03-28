Class {
	#name : 'RwReadOnlyDiskRepositoryDefinitionV2',
	#superclass : 'RwDiskRepositoryDefinitionV2',
	#instVars : [
		'commitId'
	],
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'private' }
RwReadOnlyDiskRepositoryDefinitionV2 >> _storeRepositoryRootInSessionTemps [
	"always use sessionTemps, because we do not want to ever have a hard-wired reference to
		the disk location ... used for references to standard projects in product tree 
		extent0.rowan.dbf ... since the reference should always be through $GEMSTONE"

	^ true
]

{ #category : 'accessing' }
RwReadOnlyDiskRepositoryDefinitionV2 >> commitId [
	^ commitId ifNil: [ '' ]
]

{ #category : 'accessing' }
RwReadOnlyDiskRepositoryDefinitionV2 >> commitId: aString [
	commitId := aString
]

{ #category : 'actions' }
RwReadOnlyDiskRepositoryDefinitionV2 >> readOnlyRepositoryRoot: repositoryRootPathString commitId: aString [
	self
		repositoryUrl: 'file:' , repositoryRootPathString;
		commitId: aString
]

{ #category : 'accessing' }
RwReadOnlyDiskRepositoryDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self
		_safeGetRepositoryRootIfNil: [ 
			repositoryUrl
				ifNil:
					[ self error: 'For a readonly repository, the repositoryUrl must be defined' ]
						self _safeSetRepositoryRoot: self projectsHome / self name ]
]

{ #category : 'accessing' }
RwReadOnlyDiskRepositoryDefinitionV2 >> repositoryUrl: urlString [
	SessionTemps current removeKey: self _sessionTempsKey ifAbsent: [  ].
	super repositoryUrl: urlString
]
