Class {
	#name : 'RwPrjCreateToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'project repository creation' }
RwPrjCreateToolV2 >> createResolvedProjectRepository: resolvedRepository [
	"Create create new repository on `disk`, based on the given resolved project, if it does not already exist."

	resolvedRepository repositoryRoot ensureCreateDirectory.
	resolvedRepository projectRoots do: [ :path | path ensureCreateDirectory ]
]
