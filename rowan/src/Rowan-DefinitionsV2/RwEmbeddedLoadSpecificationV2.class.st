Class {
	#name : 'RwEmbeddedLoadSpecificationV2',
	#superclass : 'RwLoadSpecificationV2',
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'actions' }
RwEmbeddedLoadSpecificationV2 >> resolveWithParentProject: aResolvedProject [
	"give embedded projects a chance to resolve cleanly"

	| basicProject |
	self projectsHome: aResolvedProject projectsHome.
	basicProject := RwResolvedProjectV2 basicLoadSpecification: self.
	basicProject _projectRepository: aResolvedProject _projectRepository copy.
	self projectsHome: aResolvedProject repositoryRoot.
	^ basicProject resolve
]
