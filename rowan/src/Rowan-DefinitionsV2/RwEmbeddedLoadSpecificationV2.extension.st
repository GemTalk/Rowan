Extension { #name : 'RwEmbeddedLoadSpecificationV2' }

{ #category : '*rowan-definitionsV2' }
RwEmbeddedLoadSpecificationV2 >> resolveWithParentProject: aResolvedProject [
	"give embedded projects a chance to resolve cleanly"

	| basicProject |
	self projectsHome: aResolvedProject projectsHome.
	basicProject := RwResolvedProjectV2 basicLoadSpecification: self.
	basicProject _projectRepository: aResolvedProject _projectRepository copy.
	self projectsHome: aResolvedProject repositoryRoot.
	^ basicProject resolve
]
