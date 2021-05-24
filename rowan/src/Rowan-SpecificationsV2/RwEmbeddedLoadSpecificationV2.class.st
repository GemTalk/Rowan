Class {
	#name : 'RwEmbeddedLoadSpecificationV2',
	#superclass : 'RwLoadSpecificationV2',
	#category : 'Rowan-SpecificationsV2'
}

{ #category : 'accessing' }
RwEmbeddedLoadSpecificationV2 class >> label [
	^ 'embedded load specification '
]

{ #category : 'actions' }
RwEmbeddedLoadSpecificationV2 >> produceWithParentProject: aResolvedProject [
	"give embedded projects a chance to resolve cleanly. Return a produced project that has been attached to the disk representation of project."

	self projectsHome: aResolvedProject projectsHome.
	^ super produceWithParentProject: aResolvedProject
]

{ #category : 'to be removed' }
RwEmbeddedLoadSpecificationV2 >> resolveWithParentProject: aResolvedProject [
	"give embedded projects a chance to resolve cleanly"

	| basicProject |
	self projectsHome: aResolvedProject projectsHome.
	basicProject := RwResolvedProjectV2 basicLoadSpecification: self.
	basicProject _projectRepository: aResolvedProject _projectRepository copy.
	self projectsHome: aResolvedProject repositoryRoot.
	basicProject _projectRepository resolve.
	^ basicProject
]
