Class {
	#name : 'RwEmbeddedLoadSpecificationV2',
	#superclass : 'RwLoadSpecificationV2',
	#category : 'Rowan-SpecificationsV2'
}

{ #category : 'accessing' }
RwEmbeddedLoadSpecificationV2 class >> label [
	^ 'embedded load specification '
]

{ #category : 'testing' }
RwEmbeddedLoadSpecificationV2 >> isEmbedded [
	"answer true if the receiver is an embedded project (i.e., in same git repository as the project that requires the reciever)"

	^ true
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
