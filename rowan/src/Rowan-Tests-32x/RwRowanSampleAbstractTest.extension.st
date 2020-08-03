Extension { #name : 'RwRowanSampleAbstractTest' }

{ #category : '*rowan-tests-32x' }
RwRowanSampleAbstractTest >> _cloneProjectFromSpecUrl: specUrlString projectsHome: projectsHome [

	^ self _cloneProjectFromSpecUrl: specUrlString projectsHome: projectsHome registerProject: true
]

{ #category : '*rowan-tests-32x' }
RwRowanSampleAbstractTest >> _cloneProjectFromSpecUrl: specUrlString projectsHome: projectsHome registerProject: aBool [

	^ Rowan projectTools clone
		cloneSpecification: specUrlString asRwUrl asSpecification
		gitRootPath: projectsHome
		useSsh: true
		registerProject: aBool.
]
