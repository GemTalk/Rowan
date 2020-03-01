Extension { #name : 'RwRowanSampleAbstractTest' }

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwRowanSampleAbstractTest >> _cloneProjectFromSpecUrl: specUrlString projectsHome: projectsHome [
	^ self
		_cloneProjectFromSpecUrl: specUrlString
		projectsHome: projectsHome
		registerProject: true
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwRowanSampleAbstractTest >> _cloneProjectFromSpecUrl: specUrlString projectsHome: projectsHome registerProject: aBool [

	| projectDefinition |
	projectDefinition := (RwComponentProjectDefinition newForUrl: specUrlString)
		projectHome: projectsHome;
		clone;
		yourself.
	aBool ifTrue: [ projectDefinition register ].
	^ projectDefinition
]
