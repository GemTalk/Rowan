Extension { #name : 'RwRowanSampleAbstractTest' }

{ #category : '*rowan-tests-35x' }
RwRowanSampleAbstractTest >> _cloneProjectFromSpecUrl: specUrlString projectsHome: projectsHome [

	^ (RwComponentProjectDefinition newForUrl: specUrlString)
		projectHome: projectsHome;
		clone;
		register;
		yourself.
]

{ #category : '*rowan-tests-35x' }
RwRowanSampleAbstractTest >> _cloneProjectFromSpecUrl: specUrlString projectsHome: projectsHome registerProject: aBool [

	| projectDefinition |
	projectDefinition := (RwComponentProjectDefinition newForUrl: specUrlString)
		projectHome: projectsHome;
		clone;
		yourself.
	aBool ifTrue: [ projectDefinition register ].
]
