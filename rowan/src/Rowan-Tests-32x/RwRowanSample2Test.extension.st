Extension { #name : 'RwRowanSample2Test' }

{ #category : '*rowan-tests-32x' }
RwRowanSample2Test >> _rowanSample2SpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample2.ston'
]
