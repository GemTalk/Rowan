Extension { #name : 'RwRowanSample2Test' }

{ #category : '*rowan-tests-32x' }
RwRowanSample2Test >> _migration_1_branchName [
	^ 'migration_1'
]

{ #category : '*rowan-tests-35x' }
RwRowanSample2Test >> _migration_2_branchName [
	^ 'migration_2_v2'
]

{ #category : '*rowan-tests-32x' }
RwRowanSample2Test >> _rowanSample2SpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample2.ston'
]
