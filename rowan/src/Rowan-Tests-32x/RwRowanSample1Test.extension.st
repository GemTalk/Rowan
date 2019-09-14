Extension { #name : 'RwRowanSample1Test' }

{ #category : '*rowan-tests-32x' }
RwRowanSample1Test >> _issue_345_branch_name [

       ^ 'issue_345'
]

{ #category : '*rowan-tests-32x' }
RwRowanSample1Test >> _rowanSample1LoadSpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/samples/RowanSample1.ston'
]
