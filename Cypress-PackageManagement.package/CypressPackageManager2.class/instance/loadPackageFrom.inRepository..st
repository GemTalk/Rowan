Loading
loadPackageFrom: aKnownPackageInformation inRepository: aRepository

	| snapshot summary loader |
	snapshot := (aRepository readPackageStructureForPackageNamed: aKnownPackageInformation name)
				snapshot.
	loader := snapshot updatePackage: (CypressPackageDefinition
						named: aKnownPackageInformation name).
	summary := Dictionary new.
	loader unloadable notEmpty
		ifTrue: 
			[summary
				at: 'Unloadable'
				put: (loader unloadable collect: [:each | each printString])].
	loader errors notEmpty
		ifTrue: 
			[summary
				at: 'Errors'
				put: (loader errors collect: [:each | each printString])].
	loader requirements notEmpty
		ifTrue: [summary
				at: 'Missing Requirements'
				put: loader requirements asArray].
	^summary.
