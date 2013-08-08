updating
loadPackageFrom: aCypressPackageInformation

	| summary loader |
	loader := (CypressSnapshot definitions: aCypressPackageInformation savedDefinitions)
				updatePackage: (CypressPackageDefinition named: aCypressPackageInformation name).
	summary := Dictionary new.

	loader unloadable notEmpty ifTrue: [summary at: 'Unloadable count' put: loader unloadable size].
	loader errors notEmpty ifTrue: [summary at: 'Errors count' put: loader errors size].
	loader requirements notEmpty ifTrue: [summary at: 'Missing Requirements count' put: loader requirements size].

	^summary
