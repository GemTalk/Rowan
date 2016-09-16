"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: updating
"
loadPackageFrom: aCypressPackageInformation

	| summary loader |
	loader := (CypressSnapshot definitions: aCypressPackageInformation savedDefinitions)
				updatePackage: (CypressPackageDefinition named: aCypressPackageInformation name).
	summary := Dictionary new.

	loader unloadable notEmpty
		ifTrue: [summary at: 'Unloadable' put: (loader unloadable collect: [:each | each printString])].
	loader errors notEmpty
		ifTrue: [summary at: 'Errors' put: (loader errors collect: [:each | each printString])].
	loader requirements notEmpty
		ifTrue: [summary at: 'Missing Requirements' put: loader requirements asArray].

	^summary
