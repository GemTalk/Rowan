"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
addPackage: aPackageDefinition

	| key |
	key := aPackageDefinition key.
	(packages includesKey: key) ifTrue: [self error: 'Duplicate package'].
	packages at: key put: aPackageDefinition