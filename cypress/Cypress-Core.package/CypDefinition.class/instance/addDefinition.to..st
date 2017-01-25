"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
addDefinition: aDefinition to: aDictionary

	| key |
	key := aDefinition key.
	(aDictionary includesKey: key)
		ifTrue: [self error: 'Duplicate definition.'].
	aDictionary at: key put: aDefinition