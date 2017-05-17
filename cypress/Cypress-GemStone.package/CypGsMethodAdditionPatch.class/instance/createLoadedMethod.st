"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-installing
"
createLoadedMethod

	| existing new |
	existing := CypGsLoadedMethod existingForMethod: compiledMethod
				ifAbsent: [nil].
	existing
		ifNotNil: 
			[self
				error: 'Internal error -- existing LoadedMethod found for new method.'].
	new := CypGsLoadedMethod newNamed: selector.
	new
		handle: compiledMethod;
		classIsMeta: isMeta;
		registerYourself.
	^new