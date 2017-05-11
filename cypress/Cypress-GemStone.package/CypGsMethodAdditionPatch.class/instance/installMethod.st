"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: installing
"
installMethod

	| methodDictionary protocolName |
	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
	methodDictionary at: selector put: compiledMethod.
	protocolName := (methodDefinition propertyAt: 'protocol'
				ifAbsent: ['unknown']) asSymbol.
	(behavior includesCategory: protocolName)
		ifFalse: [behavior addCategory: protocolName].
	behavior moveMethod: selector toCategory: protocolName