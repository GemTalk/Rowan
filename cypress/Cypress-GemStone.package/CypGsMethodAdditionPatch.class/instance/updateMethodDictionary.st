"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: installing
"
updateMethodDictionary

	| methodDictionary |
	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
	methodDictionary at: selector put: compiledMethod