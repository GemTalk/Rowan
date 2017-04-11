"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - applying
"
compileMethods

	addedMethods do: [:each | each compileInContext: tempSymbols].
	methodsNeedingRecompile do: [:each | each compileInContext: tempSymbols]