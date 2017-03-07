"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - moves
"
addMethodsDefinedInPackage: package classOrExtension: class to: dictionary

	class classMethodDefinitions do: 
			[:method |
			dictionary at: class key -> true -> method key
				put: package -> class -> method].
	class instanceMethodDefinitions do: 
			[:method |
			dictionary at: class key -> false -> method key
				put: package -> class -> method]