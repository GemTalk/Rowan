"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
scanForNewMethodsByProtocols
	"If we do not have a LoadedMethod for a method in the system, create one and put it in a package
	based on its protocol."

	CypGsLoadedMethod allMethodsDo: 
			[:method |
			| loadedMethod |
			loadedMethod := CypGsLoadedMethod existingForMethod: method.
			loadedMethod == AbsentToken
				ifTrue: 
					[| class loadedClass package |
					class := method inClass.
					loadedClass := CypGsLoadedClass existingForClass: class.
					loadedClass == AbsentToken
						ifTrue: [self error: 'Classes should have been scanned before methods.'].
					loadedMethod := CypGsLoadedMethod forMethod: method.
					"Initially, put the method in the package of its class. If this is wrong, updating from protocol will fix it."
					loadedClass addLoadedMethod: loadedMethod.
					loadedMethod updateFromImageProtocols]]