"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: unknown
"
scanForNewMethodsByProtocols
	"If we do not have a LoadedMethod for a method in the system, create one and put it in a package
	based on its category."

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
					loadedMethod := CypGsLoadedMethod newNamed: method selector asString.
					"Assume method is in package of class. updateForProtocolChange 
					will correct if it's really an extension method."
					loadedClass addLoadedMethod: loadedMethod.
					loadedMethod
						handle: method;
						updateForProtocolChange]]