tests
_auditLoadedClassBlock 

	^[:aLoadedClass |
		| ar |
		ar := {}.
		(Rowan globalNamed: aLoadedClass name) 
			ifNil: [
				ar addAll: { 'Class does not exists ' -> aLoadedClass name }.
				GsFile gciLogServer: 'Class does not exist ', aLoadedClass name ]
			ifNotNil: [:aBehavior |
				"check for non-extension methods that are not packaged in Rowan" 
				(aBehavior selectors reject: [:e |  
					((aBehavior categoryOfSelector: e) first == $*) or: [
						aLoadedClass includesSelector: e isMeta: false]]) 
							do: [:aSelector |
								ar addAll: { aSelector  -> ' Missing loaded method '  }.
								GsFile gciLogServer: 'Missing loaded method ', aLoadedClass name, '>>', aSelector ].
				(aBehavior class selectors reject: [:e | 
					((aBehavior class categoryOfSelector: e) first == $*) or: [
						aLoadedClass includesSelector: e isMeta: true]]) 
							do: [:aSelector |
								ar addAll: {aSelector  -> 'Missing loaded class method ' }.
								GsFile gciLogServer: 'Missing loaded method ', aLoadedClass name, ' class>>', aSelector ].
				"look for methods removed outside of Rowan API"
				aLoadedClass 
					loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClass :aLoadedMethod | 
						(aBehavior compiledMethodAt: aLoadedMethod name otherwise: nil) 
							ifNil: [
								ar addAll: { aLoadedMethod name  -> 'Missing compiled method: ' }.
								GsFile gciLogServer: 'Missing compiled method ', loadedClass name, '>>', aLoadedMethod name ] ]
					loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod |
						(aBehavior class compiledMethodAt: aLoadedMethod name otherwise: nil) 
							ifNil: [
								ar addAll: { aLoadedMethod name  -> 'Missing compiled class method ' }.
								GsFile gciLogServer: 'Missing compiled method ', loadedClass name, ' class>>', aLoadedMethod name ] ] ].
		ar ].