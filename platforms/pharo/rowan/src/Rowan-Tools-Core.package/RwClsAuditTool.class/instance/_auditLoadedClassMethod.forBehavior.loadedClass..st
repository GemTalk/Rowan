audit
_auditLoadedClassMethod: aLoadedMethod forBehavior: aClassOrMeta loadedClass: aLoadedClassOrExtension
"verify that compiled method is present for each loaded class method. return nil if no error"
"we already check verifying selectors that compiled method matches loaded method"

		^( aClassOrMeta compiledMethodAt: aLoadedMethod name  otherwise: nil) 
				ifNil: [(aLoadedClassOrExtension name ,  ' >> ', aLoadedMethod name) -> 'Missing compiled class method. ' ]
				ifNotNil: [:aMethod | aMethod == aLoadedMethod handle 
						ifTrue: [((aLoadedMethod propertyAt: 'protocol') equalsNoCase: (aClassOrMeta categoryOfSelector: aMethod selector) ) 
							ifTrue: [nil]
							ifFalse: [aLoadedClassOrExtension name , '#', (aLoadedMethod properties at: 'protocol') -> 'Missing class method category for loaded class']
						]
						ifFalse: [(aClassOrMeta name ,  ' >> ', aLoadedMethod name) -> 'Compiled class method is not identical to loaded class method. ']		
			]
