audit
_auditLoadedInstanceMethod: aLoadedMethod forBehavior: aClassOrMeta loadedClass: aLoadedClassOrExtension
"verify that compiled method is present for each loaded instance method. return nil if no error"
"we already check verifying selectors that compiled method matches loaded method"

		^( aClassOrMeta compiledMethodAt: aLoadedMethod name  otherwise: nil) 
				ifNil: [(aLoadedClassOrExtension name ,  ' >> ', aLoadedMethod name) -> 'Missing compiled instance method. ' ]
				ifNotNil: [:aMethod | aMethod == aLoadedMethod handle 
						ifTrue: [((aLoadedMethod propertyAt: 'protocol') equalsNoCase: (aClassOrMeta categoryOfSelector: aMethod selector) ) 
							ifTrue: [nil]
							ifFalse: [aLoadedClassOrExtension name , '#', (aLoadedMethod propertyAt: 'protocol') -> 'Missing method category for loaded class. ']
						]
						ifFalse: [(aClassOrMeta name ,  ' >> ', aLoadedMethod name) -> 'Compiled instance method is not identical to loaded instance method. ']		
			]
