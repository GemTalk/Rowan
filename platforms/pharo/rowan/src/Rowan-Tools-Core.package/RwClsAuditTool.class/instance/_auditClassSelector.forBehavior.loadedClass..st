audit
_auditClassSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass
"audit a selector. verify compiled method matches loaded method reference return nil if no problem found"

	^(aLoadedClass loadedMethodAt: aSelector isMeta:  true)
			ifNil: [
				| notification |
				notification := (RwAuditMethodErrorNotification
					method: aSelector 
					isMeta: true
					inClassNamed: aBehavior theNonMetaClass name
					isClassExtension: aLoadedClass isLoadedClassExtension
					intoPackageNamed: aLoadedClass packageName )
						description: 'Missing loaded instance method. ';
						yourself.
				(notification signal) 
					ifTrue: [ (aLoadedClass name ,  ' >> ', aSelector) -> 'Missing loaded class method. ' ]
					ifFalse: [ 
						"don't record audit error"
						nil ] ]
			ifNotNil: [:aLoadedMethod |
				(aBehavior compiledMethodAt: aSelector  otherwise: nil) == aLoadedMethod handle
						ifTrue: [
							| expected actual |
							((expected := aLoadedMethod propertyAt: 'protocol') equalsNoCase: (actual := aBehavior categoryOfSelector:  aSelector ) ) 
								ifTrue: [nil]
								ifFalse: [aLoadedClass name , '#', (aLoadedMethod propertyAt: 'protocol') -> ('Missing class method category for loaded class (expected: ', expected printString, ' actual: ', actual printString, ')') ]
						] 
						ifFalse: [(aLoadedClass name ,  ' >> ', aSelector) -> 'Compiled class method is not identical to loaded class method '] 
			]