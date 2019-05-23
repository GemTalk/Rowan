audit
_auditSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass
"audit an instance selector. return nil if no problem found"

	^(aLoadedClass loadedMethodAt: aSelector isMeta:  aBehavior isMeta)
			ifNil: [
				| notification |
				notification := (RwAuditMethodErrorNotification
					method: aSelector 
					isMeta: false
					inClassNamed: aBehavior theNonMetaClass name
					isClassExtension: aLoadedClass isLoadedClassExtension
					intoPackageNamed: aLoadedClass packageName )
						description: 'Missing loaded instance method. ';
						yourself.
				(notification signal) 
					ifTrue: [ (aLoadedClass name ,  ' >> ', aSelector) -> 'Missing loaded instance method. ' ]
					ifFalse: [ 
						"don't record audit error"
						nil ]  ]
			ifNotNil: [:aLoadedMethod |
				(aBehavior compiledMethodAt: aSelector  otherwise: nil) == aLoadedMethod handle
						ifTrue: [
							| expected actual |
							((expected := aLoadedMethod propertyAt: 'protocol') equalsNoCase: (actual := aBehavior categoryOfSelector:   aSelector ) ) 
								ifTrue: [nil]
								ifFalse: [aLoadedClass name , '#', (aLoadedMethod propertyAt: 'protocol') -> ('Missing instance method category for loaded class (expected: ', expected printString, ' actual: ', actual printString, ')') ]
						] 
						ifFalse: [(aLoadedClass name ,  ' >> ', aSelector) -> 'Compiled instance method is not identical to loaded method. ']
			]