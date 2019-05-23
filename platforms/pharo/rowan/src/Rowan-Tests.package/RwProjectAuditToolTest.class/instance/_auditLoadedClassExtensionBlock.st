tests
_auditLoadedClassExtensionBlock

^[:aLoadedClassExtension |
		| ar |
		ar := {}.
		(Rowan globalNamed: aLoadedClassExtension name) 
			ifNil: [ 
				ar addAll: { 'Class does not exists '-> aLoadedClassExtension name }.
				GsFile gciLogServer: 'Class does not exist ', aLoadedClassExtension name ]
			ifNotNil: [ :aBehavior |
				| selectors extensionCategoryName categories |
				extensionCategoryName := '*', aLoadedClassExtension loadedPackage name asLowercase.

				categories := (aBehavior _baseCategorys: 0)
					ifNil: [ #() ]
					ifNotNil: [:catDict | catDict keys ].
				(categories
						detect: [:each | each equalsNoCase: extensionCategoryName ]
						ifNone: [ ])
					ifNotNil: [:categoryName |
						selectors := aBehavior selectorsIn: categoryName.
						(selectors reject: [:e | 
							aLoadedClassExtension includesSelector: e isMeta: false]) 
								do: [:aSelector |
									ar addAll: { aSelector -> 'Missing loaded extension method' ->  aSelector }.
									GsFile gciLogServer: 'Missing loaded extension method ', aLoadedClassExtension name, '>>', aSelector ].
						selectors do: [:sel |
							| cMeth lMeth |
							(cMeth := aBehavior compiledMethodAt: sel otherwise: nil) == (lMeth := (aLoadedClassExtension loadedInstanceMethods at: sel) handle)
								ifFalse: [
									ar addAll: { sel -> 'compiled Method is not identical to loaded Method' }.
									GsFile gciLogServer: 
										'Compiled Method (oop: ', 
										cMeth asOop printString, 
										') is not identical to loaded Method (oop: ',
									  lMeth asOop printString, 
										') ', 
										aLoadedClassExtension name, 
										'>>', 
										sel ] ] ]
					ifNil: [ 
						aLoadedClassExtension loadedInstanceMethods isEmpty not
							ifTrue: [
								ar addAll: 
									{('expected methods in the category ', extensionCategoryName printString, ' are missing') -> aLoadedClassExtension name }.
								GsFile gciLogServer: 'expected methods in category ', extensionCategoryName printString, 'for class ', aLoadedClassExtension name printString ] 
				].

				categories := (aBehavior class _baseCategorys: 0)
					ifNil: [ #() ]
					ifNotNil: [:catDict | catDict keys ].
				(categories
						detect: [:each | each equalsNoCase: extensionCategoryName ]
						ifNone: [ ])
					ifNotNil: [:categoryName |
						selectors := aBehavior class selectorsIn: categoryName.
						(selectors reject: [:e | aLoadedClassExtension includesSelector: e isMeta: true]) do: [:aSelector |
								ar addAll: {aSelector -> 'Missing loaded extensions class method: ' }.
								GsFile gciLogServer: 'Missing loaded extension method ', aLoadedClassExtension name, ' class>>', aSelector ].
						selectors do: [:sel |
							| cMeth lMeth |
							(cMeth := aBehavior class compiledMethodAt: sel otherwise: nil) == (lMeth := (aLoadedClassExtension loadedClassMethods at: sel) handle)
								ifFalse: [
									ar addAll: { sel -> 'class compiled Method is not identical to loaded Method' }.
									GsFile gciLogServer: 
										'Compiled Method (oop: ', 
										cMeth asOop printString, 
										') is not identical to loaded Method (oop: ',
									  lMeth asOop printString, 
										') ', 
										aLoadedClassExtension name, 
										' class>>', 
										sel ] ]  ]
					ifNil: [
						aLoadedClassExtension loadedClassMethods isEmpty not
							ifTrue: [ 
								ar addAll: {('expected class methods in the category ', extensionCategoryName printString, ' are missing') -> aLoadedClassExtension name }.
								GsFile gciLogServer: 'expected methods in category ', extensionCategoryName printString, 'for class ', aLoadedClassExtension name printString, ' class' ] 
				].

				"look for methods removed outside of Rowan API"
				aLoadedClassExtension 
					loadedInstanceMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod | 
						(aBehavior compiledMethodAt: aLoadedMethod name otherwise: nil) 
							ifNil: [
								ar addAll: {aLoadedMethod name -> 'Missing compiled method: ' }.
								GsFile gciLogServer: 'Missing compiled  method ', loadedClass name, '>>', aLoadedMethod name ] ]
					loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod |
						(aBehavior class compiledMethodAt: aLoadedMethod name otherwise: nil) 
							ifNil: [
								ar addAll: { aLoadedMethod name -> 'Missing compiled class method: ' }.
								GsFile gciLogServer: 'Missing compiled  class method ', loadedClass name, ' class>>', aLoadedMethod name ] ] ].
		ar ].