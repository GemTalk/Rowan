private - moves
addMethodsAddedByClassesOrExtensionsModification: classesOrExtensionsModification inPackage: package toDictionary: dictionary

	classesOrExtensionsModification elementsModified do: 
			[:classModification |
			| classMethodsModification instanceMethodsModification |
			classMethodsModification := classModification classMethodsModification.
			instanceMethodsModification := classModification
						instanceMethodsModification.
			classMethodsModification elementsModified do: 
					[:methodModification |
					methodModification before
						ifNil: 
							[| newMethod |
							newMethod := methodModification after.
							dictionary at: classModification key -> true -> newMethod key
								put: (RwMethodAdditionOrRemoval
										packageDefinition: package
										classDefinitionOrExtension: classModification after
										methodKey: newMethod key
										isMeta: true
										methodsModification: classMethodsModification)]].
			instanceMethodsModification elementsModified do: 
					[:methodModification |
					methodModification before
						ifNil: 
							[| newMethod |
							newMethod := methodModification after.
							dictionary at: classModification key -> false -> newMethod key
								put: (RwMethodAdditionOrRemoval
										packageDefinition: package
										classDefinitionOrExtension: classModification after
										methodKey: newMethod key
										isMeta: false
										methodsModification: instanceMethodsModification)]]]