private - moves
addMethodsRemovedByClassesOrExtensionsModification: classesOrExtensionsModification inPackage: package toDictionary: dictionary

	classesOrExtensionsModification elementsModified do: 
			[:classModification |
			| classMethodsModification instanceMethodsModification |
			classMethodsModification := classModification classMethodsModification.
			instanceMethodsModification := classModification
						instanceMethodsModification.
			classMethodsModification elementsModified do: 
					[:methodModification |
					methodModification after
						ifNil: 
							[| oldMethod |
							oldMethod := methodModification before.
							dictionary at: classModification key -> true -> oldMethod key
								put: (RwMethodAdditionOrRemoval
										packageDefinition: package
										classDefinitionOrExtension: classModification before
										methodKey: oldMethod key
										isMeta: true
										methodsModification: classMethodsModification)]].
			instanceMethodsModification elementsModified do: 
					[:methodModification |
					methodModification after
						ifNil: 
							[| oldMethod |
							oldMethod := methodModification before.
							dictionary at: classModification key -> false -> oldMethod key
								put: (RwMethodAdditionOrRemoval
										packageDefinition: package
										classDefinitionOrExtension: classModification after
										methodKey: oldMethod key
										isMeta: false
										methodsModification: instanceMethodsModification)]]]