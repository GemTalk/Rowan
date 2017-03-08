"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - moves
"
addMethodsAddedByClassesOrExtensionsModification: classesOrExtensionsModification inPackage: package toDictionary: dictionary

	classesOrExtensionsModification elementsModified do: 
			[:classModification |
			classModification classMethodsModification elementsModified do: 
					[:methodModification |
					methodModification before
						ifNil: 
							[| newMethod |
							newMethod := methodModification after.
							dictionary at: classModification key -> true -> newMethod key
								put: package -> classModification after -> newMethod]].
			classModification instanceMethodsModification elementsModified do: 
					[:methodModification |
					methodModification before
						ifNil: 
							[| newMethod |
							newMethod := methodModification after.
							dictionary at: classModification key -> false -> newMethod key
								put: package -> classModification after -> newMethod]]]