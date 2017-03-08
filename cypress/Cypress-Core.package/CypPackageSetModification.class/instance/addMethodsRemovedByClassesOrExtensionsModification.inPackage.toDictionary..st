"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - moves
"
addMethodsRemovedByClassesOrExtensionsModification: classesOrExtensionsModification inPackage: package toDictionary: dictionary

	classesOrExtensionsModification elementsModified do: 
			[:classModification |
			classModification classMethodsModification elementsModified do: 
					[:methodModification |
					methodModification after
						ifNil: 
							[| oldMethod |
							oldMethod := methodModification before.
							dictionary at: classModification key -> true -> oldMethod key
								put: package -> classModification before -> oldMethod]].
			classModification instanceMethodsModification elementsModified do: 
					[:methodModification |
					methodModification after
						ifNil: 
							[| oldMethod |
							oldMethod := methodModification before.
							dictionary at: classModification key -> false -> oldMethod key
								put: package -> classModification after -> oldMethod]]]