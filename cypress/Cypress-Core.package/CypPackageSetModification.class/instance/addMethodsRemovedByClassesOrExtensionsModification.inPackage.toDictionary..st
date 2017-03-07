"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - moves
"
addMethodsRemovedByClassesOrExtensionsModification: classesOrExtensionsModification inPackage: package toDictionary: dictionary

	classesOrExtensionsModification elementsRemoved do: 
			[:oldClass |
			self
				addMethodsDefinedInPackage: package
				classOrExtension: oldClass
				to: dictionary].
	classesOrExtensionsModification elementsModified do: 
			[:classModification |
			classModification classMethodsModification elementsRemoved do: 
					[:oldMethod |
					dictionary at: classModification key -> true -> oldMethod key
						put: package -> classModification before -> oldMethod].
			classModification instanceMethodsModification elementsRemoved do: 
					[:oldMethod |
					dictionary at: classModification key -> false -> oldMethod key
						put: package -> classModification before -> oldMethod]]