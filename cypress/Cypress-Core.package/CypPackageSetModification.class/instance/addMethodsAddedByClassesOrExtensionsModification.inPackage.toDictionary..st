"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - moves
"
addMethodsAddedByClassesOrExtensionsModification: classesOrExtensionsModification inPackage: package toDictionary: dictionary

	classesOrExtensionsModification elementsAdded do: 
			[:newClass |
			self
				addMethodsDefinedInPackage: package
				classOrExtension: newClass
				to: dictionary].
	classesOrExtensionsModification elementsModified do: 
			[:classModification |
			classModification classMethodsModification elementsAdded do: 
					[:newMethod |
					dictionary at: classModification key -> true -> newMethod key
						put: package -> classModification after -> newMethod].
			classModification instanceMethodsModification elementsAdded do: 
					[:newMethod |
					dictionary at: classModification key -> false -> newMethod key
						put: package -> classModification after -> newMethod]]