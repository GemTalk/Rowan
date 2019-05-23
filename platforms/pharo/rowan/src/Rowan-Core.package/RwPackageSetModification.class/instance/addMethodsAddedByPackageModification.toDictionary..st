private - moves
addMethodsAddedByPackageModification: packageModification toDictionary: dictionary

	self
		addMethodsAddedByClassesOrExtensionsModification: packageModification
				classesModification
		inPackage: packageModification after
		toDictionary: dictionary.
	self
		addMethodsAddedByClassesOrExtensionsModification: packageModification
				classExtensionsModification
		inPackage: packageModification after
		toDictionary: dictionary