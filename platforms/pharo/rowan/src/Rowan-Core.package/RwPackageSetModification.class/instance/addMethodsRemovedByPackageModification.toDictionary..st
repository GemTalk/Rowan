private - moves
addMethodsRemovedByPackageModification: packageModification toDictionary: dictionary

	self
		addMethodsRemovedByClassesOrExtensionsModification: packageModification
				classesModification
		inPackage: packageModification before
		toDictionary: dictionary.
	self
		addMethodsRemovedByClassesOrExtensionsModification: packageModification
				classExtensionsModification
		inPackage: packageModification before
		toDictionary: dictionary