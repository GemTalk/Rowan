private - moves
addMethodsAddedByPackageModification: packageModification inProject: projectModification toDictionary: dictionary

	packageModification after key notNil
	ifTrue: [ 
		"ignore additions for packages that have been removed"
		self
			addMethodsAddedByClassesOrExtensionsModification: packageModification
					classesModification
			inPackage: packageModification after
			inProject: projectModification after
			toDictionary: dictionary.
		self
			addMethodsAddedByClassesOrExtensionsModification: packageModification
					classExtensionsModification
			inPackage: packageModification after
			inProject: projectModification after
			toDictionary: dictionary]
