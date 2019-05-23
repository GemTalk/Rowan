private - moves
addMethodsRemovedByPackageModification: packageModification inProject: projectModification toDictionary: dictionary

	packageModification before key notNil
	ifTrue: [ 
		"ignore removals for packages that have been added"
		self
			addMethodsRemovedByClassesOrExtensionsModification: packageModification
					classesModification
			inPackage: packageModification before
			inProject: projectModification before
			toDictionary: dictionary.
		self
			addMethodsRemovedByClassesOrExtensionsModification: packageModification
					classExtensionsModification
			inPackage: packageModification before
			inProject: projectModification before
			toDictionary: dictionary ]
