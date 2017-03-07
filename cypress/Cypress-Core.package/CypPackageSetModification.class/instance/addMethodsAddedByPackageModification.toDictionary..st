"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - moves
"
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