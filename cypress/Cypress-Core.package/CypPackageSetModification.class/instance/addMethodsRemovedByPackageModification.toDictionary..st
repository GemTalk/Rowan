"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - moves
"
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