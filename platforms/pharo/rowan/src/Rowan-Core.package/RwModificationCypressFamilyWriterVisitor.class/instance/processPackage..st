package writing
processPackage: aPackageModification

	classDefFileNameMap := self _createFileNameMapForClassesOrPackages:  aPackageModification after classDefinitions.
	classExtFileNameMap := self _createFileNameMapForClassesOrPackages:  aPackageModification after classExtensions.
	classDefBeforeFileNameMap := self _createFileNameMapForClassesOrPackages:  aPackageModification before classDefinitions.
	classExtBeforeFileNameMap := self _createFileNameMapForClassesOrPackages:  aPackageModification before classExtensions.

	super processPackage: aPackageModification