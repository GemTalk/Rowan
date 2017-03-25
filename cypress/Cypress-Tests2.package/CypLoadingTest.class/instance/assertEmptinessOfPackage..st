"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: support
"
assertEmptinessOfPackage: packageName

	| loadedPackage |
	loadedPackage := Cypress existingOrNewLoadedPackageNamed: packageName.
	self
		assert: loadedPackage loadedClasses isEmpty;
		assert: loadedPackage loadedClassExtensions isEmpty