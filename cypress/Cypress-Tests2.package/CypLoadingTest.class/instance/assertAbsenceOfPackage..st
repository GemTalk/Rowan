"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: support
"
assertAbsenceOfPackage: packageName

	| loadedPackage |
	loadedPackage := Cypress loadedPackageNamed: packageName ifAbsent: [nil].
	self assert: loadedPackage equals: nil