"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: support
"
assertAbsenceOfPackage: packageName

	self assert: (Cypress loadedPackageNamed: packageName ifNone: [nil])
		equals: nil