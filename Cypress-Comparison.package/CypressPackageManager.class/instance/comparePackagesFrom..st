"
notice: This work is protected by copyright. All rights reserved.
category: comparing
"
comparePackagesFrom: someCypressPackageInformations

	^(someCypressPackageInformations
		inject: CypressPackageComparator new
		into: 
			[:comparer :each |
			comparer comparingPackageNamed: each name fromDirectory: each savedLocation])
				getDifferences