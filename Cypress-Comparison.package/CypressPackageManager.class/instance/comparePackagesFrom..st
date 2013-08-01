comparing
comparePackagesFrom: someCypressPackageInformations

	^(someCypressPackageInformations
		inject: CypressPackageComparator new
		into: 
			[:comparer :each |
			comparer comparingPackageNamed: each name fromDirectory: each savedLocation])
				getDifferences