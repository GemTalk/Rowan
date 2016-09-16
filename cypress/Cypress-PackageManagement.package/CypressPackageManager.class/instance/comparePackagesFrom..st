"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
comparePackagesFrom: someCypressPackageInformations

	^(someCypressPackageInformations
		inject: CypressPackageComparator new
		into: 
			[:comparer :each |
			comparer comparingPackageNamed: each name fromDirectory: each savedLocation])
				getDifferences