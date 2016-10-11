"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-Comparison
"
comparePackagesFrom: someCypressPackageInformations

	| packageNames savedLocation comparator |
	packageNames := someCypressPackageInformations collect: 
					[:each |
					savedLocation := each savedLocation.
					each name].
	comparator := CypressPackageComparator new.
	comparator comparingPackages: packageNames fromDirectory: savedLocation.
	^comparator getDifferences