writing
writePackagesToDiskFrom: someCypressPackageInformations

	| packageStructure |
	^someCypressPackageInformations do: 
			[:each |
			packageStructure := CypressPackageStructure
						fromPackage: (CypressPackageDefinition named: each name).
			each repository writer writePackageStructure: packageStructure.
			self writeGemStoneFileoutForPackageStructure: packageStructure
				andPackageInformation: each.
			each refresh.
			self saveKnownPackages]