"
notice: This work is protected by copyright. All rights reserved.
category: writing - needs work
"
writePackagesToDiskFrom: someCypressPackageInformations

	| packageStructure |
	^someCypressPackageInformations do: 
			[:each |
			packageStructure := CypressPackageStructure
						fromPackage: (CypressPackageDefinition named: each name).
			each repository writer writePackageStructure: packageStructure.
"The choice to produce .gs files should be provided by the package and failing that, by the repository."
			(GsFile openWriteOnServer: each savedLocation , each name , '.gs')
				nextPutAll: (String
							streamContents: [:stream | packageStructure fileOutOn: stream]);
				close.
			each refresh.
			self saveKnownPackages]