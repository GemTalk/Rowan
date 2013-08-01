writing
writePackagesToDiskFrom: someCypressPackageInformations

	| packageStructure |
	^someCypressPackageInformations do: 
			[:each |
			packageStructure := CypressPackageStructure
						fromPackage: (CypressPackageDefinition named: each name).
			CypressPackageWriter writePackageStructure: packageStructure
				to: each savedLocation.
			(GsFile openWriteOnServer: each savedLocation , each name , '.gs')
				nextPutAll: (String
							streamContents: [:stream | packageStructure fileOutOn: stream]);
				close.
			self updateSavedLocation: each savedLocation for: each]