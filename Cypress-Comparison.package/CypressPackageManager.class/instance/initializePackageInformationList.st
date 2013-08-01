initializing
initializePackageInformationList

	packageInformationList := self potentialPackageNames collect: 
					[:each |
					CypressPackageInformation
						named: each
						savedAt: (knownPackages at: each ifAbsent: [''])]