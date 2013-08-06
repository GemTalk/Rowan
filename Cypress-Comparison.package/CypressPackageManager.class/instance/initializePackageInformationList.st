initializing
initializePackageInformationList

	packageInformationList := self potentialPackageNames collect: 
					[:each |
					| directory repo |
					directory := knownPackages at: each ifAbsent: [nil].
					repo := directory
								ifNil: [nil]
								ifNotNil: 
									[knownRepositories at: directory
										ifAbsentPut: [CypressFileSystemRepository on: directory]].
					CypressPackageInformation named: each repository: repo]