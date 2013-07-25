unknown
fileOutsForPackagesNamed: someNames

	^someNames inject: Dictionary new
		into: 
			[:result :each |
			result
				at: each
					put: (String streamContents: 
								[:stream |
								(CypressPackageStructure fromPackage: (self named: each))
									fileOutOn: stream]);
				yourself]