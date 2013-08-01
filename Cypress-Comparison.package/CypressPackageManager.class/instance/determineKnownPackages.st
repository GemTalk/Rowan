updating
determineKnownPackages

	^(packageInformationList select: [:each | each savedLocation notEmpty])
		inject: Dictionary new
		into: 
			[:dict :each |
			dict
				at: each name put: each savedLocation;
				yourself]