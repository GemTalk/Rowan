accessing
conditionalPackageMapSpecsAtGemStoneUserId: userId 

	^ ((self conditionalPackageMapSpecs at: 'gemstone' ifAbsent: [ ^ Dictionary new ])
		at: userId ifAbsent: [ ^ Dictionary new ])
