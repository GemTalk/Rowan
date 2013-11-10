"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: updating - private
"
determineKnownPackages

	^(packageInformationList select: [:each | each repository notNil])
		inject: Dictionary new
		into: 
			[:dict :each |
			dict
				at: each name put: each savedLocation;
				yourself]