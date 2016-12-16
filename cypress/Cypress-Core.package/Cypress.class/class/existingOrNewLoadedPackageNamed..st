"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
existingOrNewLoadedPackageNamed: aName

	^self loadedPackageNamed: aName
		ifNone: [self createLoadedPackageNamed: aName]