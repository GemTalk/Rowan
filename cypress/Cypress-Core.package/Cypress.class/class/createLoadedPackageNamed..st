"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
createLoadedPackageNamed: aName
	"Create and answer a new package."

	^loadedPackages at: aName
		put: (platform loadedPackageClass newNamed: aName)