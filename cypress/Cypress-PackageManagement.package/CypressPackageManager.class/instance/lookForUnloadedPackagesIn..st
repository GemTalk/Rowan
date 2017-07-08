"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: updating
"
lookForUnloadedPackagesIn: aDirectory
	"Load any package names from aDirectory as known packages.
	 This does not load the package contents."

	self lookForUnloadedPackagesInRepository: (self repositoryOn: aDirectory).
	^nil
