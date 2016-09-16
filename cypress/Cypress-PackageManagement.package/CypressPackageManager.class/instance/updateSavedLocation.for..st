"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: updating
"
updateSavedLocation: aDirectory for: aCypressPackageInformation
	"Update the specified package to reflect the path and repository where the
	 package should be saved."

	aCypressPackageInformation
		updateKnownPackageRepository: (self repositoryOn: aDirectory).
	self saveKnownPackages.
	^nil