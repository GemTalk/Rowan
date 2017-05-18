"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - applying
"
markPackagesNotDirty
	"All affected packages are now consistent with the source, and should be marked not dirty."

	self
		allPatchesAffectingLivePackagesDo: [:patch | patch markPackageNotDirty]