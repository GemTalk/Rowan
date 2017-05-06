"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - applying
"
addAndUpdateLoadedPackages
	"Update the LoadedPackages to reflect the results of this patchSet. Does not update the other loadedThings."

	addedPackages
		do: [:packageAdditionPatch | packageAdditionPatch createLoadedPackage].
	packagesWithPropertyChanges do: [:patch | patch updateLoadedPackage]