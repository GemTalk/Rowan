"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - applying
"
allPatchesAffectingLivePackagesDo: aBlock
	"Evaluate aBlock with every patch that can affect a package without deleting it, which is a lot."

	
	{addedPackages.
	packagesWithPropertyChanges.
	deletedClasses.
	addedClasses.
	classesWithNewVersions.
	classesWithClassVariableChanges.
	classesWithPropertyChanges.
	deletedMethods.
	addedMethods.
	methodsNeedingRecompile.
	methodsWithPropertyChanges}
			do: [:patches | patches do: aBlock]