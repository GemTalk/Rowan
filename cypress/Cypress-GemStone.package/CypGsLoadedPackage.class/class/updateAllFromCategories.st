"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: updating
"
updateAllFromCategories
	"Where tools do not yet fully understand Cypress packages, but do handle class categories and method protocols,
	assign each class and method in the system to some LoadedPackage. This will create LoadedPackages as required."
	"Scan the image for classes and methods. Platform-specific"

	self
		updateLoadedClasses;
		updateLoadedMethods;
		moveClassesToPackageOfCategory;
		moveMethodsToPackageOfProtocolOrClass