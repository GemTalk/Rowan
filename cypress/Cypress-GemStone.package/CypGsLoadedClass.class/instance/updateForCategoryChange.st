"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
updateForCategoryChange
	"Part of updating from image categories.
	If my category property no longer matches the package I'm in, move me to that package."

	| oldPackageName newPackageName |
	oldPackageName := loadedPackage name.
	newPackageName := self propertyAt: 'category'.
	oldPackageName = newPackageName
		ifFalse: 
			[| newPackage |
			loadedPackage removeLoadedClass: self.
			"Could need to create the new package."
			newPackage := CypGsLoadedPackage packageNamed: newPackageName.
			newPackage addLoadedClass: self]