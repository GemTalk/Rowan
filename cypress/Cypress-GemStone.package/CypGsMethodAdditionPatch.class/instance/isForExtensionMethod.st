"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-testing
"
isForExtensionMethod
	"Answer true if the method is not in the same package that defines the class."

	| packageOfClass packageOfMethod loadedClass |
	packageOfMethod := packageDefinition key.
	loadedClass := CypGsLoadedClass existingForClass: behavior thisClass.
	packageOfClass := loadedClass loadedPackage name.
	^packageOfClass ~= packageOfMethod