"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
updateForPackageChangeFrom: oldPackageName to: newPackageName
	"I'm being moved from one package to another. One or both of these is through a class extension."

	| newPackage myClass packageOfClass |
	loadedClass removeLoadedMethod: self.
	newPackage := CypGsLoadedPackage packageNamed: newPackageName.
	newPackage
		ifNil: [newPackage := CypGsLoadedPackage newNamed: newPackageName].
	myClass := handle inClass.
	packageOfClass := (CypGsLoadedClass existingForClass: myClass) package.
	newPackage = packageOfClass
		ifFalse: 
			["We're moving into a class extension. Might have to create the extension."
			self halt]
		ifTrue: 
			["We're not moving to an extension."
			self halt]