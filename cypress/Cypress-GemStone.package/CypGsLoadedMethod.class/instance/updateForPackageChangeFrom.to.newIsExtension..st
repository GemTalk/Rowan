"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
updateForPackageChangeFrom: oldPackageName to: newPackageName newIsExtension: newIsExtension
	"I'm being moved from one package to another. One or both of these is through a class extension.
	May have to create a new LoadedClass, LoadedExtendedClass, and/or a new package."
	"We know what our old loadedClass or loadedClassExtension is, so removing is simple."

	| newPackage myBehavior myClass packageOfClass newLoadedClass |
	loadedClass ifNotNil: [loadedClass removeLoadedMethod: self].

	"Now we need something to add ourselves to."
	newPackage := Cypress existingOrNewLoadedPackageNamed: newPackageName.
	myBehavior := handle inClass.
	classIsMeta := myBehavior isMeta.
	myClass := myBehavior thisClass.



	newLoadedClass := newIsExtension ifTrue: [] ifFalse: [].
	newLoadedClass := CypGsLoadedClass existingForClass: myClass.
	packageOfClass := newLoadedClass package.
	newPackage = packageOfClass
		ifFalse: 
			["We're moving into a class extension, not the class definition.
			 Might have to create the extension."
			newLoadedClass := newPackage loadedExtensionForClass: myClass.
			newLoadedClass == AbsentToken
				ifTrue: 
					["Create an extension"
					newLoadedClass := CypGsLoadedClassExtension newNamed: myClass name.
					newPackage addLoadedClassExtension: newLoadedClass]].
	loadedClass := newLoadedClass