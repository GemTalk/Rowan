"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
moveToPackageNamed: newPackageName
	"I'm being moved from one package to another, or I'm new and being moved into a package.
 	I may need to be in a class extension.
	We may have to create a new LoadedClass, LoadedClassExtension, and/or a new package."

	| newPackage isExtension myClass newLoadedClass |
	loadedClass ifNotNil: [loadedClass removeLoadedMethod: self].	"That takes care of removal from any old package."

	"Now we need something to add ourselves to."
	newPackage := Cypress existingOrNewLoadedPackageNamed: newPackageName.
	myClass := self myNonMetaClass.
	isExtension := newPackage ~= self definingPackageOfClass.
	newLoadedClass := isExtension
				ifTrue: [newPackage loadedExtensionForClass: myClass]
				ifFalse: [newPackage existingLoadedClassForClass: myClass].
	newLoadedClass addLoadedMethod: self