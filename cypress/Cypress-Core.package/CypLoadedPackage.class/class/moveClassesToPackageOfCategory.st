"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
moveClassesToPackageOfCategory
	"Assign each class in the system to some LoadedPackage. This will create LoadedThings as required."

	self allClassesDo: 
			[:aClass |
			| loadedClass oldPackage newPackage |
			loadedClass := CypGsLoadedClass forClass: aClass.
			newPackage := Cypress
						existingOrNewLoadedPackageNamed: (self categoryOfClass: aClass).
			(oldPackage := loadedClass loadedPackage)
				ifNil: [newPackage addLoadedClass: loadedClass]
				ifNotNil: 
					[oldPackage = newPackage
						ifFalse: 
							[Cypress configuration allowSilentDirtying
								ifTrue: 
									[oldPackage removeLoadedClass: loadedClass.
									newPackage addLoadedClass: loadedClass]
								ifFalse: [self error: 'Would dirty package']]]]