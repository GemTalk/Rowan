"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initialization
"
moveClassesToPackageOfCategory
	"Where tools do not yet fully understand Cypress packages, but do handle class categories and method protocols,
	assign each class in the system to some LoadedPackage. This will create LoadedPackages as required."

	self allClassesDo: 
			[:aClass |
			| loadedClass oldPackage newPackage |
			loadedClass := CypGsLoadedClass forClass: aClass.
			newPackage := Cypress
						existingOrNewLoadedPackageNamed: (self categoryOf: aClass).
			oldPackage := loadedClass package
						ifNil: [^newPackage addLoadedClass: loadedClass].
			oldPackage = newPackage
				ifFalse: 
					[Cypress configuration allowSilentDirtying
						ifTrue: 
							[oldPackage removeLoadedClass: loadedClass.
							newPackage addLoadedClass: loadedClass]
						ifFalse: [self error: 'Would dirty package']]]