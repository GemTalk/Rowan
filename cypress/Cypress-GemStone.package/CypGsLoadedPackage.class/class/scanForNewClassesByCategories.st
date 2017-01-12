"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
scanForNewClassesByCategories
	"If we do not have a LoadedClass for a class in the system, create one and put it in a package
	based on its category."

	CypGsLoadedClass allClassesDo: 
			[:class |
			| loadedClass |
			loadedClass := CypGsLoadedClass existingForClass: class.
			loadedClass == AbsentToken
				ifTrue: 
					[| package |
					package := self packageNamed: class category asString.
					loadedClass := CypGsLoadedClass newForClass: class.
					package addLoadedClass: loadedClass.
					loadedClass updateFromImageCategory]]