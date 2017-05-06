"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initialization
"
initialize

	addedPackages := Set new.
	deletedPackages := Set new.
	packagesWithPropertyChanges := Set new.
	deletedClasses := Set new.
	addedClasses := Set new.
	classesWithNewVersions := Set new.
	classesWithClassVariableChanges := Set new.
	classesWithPropertyChanges := Set new.
	deletedMethods := Set new.
	addedMethods := Set new.
	methodsNeedingRecompile := Set new.
	methodsWithPropertyChanges := Set new.
	tempSymbols := SymbolDictionary new.
	createdClasses := Dictionary new.
	errors := Set new