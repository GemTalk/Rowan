"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - applying
"
createNewClassesAndClassVersions
	"Who: Added classes and classes with new versions
	   How: Create the new class or class version. Superclass by identity, not name resolution.
	   Order: Superclasses first.
	   Errors: Collect for reporting later"

	| newClassesByName workList |
	newClassesByName := Dictionary new.
	self addCreatedClassesAndVersionsTo: newClassesByName.
	workList := Set withAll: newClassesByName keys.
	[workList isEmpty] whileFalse: 
			[self createClassesFromWorklist: workList andClassesByName: newClassesByName]