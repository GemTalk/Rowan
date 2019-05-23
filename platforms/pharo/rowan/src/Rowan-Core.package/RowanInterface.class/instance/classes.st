accessing
classes

	| classes |
	classes := IdentitySet new.
	classes
		addAll: self extendedClasses;
		addAll: self definedClasses.
	^ classes