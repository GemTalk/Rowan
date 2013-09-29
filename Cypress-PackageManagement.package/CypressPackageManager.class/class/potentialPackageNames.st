"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
potentialPackageNames
	"Answer a list of 'package names' from classes and methods.
	 The class category is the package name, if the class is in a package at all.
	 The method category begins with an asterisk (*) before the package name,
	 but can be continued with other details (e.g., *PackageName-accessing).
	 This version does NOT recognize method category suffixes."

	| classCategories methodCategories |
	classCategories := Set new.
	methodCategories := Set new.
	System myUserProfile symbolList allSatisfying: 
			[:aClass |
			aClass isBehavior and: 
					[classCategories addAll: (self packageNamePermutationsFor: aClass category).
					aClass categorysDo: 
							[:cat :method |
							cat first = $*
								ifTrue: [methodCategories addAll: (self packageNamePermutationsFor: (cat copyFrom: 2 to: cat size))]].
					false]].
	^(Set new)
		addAll: classCategories;
		addAll: methodCategories;
		remove: 'User Classes';
		remove: 'Kernel';
		sortAscending