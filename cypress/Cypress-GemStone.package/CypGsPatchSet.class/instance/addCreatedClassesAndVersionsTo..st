"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - applying
"
addCreatedClassesAndVersionsTo: newClassesByName

	addedClasses do: [:each | each addToDictionary: newClassesByName].
	classesWithNewVersions
		do: [:each | each addToDictionary: newClassesByName]