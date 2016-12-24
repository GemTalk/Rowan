"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
updateLoadedClassesFromCategories
	"All loadedClasses are in the ClassRegistry. Some of those classes may have been deleted. 
	Try to resolve each by name. Remove the LoadedClasses that are no longer valid. Update the 
	ones whose names now resolve to a different version in the same ClassHistory."

	CypGsLoadedClass
		allDoSafely: [:loadedClass | loadedClass updateFromImageCategory]