"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
updatePropertiesFromClass
	"Update my properties from the class. Leave unknown properties alone, they may apply to another platform."

	self
		updateSymbolDictionaryFromClass;
		updateClassNameFromClass;
		updateSuperclassNameFromClass;
		updateInstvarNamesFromClass;
		updateClassInstvarNamesFromClass;
		updateClassVariableNamesFromClass;
		updatePoolDictionaryNamesFromClass;
		updateClassTypeFromClass;
		updateOptionsFromClass;
		updateCommentFromClass;
		updateCategoryFromClass