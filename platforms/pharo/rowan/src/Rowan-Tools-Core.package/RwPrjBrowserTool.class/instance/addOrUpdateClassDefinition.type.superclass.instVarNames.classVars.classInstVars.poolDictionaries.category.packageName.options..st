class browsing
addOrUpdateClassDefinition: className type: type superclass: superclassName instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: category packageName: packageName options: optionsArray

	^ self 
		addOrUpdateClassDefinition: className
		type: type 
		superclass: superclassName 
		instVarNames: anArrayOfStrings 
		classVars: anArrayOfClassVars 
		classInstVars: anArrayOfClassInstVars 
		poolDictionaries: anArrayOfPoolDicts 
		category: category 
		packageName: packageName 
		constraints: #() 
		options: optionsArray
