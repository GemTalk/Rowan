loading
loadMethodDefinition

	self actualClass
		compileMethod: self source
		dictionaries: System myUserProfile symbolList
		category: self category
		environmentId: 0
