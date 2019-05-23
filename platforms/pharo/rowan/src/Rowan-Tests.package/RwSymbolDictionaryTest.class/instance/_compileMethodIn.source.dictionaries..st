private
_compileMethodIn: behavior source: sourceString dictionaries: symbolList

	^ behavior
		compileMethod: sourceString
		dictionaries: symbolList
		category: nil
		intoMethodDict: false
		intoCategories: nil
		intoPragmas: nil
		environmentId: 0