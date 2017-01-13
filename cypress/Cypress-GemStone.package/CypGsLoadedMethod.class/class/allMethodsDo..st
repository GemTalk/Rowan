"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: enumeration
"
allMethodsDo: aBlock
	"Iterate over all methods defined in classes visible in the current session's transient symbol list, 
		or in the corresponding metaclass, evaluating the block with each."

	CypGsLoadedClass allClassesDo: 
			[:class |
			self
				allMethodsInBehavior: class do: aBlock;
				allMethodsInBehavior: class class do: aBlock]