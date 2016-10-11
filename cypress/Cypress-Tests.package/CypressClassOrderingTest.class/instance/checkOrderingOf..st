"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: support
"
checkOrderingOf: someClass

	| class superclass inheritance |
	class := someClass.
	inheritance := OrderedCollection with: class.
	
	[superclass := class superclass.
	superclass isNil] whileFalse: 
				[(unorderedClasses includes: superclass)
					ifTrue: [inheritance add: superclass].
				class := superclass].
	1 to: inheritance size - 1
		do: 
			[:subIndex |
			| classIndex superclassIndex |
			classIndex := orderedClasses indexOf: (inheritance at: subIndex).
			superclassIndex := orderedClasses indexOf: (inheritance at: subIndex + 1).
			self
				assert: classIndex > 0;
				assert: superclassIndex > 0;
				assert: classIndex > superclassIndex]