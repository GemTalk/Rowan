"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: sorting
"
determineClassHierarchicalOrder2: someClasses
	"Returns an ordered collection of the specified classes such that
	 hierarchical dependencies come first."
	"Not sure whether we ever get non-behaviors. 
	The previous, more complex, version of this method contained this filter."

	| order toBeOrdered processed aClass |
	toBeOrdered := (someClasses select: [:each | each isBehavior])
				asIdentitySet.
	order := OrderedCollection new.
	processed := IdentitySet new.
	[(aClass := self anyElementOf: toBeOrdered ifEmpty: [nil]) isNil]
		whileFalse: 
			[self
				orderBySuperclass: aClass
				from: toBeOrdered
				into: order
				ignoring: processed].
	^order