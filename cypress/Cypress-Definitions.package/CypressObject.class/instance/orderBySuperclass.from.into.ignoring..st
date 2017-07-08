"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
orderBySuperclass: aClass from: toBeOrdered into: order ignoring: processed
	"Private. Add to 'order', superclasses first, aClass and any of its superclasses 
	that appear in 'toBeOrdered' but do not appear in 'processed'.
	Remove from 'toBeOrdered' any class added to 'ordered'.
	Any class seen, add to 'processed' whether or not added to 'order'."

	| superclass |
	superclass := aClass superclass.
	superclass isNil | (processed includes: superclass)
		ifFalse: 
			[self
				orderBySuperclass: superclass
				from: toBeOrdered
				into: order
				ignoring: processed].
	processed add: aClass.
	(toBeOrdered includes: aClass)
		ifTrue: 
			[toBeOrdered remove: aClass.
			order add: aClass]
