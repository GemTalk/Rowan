"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: sorting
"
determineClassHierarchicalOrder: someClasses
	"Returns an ordered collection of the specified classes such that
	 hierarchical dependencies come first."

	| org order classSet |
	org := Dictionary new.
	org at: #nil put: ClassSet new.
	classSet := ClassSet new.
	someClasses do: 
			[:each |
			| sub |
			sub := each.
			sub isBehavior
				ifTrue: 
					[| superCls |
					classSet add: sub.
					
					[superCls := sub superClass.
					superCls ~~ nil] whileTrue: 
								[| assoc |
								assoc := org associationAt: superCls otherwise: nil.
								assoc
									ifNil: 
										[assoc := Association newWithKey: superCls value: ClassSet new.
										org add: assoc].
								assoc value add: sub.
								sub := superCls].
					(org at: #nil) add: sub]].

	"Order the subclass sets and weed out unwanted classes."
	order := Array new.
	self
		addClasses: (org at: #nil)
		to: order
		fromRelevantClasses: classSet
		organizedBy: org.
	^order