"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: sorting
"
addClasses: subs to: order fromRelevantClasses: classSet organizedBy: org

	1 to: subs size
		do: 
			[:i |
			| assoc class |
			class := subs at: i.
			(classSet includesIdentical: class) ifTrue: [order add: class].
			assoc := org associationAt: class otherwise: nil.
			assoc ~~ nil
				ifTrue: 
					[self
						addClasses: assoc value
						to: order
						fromRelevantClasses: classSet
						organizedBy: org]]