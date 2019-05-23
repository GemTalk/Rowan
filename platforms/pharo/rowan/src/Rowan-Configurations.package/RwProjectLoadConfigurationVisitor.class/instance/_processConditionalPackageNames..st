private
_processConditionalPackageNames: aProjectLoadConfiguration

	aProjectLoadConfiguration conditionalPackageMatchers keysAndValuesDo: [:platformMatchers :groupMap | 
		(self _platformAttributeMatchIn: platformMatchers)
			ifTrue: [
				groupMap keysAndValuesDo: [:group :map |
					(self groupNames includes: group)
						ifTrue: [ 
							self _addPackageNames: (map at: #packageNames ifAbsent: [ #() ]) for: aProjectLoadConfiguration.
							self configurationNames addAll: (map at: #configurationNames ifAbsent: [ #()] ) ] ] ] ].
