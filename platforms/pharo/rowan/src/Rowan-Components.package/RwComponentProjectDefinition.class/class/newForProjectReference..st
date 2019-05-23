instance creation
newForProjectReference: aRwProjectReferenceDefintion

	^ self
		withProperties:
			(Dictionary new
				add: 'name' -> aRwProjectReferenceDefintion projectAlias;
				add: 'projectRef' -> aRwProjectReferenceDefintion;
				yourself)
		packageDefinitions: Dictionary new
		componentDefinitions: Dictionary new