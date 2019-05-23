instance creation
newForSpecification: aRwSpecification

	^ self
		withProperties:
			(Dictionary new
				add: 'name' -> aRwSpecification specName;
				add: 'spec' -> aRwSpecification;
				yourself)
		packageDefinitions: Dictionary new