class browsing
classCreationTemplateForSubclassOf: superclassName category: category

	^ self
		classCreationTemplateForSubclassOf: superclassName
		category: category
		packageName: nil