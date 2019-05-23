class browsing
classCreationTemplateForSubclassOf: superclassName className: className category: category

	^ self
		classCreationTemplateForSubclassOf: superclassName
		className: className
		category: category
		packageName: nil