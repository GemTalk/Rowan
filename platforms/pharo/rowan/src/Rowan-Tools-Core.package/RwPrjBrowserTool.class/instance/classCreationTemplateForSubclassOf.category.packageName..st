class browsing
classCreationTemplateForSubclassOf: superclassName category: category packageName: packageName

	^ self
		classCreationTemplateForSubclassOf: superclassName
		className: 'NameOfSubclass'
		category: category
		packageName: packageName