class browsing
classCreationTemplateForSubclassOf: superclassName className: className category: category packageName: packageName

	^ self
		classCreationTemplateForSubclassOf: superclassName
		className: className
		type: 'normal'
		category: category
		packageName: packageName