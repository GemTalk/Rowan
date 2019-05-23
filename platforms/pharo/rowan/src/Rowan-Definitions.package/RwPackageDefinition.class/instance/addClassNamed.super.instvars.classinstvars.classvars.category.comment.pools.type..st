accessing
addClassNamed: className super: superclassName instvars: instvars classinstvars: classinstvars classvars: classvars category: category comment: comment pools: pools type: type

	^ self addClassDefinition: 
		(RwClassDefinition 
			newForClassNamed: className 
				super: superclassName 
				instvars: instvars 
				classinstvars: 
				classinstvars 
				classvars: classvars 
				category: category 
				comment: comment 
				pools: pools 
				type: type)