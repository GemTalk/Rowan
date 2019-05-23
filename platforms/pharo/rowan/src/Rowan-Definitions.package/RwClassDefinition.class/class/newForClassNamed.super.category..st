instance creation
newForClassNamed: className super: superClassName category: category

	^ self 
		newForClassNamed: className 
		super: superClassName 
		instvars: #() 
		classinstvars: #() 
		classvars: #() 
		category: category 
		comment: nil 
		pools: #() 
		type: 'normal'