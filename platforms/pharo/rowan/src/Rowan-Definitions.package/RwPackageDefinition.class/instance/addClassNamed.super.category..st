accessing
addClassNamed: className super: superclassName category: categryName

	^ self addClassDefinition: (RwClassDefinition newForClassNamed: className super: superclassName  category: categryName)