tonel parser
newClassDefinitionFrom: anArray
  | metadata |
  metadata := anArray sixth.
	currentClassExtension := nil.
  currentClassDefinition := RwClassDefinition
		newForClassNamed: (metadata at: #'name') 
			super: (metadata at: #'superclass' ifAbsent: [metadata at: #'super']) 
			instvars: (metadata at: #'instvars' ifAbsent: [ #() ]) 
			classinstvars: (metadata at: #'classinstvars' ifAbsent: [ #() ]) 
			classvars: (metadata at: #'classvars' ifAbsent: [ #() ]) 
			category: (metadata at: #'category' ifAbsent: [ ])
			comment: (anArray second ifNil: [ '' ]) 
			pools: (metadata at: #'pools' ifAbsent: [ #() ]) 
			type: (metadata at: #'type' ifAbsent: [ #'normal' ]) asSymbol.
	^ currentClassDefinition
		gs_options: (metadata at: #'gs_options' ifAbsent: [ #() ]);
		gs_constraints: (metadata at: #'gs_constraints' ifAbsent: [ #() ]);
		yourself