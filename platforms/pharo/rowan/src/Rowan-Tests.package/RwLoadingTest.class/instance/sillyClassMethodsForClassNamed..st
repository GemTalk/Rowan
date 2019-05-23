support - simple definitions
sillyClassMethodsForClassNamed: name

	^Dictionary
		with: #'sillyMessage' -> (RwMethodDefinition
						withProperties: (Dictionary with: 'selector' -> #'sillyMessage'
								with: 'protocol' -> 'silliness')
						source: 'sillyMessage
	^ ''You have sent a silly message to the class '' , name')