"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: support-definitionBuilding
"
sillyClassMethodsForClassNamed: name

	^Dictionary
		with: 'sillyMessage' -> (CypMethodDefinition
						withProperties: (Dictionary with: 'selector' -> 'sillyMessage'
								with: 'protocol' -> 'silliness')
						source: 'sillyMessage
	^ ''You have sent a silly message to the class '' , name')