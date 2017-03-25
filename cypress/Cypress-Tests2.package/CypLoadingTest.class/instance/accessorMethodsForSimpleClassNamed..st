"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: support-definitionBuilding
"
accessorMethodsForSimpleClassNamed: name

	| result |
	result := Dictionary new.
	
	{'iv1In' , name.
	'iv2In' , name} do: 
				[:ivName |
				result
					at: ivName
						put: (CypMethodDefinition
								withProperties: (Dictionary with: 'selector' -> ivName
										with: 'protocol' -> 'accessing')
								source: ivName , ' ^' , ivName);
					at: ivName , ':'
						put: (CypMethodDefinition
								withProperties: (Dictionary with: 'selector' -> (ivName , ':')
										with: 'protocol' -> 'accessing')
								source: ivName , ': anObject ' , ivName , ' := anObject')].
	^result