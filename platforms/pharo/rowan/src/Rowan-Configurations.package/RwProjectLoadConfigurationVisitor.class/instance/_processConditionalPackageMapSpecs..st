private
_processConditionalPackageMapSpecs: aProjectLoadConfiguration

	| currentUserId |
	currentUserId := Rowan image currentUserId.
	aProjectLoadConfiguration conditionalPackageMapSpecMatchers keysAndValuesDo: [:platformMatcher :thePackageMapSpecsMap | 
		(self _platformAttributeMatchIn: {platformMatcher})
			ifTrue: [
				(thePackageMapSpecsMap 
					at: currentUserId 
					ifAbsent: [ 
						"<allusers> is preferred, but for the alpha, there are outstanding configs that use <default>"
						thePackageMapSpecsMap at: 'default'  
							ifAbsent: [ thePackageMapSpecsMap at: 'allusers'  ifAbsent: [] ] ])
								ifNotNil: [:thePackageMapSpecs |
									(thePackageMapSpecs at: #defaultSymbolDictName otherwise: nil) 
										ifNotNil: [:name | packageMapSpecs at: #defaultSymbolDictName put: name ].
									(thePackageMapSpecs at: #defaultUseSessionMethodsForExtensions otherwise: nil) 
										ifNotNil: [:boolean | packageMapSpecs at: #defaultUseSessionMethodsForExtensions put: boolean  ].
									(thePackageMapSpecs at: #packageNameToPlatformPropertiesMap otherwise: nil) 
										ifNotNil: [:theMap | 
											| map |
											map := packageMapSpecs at: #packageNameToPlatformPropertiesMap ifAbsentPut: [ Dictionary new ].
											theMap keysAndValuesDo: [:thePackageName :thePropMap | 
												thePropMap keysAndValuesDo: [:thePropertyName :thePropertyValue |
													| packageMap |
													packageMap  := map at: thePackageName ifAbsentPut: [ Dictionary new ].
													(#('symbolDictName' 'userId' 'useSessionMethodsForExtensions' ) includes: thePropertyName)
														ifTrue: [ packageMap at: thePropertyName put: thePropertyValue  ] 
														ifFalse: [ self error: 'Unrecognized property name ', thePropertyName printString, ' in package name to properties map' ] ] ] ] ] ] ]
