Extension { #name : 'Behavior' }

{ #category : '*rowan-gemstone-kernel-32' }
Behavior >> compileMethod: sourceString
dictionaries: aSymbolList
category: categorySymbol
intoMethodDict: aMethodDict
intoCategories: aCategDict
environmentId: env [

^ self compileMethod: sourceString 
       dictionaries: aSymbolList
       category: categorySymbol 
       intoMethodDict: aMethodDict
       intoCategories: aCategDict  
       intoPragmas: nil environmentId: env
]
