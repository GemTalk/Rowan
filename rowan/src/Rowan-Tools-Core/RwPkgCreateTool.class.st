Class {
	#name : 'RwPkgCreateTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPkgCreateTool >> createLoadedPackageNamed: packageName inProjectNamed: projectName [

	"create a loaded package named <packageName> in the loaded project named <projectName>"

	| spec registry theSymbolDictionary packageSymDictName gemstoneSpec |
	spec := (Rowan image loadedProjectNamed: projectName) specification.
	gemstoneSpec := spec platformSpec at: 'gemstone'.
	packageSymDictName := gemstoneSpec symbolDictNameForPackageNamed: packageName.
	theSymbolDictionary := Rowan image newOrExistingSymbolDictionaryNamed: packageSymDictName.
	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.
	registry newLoadedPackageNamed: packageName

]
