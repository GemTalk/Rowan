Class {
	#name : 'RwPkgCreateTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPkgCreateTool >> createLoadedPackageNamed: packageName inProjectNamed: projectName [

	"create a loaded package named <packageName> in the loaded project named <projectName>"

	| spec registry theSymbolDictionary packageSymDictName gemstoneSpec loadedPackage loadedProject |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	spec := loadedProject specification.
	gemstoneSpec := spec platformSpec at: 'gemstone'.
	packageSymDictName := gemstoneSpec symbolDictNameForPackageNamed: packageName.
	theSymbolDictionary := Rowan image newOrExistingSymbolDictionaryNamed: packageSymDictName.
	registry := theSymbolDictionary rowanSymbolDictionaryRegistry.
	loadedPackage := registry newLoadedPackageNamed: packageName.
	loadedProject addLoadedPackage: loadedPackage.

]
