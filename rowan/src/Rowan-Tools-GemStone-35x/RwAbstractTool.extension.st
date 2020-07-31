Extension { #name : 'RwAbstractTool' }

{ #category : '*rowan-tools-gemstone-35x' }
RwAbstractTool >> readlink: filepath [
	"resolve (possible) symbolic links in filepath and return an absolute path"

	^ GsFile _expandFilename: filepath isClient: false
]
