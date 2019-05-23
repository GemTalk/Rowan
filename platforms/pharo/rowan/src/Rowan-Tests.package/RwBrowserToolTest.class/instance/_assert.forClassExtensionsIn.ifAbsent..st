private
_assert: foundBlock forClassExtensionsIn: className ifAbsent: absentBlock

	"validate loaded class extension registry structure"

	| projectTools |
	projectTools := Rowan projectTools.
	projectTools edit
		definitionsForClassExtensionNamed: className
		ifFound: foundBlock
		ifAbsent: absentBlock