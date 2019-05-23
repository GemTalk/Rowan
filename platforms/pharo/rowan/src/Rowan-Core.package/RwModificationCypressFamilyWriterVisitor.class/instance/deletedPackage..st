actions
deletedPackage: aPackageModification

	currentPackageDefinition := aPackageModification before.

	self _packageSourceDir ensureDeleteAll.	"delete the whole package directory"