loading
updatePackage: aPackage
	"Answer the loader used to apply the update."

	^CypressLoader updatePackage: aPackage withSnapshot: self
