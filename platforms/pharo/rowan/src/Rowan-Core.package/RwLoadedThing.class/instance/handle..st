accessing
handle: anObject
	"Set my reference to the platform-specific image object for which I hold information."

	self markPackageDirty.
	handle := anObject
