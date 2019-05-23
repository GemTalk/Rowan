actions
clone
	"clone remote git project to disk"

	self projectRef clone.
	^ self read						"refresh receiver from the cloned repository and answer project definition set that contains reciever along with any dependent projects"