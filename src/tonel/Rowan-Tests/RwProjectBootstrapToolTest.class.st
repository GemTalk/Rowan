Class {
	#name : 'RwProjectBootstrapToolTest',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwProjectBootstrapToolTest >> testRowanBootstrapInRowan [
	"Bootstrapping Rowan is a special case, since the classes exist in the image, but Rowan thinks that the classes need to be added ... leading to intersting
		complications"

    Rowan projectTools load
      _bootstrapLoadProjectNamed: 'Rowan'
      withConfiguration: 'Default'

]
