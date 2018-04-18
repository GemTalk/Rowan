Class {
	#name : 'RwProjectBootstrapToolTest',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwProjectBootstrapToolTest >> testRowanBootstrapInRowan [
	"Bootstrapping Rowan is a special case, since the classes exist in the image, but Rowan thinks that the classes need to be added ... leading to intersting
		complications"

  | specification gitRepoPath |
  specification := RwSpecification fromUrl: 'file:$ROWAN_HOME/specs/Rowan_SystemUser_3215.ston'.
  gitRepoPath := '$ROWAN_HOME'.
  specification
    repositoryRootPath: gitRepoPath;
    repositoryUrl: 'cypress:' , gitRepoPath , '/' , specification repoPath , '/';
    register.
     Rowan projectTools load
      _bootstrapLoadProjectNamed: 'Rowan'
      withConfiguration: 'Default'.

self halt

]
