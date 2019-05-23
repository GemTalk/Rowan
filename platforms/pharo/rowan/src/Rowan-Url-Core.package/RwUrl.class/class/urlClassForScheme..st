parsing
urlClassForScheme: scheme

	"explicitly list the classes to be matched: https://github.com/dalehenrich/Rowan/issues/211"

	^ {RwFileUrl. RwCypressUrl. RwTonelUrl. RwFiletreeUrl. RwGithubUrl. RwGenericUrl. RwHierarchicalUrl. RwSmalltalkRepositoryUrl. RwGitFileTreeUrl}
				detect: [ :urlClass | urlClass schemeName = scheme ]
				ifNone: [ RwGenericUrl ].
