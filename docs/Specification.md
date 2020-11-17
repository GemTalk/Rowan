## Components

### Components
A **component** is the loadable unit in Rowan. A **component** has a name and is located on disk in the *components directory*, specified by the *components path* of the *project specification*.

A **component** is composed of:
- A list of *dependent project names*. When a **component** is loaded, the *dependent projects* are loaded as well.
- A *preload doit name*. 
The *preload doit* is performed before the **component** itself is loaded.
- A *postload doit name*. 
The *postload doit* is performed after the **component** itself is loaded.
- A list of *package names*. 
When a **component** is loaded, the listed *packages* are loaded.
- A list of *component names*. 
When a **component** is loaded, the *packages* listed by the referenced **components** are loaded.

A **component** may be visible in a code browser and as such can be used to provide organization for classes within the browser.

A **component** should have an associated set of tests that validates the public API for the component.
As a loadable unit there is an implied contract between the developers and users of the component that the public API implemented by the classes and methods loaded by the component and the tests are expected to validate that contract.
### Subcomponent
A **subcomponent**
