# agena.ai 1.2.0

* The local agena.ai API is now downloaded as a pre-built bundle by the new `local_api_install()`. git, Maven and a Java JDK are no longer needed: a Java runtime is downloaded too. Nothing is written to the working directory
* The Java runtime is resolved from the API jar's `Require-Java` manifest attribute rather than pinned by this package, so the engine runs on the runtime it was built against and a Java version change travels with the API. Jars declaring nothing get Java 21
* Where no runtime is published for a platform, a suitable Java already on the machine is used instead. Its version is checked against the jar's requirement rather than assumed, so an unusable Java is refused at setup instead of failing mid-calculation
* Components are installed under `~/.agena.ai/tools` and shared with agena.ai Explorer and other local agena.ai clients, so an install already on the machine is reused
* `local_api_install()` is safe to leave at the top of a script: it installs what is missing, makes no network request when nothing is, and never upgrades you unasked. `local_api_install(update = TRUE)` checks for a newer version
* `local_api_install(channel = "snapshot")` installs an unreleased build
* New `local_api_license_summary()` and `local_api_deactivate_license()`
* `local_api_clone()` and `local_api_compile()` are deprecated. They install the bundle and warn, and will be removed in a future release

# agena.ai 1.1.2

* Fixed a bug in function `model$get_results` that caused erroneous rows to appear in the output
* Vastly improved speed of `model$get_results`
* Fixed a bug in function `create_batch_cases` that limited the number of imported cases to a very small number

# agena.ai 1.1.1

* Documentation updates regarding new Node, Network, and Model creation
* A new vignette regarding Model creation is added

# agena.ai 1.1.0

* Cloud API functions support async requests (polling). If the computation request is too big to finish in the initial 10 second period, the job will be polled and checked again, and once finished the response will be obtained.
* Cloud API functions come with better error message displays.
* Cloud API functions come with the new parameter `debug` which enables further debugging messages to be displayed (false by default).

# agena.ai 1.0.0

* First release on CRAN

# agena.ai 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
