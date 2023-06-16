# agena.ai 1.1.0

* Cloud API functions come with better error message displays.
* Cloud API functions come with the new parameter `debug` which enables further debugging messages to be displayed (false by default).
* Cloud API functions support async requests (polling). If the computation request is too big to finish in the initial 10 second period, the job will be polled and checked again, and once finished the response will be obtained.

# agena.ai 1.0.0

* First release on CRAN

# agena.ai 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
