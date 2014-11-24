<h1>Authentication</h1>

### Security model

To establish the authenticity of a request, the server performs a message integrity check using a cryptographic primitive known as a HMAC (hash-based message authentication code). A MAC code is attached to the request in the form of an API-Access header. A second code is computed from the request object using a token associated with the client application. The result of this operation is subsequently compared to the MAC attached to the request in order to verify or refute its authenticity.

