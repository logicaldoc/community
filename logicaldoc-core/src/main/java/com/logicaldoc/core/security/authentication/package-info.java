/**
 * The pluggable authentication mechanism. Key class here is the
 * {@link com.logicaldoc.core.security.authentication.AuthenticationChain} that
 * handle a sequence of configured
 * {@link com.logicaldoc.core.security.authentication.Authenticator} <br>
 * <br>
 * Implementations of the
 * {@link com.logicaldoc.core.security.authentication.Authenticator} takes care
 * of authenticating the user using a specific technology <br>
 * <br>
 * A set of exceptions is defined in order to manifest the reason of a failed
 * authentication
 *
 * @since 1.0
 */
package com.logicaldoc.core.security.authentication;