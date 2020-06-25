package com.logicaldoc.util.security;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLSession;

/**
 * A dummy verifier that accepts all the certificates
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class DummyHostnameVerifier implements HostnameVerifier {
	
	@Override
	public boolean verify(String hostname, SSLSession sslSession) {
		return true;
	}
}