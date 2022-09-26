package com.logicaldoc.util.security;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLSession;

/**
 * A dummy verifier that accepts all the certificates, unless the environment variable <code>ldoc.ssl.validate = true</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class EasyHostnameVerifier implements HostnameVerifier {
	
	@Override
	public boolean verify(String hostname, SSLSession sslSession) {
		if ("true".equals(System.getProperty("ldoc.ssl.validate")))
			return javax.net.ssl.HttpsURLConnection.getDefaultHostnameVerifier().verify(hostname, sslSession);
		else
			return true;
	}
}