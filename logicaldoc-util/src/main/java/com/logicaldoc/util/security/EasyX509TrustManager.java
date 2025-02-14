package com.logicaldoc.util.security;

import java.security.KeyStore;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Trust manager that trusts all the certificates unless the environment
 * variable <code>ldoc.ssl.validate = true</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class EasyX509TrustManager implements X509TrustManager {

	private static final String LDOC_SSL_VALIDATE = "ldoc.ssl.validate";

	private static final Logger log = LoggerFactory.getLogger(EasyX509TrustManager.class);

	private static X509TrustManager dafaultTrustManager = null;

	static {
		try {
			TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
			tmf.init((KeyStore) null);

			for (TrustManager tm : tmf.getTrustManagers()) {
				if (tm instanceof X509TrustManager x509) {
					dafaultTrustManager = x509;
					break;
				}
			}

			log.info("Using default trust manager {}", dafaultTrustManager);
		} catch (Exception t) {
			log.warn(t.getMessage(), t);
		}
	}

	@Override
	public void checkClientTrusted(X509Certificate[] arg0, String arg1) throws CertificateException {
		if ("true".equals(System.getProperty(LDOC_SSL_VALIDATE)) && dafaultTrustManager != null)
			dafaultTrustManager.checkClientTrusted(arg0, arg1);
	}

	@Override
	public void checkServerTrusted(X509Certificate[] arg0, String arg1) throws CertificateException {
		if ("true".equals(System.getProperty(LDOC_SSL_VALIDATE)) && dafaultTrustManager != null)
			dafaultTrustManager.checkServerTrusted(arg0, arg1);
	}

	@Override
	public X509Certificate[] getAcceptedIssuers() {
		if ("true".equals(System.getProperty(LDOC_SSL_VALIDATE)) && dafaultTrustManager != null)
			return dafaultTrustManager.getAcceptedIssuers();
		else
			return new X509Certificate[0];
	}
}