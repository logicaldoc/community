package com.logicaldoc.util.security;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;

import javax.net.SocketFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * An SSL socket factory that will let any certifacte pass, even if it's expired
 * or not singed by a root CA, unless the environment variable
 * <code>ldoc.ssl.validate = true</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class EasySSLSocketFactory extends SSLSocketFactory {
	private static final Logger log = LoggerFactory.getLogger(EasySSLSocketFactory.class);

	private SSLSocketFactory factory;

	public EasySSLSocketFactory() {
		try {
			SSLContext sslcontent = SSLContext.getInstance("TLS");
			sslcontent.init(null, // KeyManager not required
					new TrustManager[] { new EasyX509TrustManager() }, new java.security.SecureRandom());
			factory = sslcontent.getSocketFactory();
		} catch (NoSuchAlgorithmException | KeyManagementException e) {
			log.debug(e.getMessage());
		}
	}

	public static SocketFactory getDefault() {
		return new EasySSLSocketFactory();
	}

	public Socket createSocket(Socket socket, String s, int i, boolean flag) throws IOException {
		return factory.createSocket(socket, s, i, flag);
	}

	public Socket createSocket(InetAddress inaddr, int i, InetAddress inaddr2, int j) throws IOException {
		return factory.createSocket(inaddr, i, inaddr2, j);
	}

	public Socket createSocket(InetAddress inaddr, int i) throws IOException {
		return factory.createSocket(inaddr, i);
	}

	public Socket createSocket(String s, int i, InetAddress inaddr, int j) throws IOException {
		return factory.createSocket(s, i, inaddr, j);
	}

	public Socket createSocket(String s, int i) throws IOException {
		return factory.createSocket(s, i);
	}

	public String[] getDefaultCipherSuites() {
		return factory.getSupportedCipherSuites();
	}

	public String[] getSupportedCipherSuites() {
		return factory.getSupportedCipherSuites();
	}
}