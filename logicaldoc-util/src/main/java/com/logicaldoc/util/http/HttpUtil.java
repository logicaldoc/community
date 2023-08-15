package com.logicaldoc.util.http;

import java.io.IOException;
import java.io.InputStream;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

import org.apache.commons.lang.StringUtils;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHost;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.TrustStrategy;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.conn.DefaultProxyRoutePlanner;
import org.apache.http.ssl.SSLContextBuilder;
import org.apache.http.util.EntityUtils;

import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

public class HttpUtil {

	private HttpUtil() {
	}

	/**
	 * Gets the proxy configuration from the context (proxy. properties)
	 */
	private static String[] getProxy() {
		String[] proxy = new String[] { null, "0", null, null };
		try {
			// Try to get the config of the application context
			ContextProperties config = Context.get().getProperties();

			// fallback to the classpath resource
			if (config == null)
				config = new ContextProperties();

			proxy[0] = config.getProperty("proxy.host");
			proxy[1] = config.getProperty("proxy.port");
			proxy[2] = config.getProperty("proxy.username");
			proxy[3] = config.getProperty("proxy.password");
		} catch (Exception t) {
			// Nothing to do
		}

		// The port must be an integer
		try {
			proxy[1] = "" + Integer.parseInt(proxy[1]);
		} catch (Exception t) {
			proxy[1] = "0";
		}

		return proxy;
	}

	public static CloseableHttpClient getNotValidatingClient(int timeout, String proxyServer, Integer proxyPort,
			String proxyUser, String proxyPassword) {
		try {
			HttpClientBuilder clientBuilder = HttpClients.custom();

			RequestConfig.Builder requestBuilder = RequestConfig.custom().setConnectTimeout(timeout * 1000)
					.setSocketTimeout(timeout * 1000).setConnectionRequestTimeout(timeout * 1000)
					.setRedirectsEnabled(true);

			if (StringUtils.isNotEmpty(proxyServer)) {
				HttpHost proxyHost = new HttpHost(proxyServer, proxyPort);
				requestBuilder.setProxy(proxyHost);

				DefaultProxyRoutePlanner routePlanner = new DefaultProxyRoutePlanner(proxyHost);
				clientBuilder.setRoutePlanner(routePlanner);

				if (StringUtils.isNotEmpty(proxyUser)) {
					CredentialsProvider credentialProvider = new BasicCredentialsProvider();
					credentialProvider.setCredentials(AuthScope.ANY,
							new UsernamePasswordCredentials(proxyUser, proxyPassword));
					clientBuilder.setRoutePlanner(routePlanner);
				}
			}

			RequestConfig requestConfig = requestBuilder.build();

			return clientBuilder.setSSLHostnameVerifier(new NoopHostnameVerifier())
					.setSSLContext(new SSLContextBuilder().loadTrustMaterial(null, new TrustStrategy() {
						public boolean isTrusted(X509Certificate[] arg0, String arg1) throws CertificateException {
							return true;
						}
					}).build()).setDefaultRequestConfig(requestConfig).build();
		} catch (Exception t) {
			return null;
		}
	}

	public static CloseableHttpClient getNotValidatingClient(int timeout) {
		String[] proxy = getProxy();
		return getNotValidatingClient(timeout, proxy[0], Integer.parseInt(proxy[1]), proxy[2], proxy[3]);
	}

	public static void close(CloseableHttpResponse response) {
		if (response != null)
			try {
				response.close();
			} catch (IOException e) {
				// Nothing to do
			}
	}

	public static String getBodyString(CloseableHttpResponse response) {
		HttpEntity rent = response.getEntity();
		if (rent != null) {
			String respBody = "";
			try {
				respBody = EntityUtils.toString(rent, "UTF-8");
			} catch (Exception e) {
				// Nothing to do
			}
			return respBody;
		}
		return "";
	}

	public static InputStream getBodyStream(CloseableHttpResponse response) throws IllegalStateException, IOException {
		HttpEntity rent = response.getEntity();
		if (rent != null)
			return rent.getContent();
		else
			return null;
	}
}