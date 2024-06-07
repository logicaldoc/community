package com.logicaldoc.util.http;

import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang.StringUtils;
import org.apache.hc.client5.http.auth.AuthCache;
import org.apache.hc.client5.http.auth.AuthScope;
import org.apache.hc.client5.http.auth.CredentialsProvider;
import org.apache.hc.client5.http.config.ConnectionConfig;
import org.apache.hc.client5.http.config.RequestConfig;
import org.apache.hc.client5.http.impl.DefaultRedirectStrategy;
import org.apache.hc.client5.http.impl.auth.BasicAuthCache;
import org.apache.hc.client5.http.impl.auth.BasicScheme;
import org.apache.hc.client5.http.impl.auth.CredentialsProviderBuilder;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.HttpClientBuilder;
import org.apache.hc.client5.http.impl.io.PoolingHttpClientConnectionManager;
import org.apache.hc.client5.http.impl.io.PoolingHttpClientConnectionManagerBuilder;
import org.apache.hc.client5.http.impl.routing.DefaultProxyRoutePlanner;
import org.apache.hc.client5.http.protocol.HttpClientContext;
import org.apache.hc.client5.http.ssl.NoopHostnameVerifier;
import org.apache.hc.client5.http.ssl.SSLConnectionSocketFactoryBuilder;
import org.apache.hc.client5.http.ssl.TrustAllStrategy;
import org.apache.hc.core5.http.ClassicHttpResponse;
import org.apache.hc.core5.http.HttpEntity;
import org.apache.hc.core5.http.HttpHost;
import org.apache.hc.core5.http.io.SocketConfig;
import org.apache.hc.core5.http.io.entity.EntityUtils;
import org.apache.hc.core5.ssl.SSLContextBuilder;

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
		} catch (NoClassDefFoundError | Exception t) {
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
			ConnectionConfig connectionConfig = ConnectionConfig.custom().setConnectTimeout(timeout, TimeUnit.SECONDS)
					.setSocketTimeout(timeout, TimeUnit.SECONDS).build();

			RequestConfig requestConfig = RequestConfig.custom().setConnectionRequestTimeout(timeout, TimeUnit.SECONDS)
					.setResponseTimeout(timeout, TimeUnit.SECONDS).build();

			SocketConfig socketConfig = SocketConfig.custom().setSoTimeout(timeout, TimeUnit.SECONDS).build();

			PoolingHttpClientConnectionManager poolingHttpClientConnectionManager = PoolingHttpClientConnectionManagerBuilder
					.create().setDefaultConnectionConfig(connectionConfig).setDefaultSocketConfig(socketConfig).build();

			HttpClientBuilder clientBuilder = HttpClientBuilder.create()
					.setRedirectStrategy(new DefaultRedirectStrategy())
					.setConnectionManager(poolingHttpClientConnectionManager).setDefaultRequestConfig(requestConfig);

			if (StringUtils.isNotEmpty(proxyServer)) {
				HttpHost proxyHost = new HttpHost(proxyServer, proxyPort);
				DefaultProxyRoutePlanner routePlanner = new DefaultProxyRoutePlanner(proxyHost);

				if (StringUtils.isNotEmpty(proxyUser)) {
					// Client credentials
					CredentialsProvider credentialsProvider = CredentialsProviderBuilder.create()
							.add(new AuthScope(proxyHost), proxyUser, proxyPassword.toCharArray()).build();

					// Create AuthCache instance
					AuthCache authCache = new BasicAuthCache();

					// Generate BASIC scheme object and add it to the local auth
					// cache
					BasicScheme basicAuth = new BasicScheme();
					authCache.put(proxyHost, basicAuth);
					HttpClientContext context = HttpClientContext.create();
					context.setCredentialsProvider(credentialsProvider);
					context.setAuthCache(authCache);

					clientBuilder = clientBuilder.setRoutePlanner(routePlanner)
							.setDefaultCredentialsProvider(credentialsProvider);
				} else {
					clientBuilder = clientBuilder.setRoutePlanner(routePlanner);
				}
			}

			// Non validating SSL policies
			clientBuilder = clientBuilder.setConnectionManager(PoolingHttpClientConnectionManagerBuilder.create()
					.setSSLSocketFactory(SSLConnectionSocketFactoryBuilder.create()
							.setSslContext(
									SSLContextBuilder.create().loadTrustMaterial(TrustAllStrategy.INSTANCE).build())
							.setHostnameVerifier(NoopHostnameVerifier.INSTANCE).build())
					.build());

			return clientBuilder.build();
		} catch (Exception t) {
			return null;
		}
	}

	public static CloseableHttpClient getNotValidatingClient(int timeout) {
		String[] proxy = getProxy();
		return getNotValidatingClient(timeout, proxy[0], Integer.parseInt(proxy[1]), proxy[2], proxy[3]);
	}

	public static void close(ClassicHttpResponse response) {
		if (response != null)
			try {
				response.close();
			} catch (IOException e) {
				// Nothing to do
			}
	}

	public static String getBodyString(ClassicHttpResponse response) {
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

	public static InputStream getBodyStream(ClassicHttpResponse response) throws IllegalStateException, IOException {
		HttpEntity rent = response.getEntity();
		if (rent != null)
			return rent.getContent();
		else
			return null;
	}
}