package com.logicaldoc.util.http;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Utility methods for handling URLs
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.2
 */
public class UrlUtil {

	private static final String UTF_8 = "UTF-8";

	private UrlUtil() {
	}

	public static Map<String, List<String>> getQueryParams(String url) {
		try {
			Map<String, List<String>> params = new HashMap<>();
			String[] urlParts = url.split("\\?");
			if (urlParts.length > 1) {
				String query = urlParts[1];
				for (String param : query.split("&")) {
					String[] pair = param.split("=");
					String key = URLDecoder.decode(pair[0], UTF_8);
					String value = "";
					if (pair.length > 1) {
						value = URLDecoder.decode(pair[1], UTF_8);
					}

					List<String> values = params.get(key);
					if (values == null) {
						values = new ArrayList<>();
						params.put(key, values);
					}
					values.add(value);
				}
			}

			return params;
		} catch (UnsupportedEncodingException ex) {
			throw new AssertionError(ex);
		}
	}

	public static Map<String, String> getParams(String urlString)
			throws UnsupportedEncodingException, MalformedURLException {
		URL url = new URL(urlString);
		Map<String, String> query_pairs = new LinkedHashMap<>();
		String query = url.getQuery();
		String[] pairs = query.split("&");
		for (String pair : pairs) {
			int idx = pair.indexOf("=");
			query_pairs.put(URLDecoder.decode(pair.substring(0, idx), UTF_8),
					URLDecoder.decode(pair.substring(idx + 1), UTF_8));
		}
		return query_pairs;
	}

	public static String getQueryParam(String url, String parameter) throws MalformedURLException {
		try {
			return getParams(url).get(parameter);
		} catch (UnsupportedEncodingException ex) {
			throw new AssertionError(ex);
		}
	}

	/**
	 * Translates a string into application/x-www-form-urlencoded format using a
	 * specific encoding scheme. This method uses the supplied encoding scheme
	 * to obtain the bytes for unsafe characters.
	 *
	 * @param s - String to be translated.
	 * @param enc - The name of a supported character encoding.
	 * @return the translated String.
	 * @throws UnsupportedEncodingException - If the named encoding is not
	 *         supported
	 */
	public static String encode(String s, String enc) throws UnsupportedEncodingException {
		return URLEncoder.encode(s, enc);
	}

	public static String normalize(String inputUrl) throws URISyntaxException {
		URI inputUri = new URI(inputUrl);
		URI normalizedUri = inputUri.normalize();
		return normalizedUri.toString();
	}
}