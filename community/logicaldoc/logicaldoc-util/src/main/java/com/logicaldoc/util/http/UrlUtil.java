package com.logicaldoc.util.http;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Utility methods for handling URLs
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.2
 */
public class UrlUtil {
	public static Map<String, String> getParams(String urlString) throws UnsupportedEncodingException, MalformedURLException {
		URL url = new URL(urlString);
		Map<String, String> query_pairs = new LinkedHashMap<String, String>();
		String query = url.getQuery();
		String[] pairs = query.split("&");
		for (String pair : pairs) {
			int idx = pair.indexOf("=");
			query_pairs.put(URLDecoder.decode(pair.substring(0, idx), "UTF-8"),
					URLDecoder.decode(pair.substring(idx + 1), "UTF-8"));
		}
		return query_pairs;
	}
}
