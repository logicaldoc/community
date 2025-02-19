package com.logicaldoc.util.http;

import java.io.IOException;

import org.apache.commons.lang.StringUtils;
import org.apache.hc.client5.http.impl.classic.AbstractHttpClientResponseHandler;
import org.apache.hc.core5.http.ClassicHttpResponse;
import org.apache.hc.core5.http.HttpEntity;
import org.apache.hc.core5.http.ParseException;
import org.apache.hc.core5.http.io.entity.EntityUtils;

/**
 * An extension of the standard HttpClientReponseHandler with utility methods
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.3
 * 
 * @param <T> The type of attended response content
 */
public abstract class BaseHttpClientResponseHandler<T> extends AbstractHttpClientResponseHandler<T> {

	private static final String UTF_8 = "UTF-8";

	public static String getResponseBody(ClassicHttpResponse response) throws IOException {
		return getResponseBody(response, null);
	}

	public static String getResponseBody(ClassicHttpResponse response, Integer maxLength) throws IOException {
		HttpEntity responseContent = response.getEntity();

		String content;
		try {
			content = EntityUtils.toString(responseContent, UTF_8);
		} catch (ParseException e) {
			throw new IOException(e);
		}

		return maxLength != null ? StringUtils.abbreviate(content, 200) : content;
	}
}