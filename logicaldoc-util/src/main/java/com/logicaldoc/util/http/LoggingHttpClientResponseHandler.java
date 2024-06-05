package com.logicaldoc.util.http;

import java.io.IOException;

import org.apache.commons.lang.StringUtils;
import org.apache.hc.client5.http.impl.classic.AbstractHttpClientResponseHandler;
import org.apache.hc.core5.http.ClassicHttpResponse;
import org.apache.hc.core5.http.HttpEntity;
import org.apache.hc.core5.http.ParseException;
import org.apache.hc.core5.http.io.entity.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * An extension of the starndard HttpClientReponseHandler that also logs the
 * content of the response in case of error
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.3
 */
public abstract class LoggingHttpClientResponseHandler<T> extends AbstractHttpClientResponseHandler<T> {

	protected static Logger log = LoggerFactory.getLogger(LoggingHttpClientResponseHandler.class);

	private static final String UTF_8 = "UTF-8";

	@Override
	public T handleResponse(final ClassicHttpResponse response) throws IOException {
		if (log.isDebugEnabled()) {
			// Log the response
			log.debug("Got response: {}", getResponseBody(response, 200));
		}

		try {
			return super.handleResponse(response);
		} catch (IOException e) {
			// Throw an exception that reports the returned content chaned with
			// the original exception from the parent
			throw new IOException(getResponseBody(response, 200), e);
		}
	}

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