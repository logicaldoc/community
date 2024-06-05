package com.logicaldoc.util.http;

import java.io.IOException;

import org.apache.hc.core5.http.HttpEntity;
import org.apache.hc.core5.http.ParseException;
import org.apache.hc.core5.http.io.entity.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A HTTP response handler that treats the response as a text content
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.3
 *
 */
public class StringHttpClientResponseHandler extends LoggingHttpClientResponseHandler<String> {

	protected static Logger log = LoggerFactory.getLogger(StringHttpClientResponseHandler.class);

	private static final String UTF_8 = "UTF-8";

	public StringHttpClientResponseHandler() {
		super();
	}

	@Override
	public String handleEntity(HttpEntity entity) throws IOException {
		// Extract body from response
		String content;
		try {
			content = EntityUtils.toString(entity, UTF_8);
		} catch (ParseException e) {
			throw new IOException(e);
		}
		return content;
	}
}