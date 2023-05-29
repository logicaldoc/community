package com.logicaldoc.util.http;

import java.nio.charset.Charset;
import java.util.Base64;

public class HttpBasicAuthenticationHeader {

	private HttpBasicAuthenticationHeader() {
	}

	public static String[] decodeFromAuthorization(String authorization) {
		if (authorization != null && authorization.startsWith("Basic")) {
			// Authorization: Basic base64credentials
			String base64Credentials = authorization.substring("Basic".length()).trim();
			String credentials = new String(Base64.getDecoder().decode(base64Credentials), Charset.forName("UTF-8"));
			// credentials = username:password
			return credentials.split(":", 2);
		} else
			return null;
	}

	public static String[] decode(final String encodedString) {
		final byte[] decodedBytes = org.apache.commons.codec.binary.Base64.decodeBase64(encodedString.getBytes());
		final String pair = new String(decodedBytes);
		final String[] userDetails = pair.split(":", 2);
		return userDetails;
	}

	public static String createEncodedText(final String username, final String password) {
		final String pair = username + ":" + password;
		final byte[] encodedBytes = org.apache.commons.codec.binary.Base64.encodeBase64(pair.getBytes());
		return new String(encodedBytes);
	}
}