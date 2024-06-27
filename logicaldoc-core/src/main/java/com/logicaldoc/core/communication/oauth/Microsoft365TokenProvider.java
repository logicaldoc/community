package com.logicaldoc.core.communication.oauth;

import java.io.IOException;
import java.util.List;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang.StringUtils;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.entity.UrlEncodedFormEntity;
import org.apache.hc.client5.http.impl.classic.BasicHttpClientResponseHandler;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.core5.http.ClassicHttpResponse;
import org.apache.hc.core5.http.ParseException;
import org.apache.hc.core5.http.io.entity.EntityUtils;
import org.apache.hc.core5.http.message.BasicNameValuePair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.logicaldoc.util.http.HttpUtil;

/**
 * A TokenProvider used to access the Exchange Online (Microsoft 365)
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.3
 */
public class Microsoft365TokenProvider implements TokenProvider {

	protected static Logger log = LoggerFactory.getLogger(Microsoft365TokenProvider.class);

	private String clientSecret;

	private String clientId;

	private String clientTenant;

	/**
	 * Constructor
	 * 
	 * @param clientId the client ID
	 * @param clientTenmant the client Tenant
	 * @param clienSecret the client Secret
	 */
	public Microsoft365TokenProvider(String clientSecret, String clientId, String clientTenant) {
		this.clientSecret = clientSecret;
		this.clientId = clientId;
		this.clientTenant = clientTenant;
	}

	@Override
	public String getAccessToken() throws IOException {
		try (CloseableHttpClient httpClient = HttpUtil.getNotValidatingClient(60);) {

			// Prepare the post parameters
			HttpPost post = new HttpPost(
					String.format("https://login.microsoftonline.com/%s/oauth2/v2.0/token", clientTenant));
			UrlEncodedFormEntity entity = (new UrlEncodedFormEntity(
					List.of(new BasicNameValuePair("client_id", clientId),
							new BasicNameValuePair("client_secret", clientSecret),
							new BasicNameValuePair("grant_type", "client_credentials"),
							new BasicNameValuePair("scope", "https://outlook.office365.com/.default"))));
			post.setEntity(entity);

			return httpClient.execute(post, new BasicHttpClientResponseHandler() {

				@Override
				public String handleResponse(ClassicHttpResponse response) throws IOException {
					String content;
					try {
						content = EntityUtils.toString(response.getEntity(), "UTF-8");
					} catch (ParseException e) {
						throw new IOException(e);
					}

					log.debug("got 365 response: {}", StringUtils.substring(content, 150));

					ObjectMapper mapper = new ObjectMapper();
					JsonNode responseObj = mapper.readTree(content);
					if (content.contains("error"))
						throw new IOException(responseObj.get("error").asText() + " - "
								+ responseObj.get("error_description").asText());

					String token = new String(new Base64().decode(responseObj.get("access_token").asText().getBytes()));
					log.debug("got access_token: {}", token);
					return responseObj.get("access_token").asText();
				}
			});
		}
	}
}