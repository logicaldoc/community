package com.logicaldoc.core.communication.oauth;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang.StringUtils;
import org.apache.http.Consts;
import org.apache.http.HttpEntity;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
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
			List<NameValuePair> postParams = new ArrayList<NameValuePair>();

			// Application LogicalDOC
			postParams.add(new BasicNameValuePair("client_id", clientId));
			postParams.add(new BasicNameValuePair("client_secret", clientSecret));
			postParams.add(new BasicNameValuePair("grant_type", "client_credentials"));
			postParams.add(new BasicNameValuePair("scope", "https://outlook.office365.com/.default"));

			HttpPost request = new HttpPost(
					String.format("https://login.microsoftonline.com/%s/oauth2/v2.0/token", clientTenant));
			UrlEncodedFormEntity entity = new UrlEncodedFormEntity(postParams, Consts.UTF_8);
			request.setEntity(entity);

			// Make request
			try (CloseableHttpResponse response = httpClient.execute(request);) {
				// Extract body from response
				HttpEntity responseContent = response.getEntity();
				String result = EntityUtils.toString(responseContent, "UTF-8");

				log.debug("got 365 response: {}", StringUtils.substring(result, 150));

				ObjectMapper mapper = new ObjectMapper();
				JsonNode responseObj = mapper.readTree(result);
				if (result.contains("error"))
					throw new IOException(
							responseObj.get("error").asText() + " - " + responseObj.get("error_description").asText());

				String token = new String(new Base64().decode(responseObj.get("access_token").asText().getBytes()));
				log.debug("got access_token: {}", token);

				return responseObj.get("access_token").asText();
			}
		}
	}
}