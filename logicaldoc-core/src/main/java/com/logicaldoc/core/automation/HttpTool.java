package com.logicaldoc.core.automation;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.hc.client5.http.classic.methods.HttpGet;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.classic.methods.HttpUriRequestBase;
import org.apache.hc.client5.http.entity.UrlEncodedFormEntity;
import org.apache.hc.client5.http.entity.mime.AbstractContentBody;
import org.apache.hc.client5.http.entity.mime.HttpMultipartMode;
import org.apache.hc.client5.http.entity.mime.MultipartEntityBuilder;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.core5.http.NameValuePair;
import org.apache.hc.core5.http.io.entity.StringEntity;
import org.apache.hc.core5.http.message.BasicNameValuePair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.http.FileHttpClientResponseHandler;
import com.logicaldoc.util.http.HttpUtil;
import com.logicaldoc.util.http.StringHttpClientResponseHandler;
import com.logicaldoc.util.io.FileUtil;

/**
 * Utility methods to call URLs
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
@AutomationDictionary
public class HttpTool {

	private static final String ERROR_CALLING_URL = "Error calling URL ";

	private static final Logger log = LoggerFactory.getLogger(HttpTool.class);

	/**
	 * Calls the provided URL with an HTTP GET request
	 * 
	 * @param url The URL to call
	 * @param headers Optional map of headers
	 * @param timeout Timeout in seconds
	 * 
	 * @return The response body
	 */
	public String get(String url, Map<String, String> headers, int timeout) {
		try (CloseableHttpClient httpClient = HttpUtil.getNotValidatingClient(timeout);) {
			HttpGet get = new HttpGet(url);
			putHeaders(get, headers);

			return httpClient.execute(get, new StringHttpClientResponseHandler());
		} catch (Exception e) {
			log.error(ERROR_CALLING_URL + url, e);
			return null;
		}
	}

	/**
	 * Downloads a file from a URL
	 * 
	 * @param url The URL to call
	 * @param headers Optional map of headers
	 * @param timeout Timeout in seconds
	 * 
	 * @return The downloaded file, this is a temporary file, remember to delete it when done
	 */
	public File download(String url, Map<String, String> headers, int timeout) {
		File downloadedFile = null;
		try (CloseableHttpClient httpClient = HttpUtil.getNotValidatingClient(timeout);) {
			downloadedFile = FileUtil.createTempFile("download-", ".dld");
			HttpGet get = new HttpGet(url);
			putHeaders(get, headers);

			httpClient.execute(get, new FileHttpClientResponseHandler(downloadedFile));
			return downloadedFile;
		} catch (Exception e) {
			log.error(ERROR_CALLING_URL + url, e);
			FileUtil.delete(downloadedFile);
			return null;
		}
	}

	/**
	 * Calls the provided URL with an HTTP POST request
	 * 
	 * @param url The URL to call
	 * @param headers Optional map of headers
	 * @param entity Body of the POST request
	 * @param timeout Timeout in seconds
	 * 
	 * @return The response body
	 */
	public String post(String url, Map<String, String> headers, String entity, int timeout) {
		try (CloseableHttpClient httpClient = HttpUtil.getNotValidatingClient(timeout);) {
			HttpPost post = new HttpPost(url);
			putHeaders(post, headers);

			if (StringUtils.isNotEmpty(entity))
				post.setEntity(new StringEntity(entity));

			return httpClient.execute(post, new StringHttpClientResponseHandler());
		} catch (Exception e) {
			log.error(ERROR_CALLING_URL + url, e);
			return null;
		}
	}

	/**
	 * Sends a HTTP POST request containing a urlencoded form 
	 * 
	 * @param url The URL to call
	 * @param headers Optional map of headers
	 * @param fields The fields of the form
	 * @param timeout Timeout in seconds
	 * 
	 * @return The response body
	 */
	public String postForm(String url, Map<String, String> headers, Map<String, String> fields, int timeout) {
		try (CloseableHttpClient httpClient = HttpUtil.getNotValidatingClient(timeout);) {
			HttpPost post = new HttpPost(url);
			putHeaders(post, headers);

			List<NameValuePair> nvps = new ArrayList<>();
			for (Map.Entry<String, String> field : fields.entrySet())
				nvps.add(new BasicNameValuePair(field.getKey(), field.getValue()));

			post.setEntity(new UrlEncodedFormEntity(nvps));
			post.setHeader("Content-Type", "application/x-www-form-urlencoded");

			return httpClient.execute(post, new StringHttpClientResponseHandler());
		} catch (Exception e) {
			log.error(ERROR_CALLING_URL + url, e);
			return null;
		}
	}

	/**
	 * Sends a HTTP POST with multiple parts
	 * 
	 * @param url The URL to call
	 * @param headers Optional map of headers
	 * @param parts Map of the parts
	 * @param timeout Timeout in seconds
	 * 
	 * @return The response body
	 */
	public String postMultipart(String url, Map<String, String> headers, Map<String, AbstractContentBody> parts,
			int timeout) {
		try (CloseableHttpClient httpClient = HttpUtil.getNotValidatingClient(timeout);) {
			HttpPost post = new HttpPost(url);
			putHeaders(post, headers);

			addParts(post, parts);

			return httpClient.execute(post, new StringHttpClientResponseHandler());
		} catch (Exception e) {
			log.error(ERROR_CALLING_URL + url, e);
			return null;
		}
	}

	/**
	 * Downloads a file received in answer to a multipart POST request
	 * 
	 * @param url The URL to call
	 * @param headers Optional map of headers
	 * @param parts Map of the parts
	 * @param timeout Timeout in seconds
	 * 
	 * @return The downloaded file, this is a temporary file, remember to delete it when done
	 */
	public File download(String url, Map<String, String> headers, Map<String, AbstractContentBody> parts, int timeout) {
		File downloadedFile = null;
		try (CloseableHttpClient httpClient = HttpUtil.getNotValidatingClient(timeout);) {
			downloadedFile = FileUtil.createTempFile("download-", ".dld");

			HttpPost post = new HttpPost(url);
			putHeaders(post, headers);

			addParts(post, parts);

			httpClient.execute(post, new FileHttpClientResponseHandler(downloadedFile));
			return downloadedFile;
		} catch (Exception e) {
			log.error(ERROR_CALLING_URL + url, e);
			FileUtil.delete(downloadedFile);
			return null;
		}
	}

	protected void addParts(HttpPost request, Map<String, AbstractContentBody> parts) {
		MultipartEntityBuilder builder = MultipartEntityBuilder.create();
		builder.setMode(HttpMultipartMode.EXTENDED);
		for (Map.Entry<String, AbstractContentBody> part : parts.entrySet())
			builder.addPart(part.getKey(), part.getValue());
		request.setEntity(builder.build());
	}

	private void putHeaders(HttpUriRequestBase request, Map<String, String> headers) {
		if (MapUtils.isNotEmpty(headers))
			for (Map.Entry<String, String> header : headers.entrySet())
				request.setHeader(header.getKey(), header.getValue());
	}
}