package com.logicaldoc.webservice;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.hc.client5.http.ClientProtocolException;
import org.apache.hc.client5.http.auth.AuthScope;
import org.apache.hc.client5.http.auth.CredentialsProvider;
import org.apache.hc.client5.http.classic.methods.HttpDelete;
import org.apache.hc.client5.http.classic.methods.HttpGet;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.entity.UrlEncodedFormEntity;
import org.apache.hc.client5.http.entity.mime.FileBody;
import org.apache.hc.client5.http.entity.mime.HttpMultipartMode;
import org.apache.hc.client5.http.entity.mime.MultipartEntityBuilder;
import org.apache.hc.client5.http.entity.mime.StringBody;
import org.apache.hc.client5.http.impl.auth.CredentialsProviderBuilder;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.HttpClientBuilder;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.core5.http.ContentType;
import org.apache.hc.core5.http.HttpEntity;
import org.apache.hc.core5.http.NameValuePair;
import org.apache.hc.core5.http.io.entity.StringEntity;
import org.apache.hc.core5.http.message.BasicHeader;
import org.apache.hc.core5.http.message.BasicNameValuePair;
import org.apache.hc.core5.net.URIBuilder;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.logicaldoc.util.http.StringHttpClientResponseHandler;
import com.logicaldoc.util.io.CharsetUtil;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSSearchOptions;

public class HttpRestWb {

	// public static String BASE_PATH = "http://localhost/logicaldoc";
	public static String BASE_PATH = "http://localhost:8086";

	public static String USERNAME = "admin";

	public static String PASSWORD = "admin";

	protected static CloseableHttpClient getHttpClient() {
		// Client credentials
		CredentialsProvider credentialsProvider = CredentialsProviderBuilder.create()
				.add(new AuthScope(null, null, -1, null, null), USERNAME, PASSWORD.toCharArray()).build();
		return HttpClientBuilder.create().setDefaultCredentialsProvider(credentialsProvider).build();
	}

	public static void main(String[] args) throws Exception {

		// String sid = loginJSON();

		try (CloseableHttpClient httpClient = getHttpClient()) {

			// uploadDocument(httpclient);

			// createFolderSimpleForm(httpclient);
			// createFolderSimpleJSON(httpclient);

			createDocument(httpClient);
			/*
			 * 
			 * //listDocuments(httpclient, 04L); //listChildren(httpclient,
			 * 04L);
			 * 
			 * //long start_time = System.nanoTime();
			 * 
			 * // WSSearchOptions wsso = buildSearchOptions("en",
			 * "document management system"); // find(httpclient, wsso);
			 * 
			 * 
			 * 
			 * /* wsso = buildSearchOptions("en", "document management");
			 * find(sid, wsso);
			 * 
			 * wsso = buildSearchOptions("en", "Document Management"); find(sid,
			 * wsso);
			 * 
			 * wsso = buildSearchOptions("en", "Document Management system");
			 * find(sid, wsso);
			 * 
			 * wsso = buildSearchOptions("en", "Management system"); find(sid,
			 * wsso);
			 * 
			 * wsso = buildSearchOptions("en", "document system"); find(sid,
			 * wsso);
			 * 
			 * wsso = buildSearchOptions("en", "documental system"); find(sid,
			 * wsso);
			 * 
			 * wsso = buildSearchOptions("en", "documental system"); find(sid,
			 * wsso);
			 * 
			 * wsso = buildSearchOptions("en", "electronic document system");
			 * find(sid, wsso);
			 * 
			 * wsso = buildSearchOptions("en",
			 * "electronic document management system"); find(sid, wsso);
			 * 
			 * wsso = buildSearchOptions("en",
			 * "electronic system for document management"); find(sid, wsso);
			 * 
			 * long end_time = System.nanoTime(); double difference = (end_time
			 * - start_time)/1e6; System.out.println("Total Exec. time (ms): "
			 * +difference);
			 */

			// Total Exec. time (ms): 681.909198
			// Total Exec. time (ms): 737.044408
			// Total Exec. time (ms): 705.149953
			// Total Exec. time (ms): 739.934107
			// Total Exec. time (ms): 767.015352

			// Con deserializzazione in Java object e riserializzaione in json
			// (per
			// system out)
			// Total Exec. time (ms): 1114.652089
			// Total Exec. time (ms): 951.179299
			// Total Exec. time (ms): 1080.464628
			// Total Exec. time (ms): 973.297579
			// Total Exec. time (ms): 1062.980412
			// Total Exec. time (ms): 1064.021243

			// createPath(httpclient, 04L, "/sgsgsgs/Barisoni/rurururu");
			// if (1 == 1) return;

			String sid = getSid(httpClient);

			logout(httpClient, sid);
		}
	}

	private static String getSid(CloseableHttpClient httpClient) throws ClientProtocolException, IOException {
		return httpClient.execute(new HttpGet(BASE_PATH + "/services/rest/auth/getSid"),
				new StringHttpClientResponseHandler());
	}

	private static void logout(CloseableHttpClient httpClient, String sid) throws ClientProtocolException, IOException {
		httpClient.execute(new HttpDelete(BASE_PATH + "/services/rest/auth/logout?sid=" + sid),
				new StringHttpClientResponseHandler());
	}

	private static void createPath(CloseableHttpClient httpClient, long parentId, String path) throws Exception {
		List<NameValuePair> formparams = new ArrayList<>();
		formparams.add(new BasicNameValuePair("parentId", String.valueOf(parentId)));
		formparams.add(new BasicNameValuePair("path", path));

		UrlEncodedFormEntity entity = new UrlEncodedFormEntity(formparams, CharsetUtil.utf8());

		HttpPost httppost = new HttpPost(BASE_PATH + "/services/rest/folder/createPath");
		httppost.setEntity(entity);

		httpClient.execute(httppost, new StringHttpClientResponseHandler());
	}

	private static WSSearchOptions buildSearchOptions(String lang1, String expression) {
		WSSearchOptions options = new WSSearchOptions();

		String lang = lang1;

		// This is the language of the document
		options.setLanguage(lang);
		options.setExpression(expression);

		// This is the language of the query
		options.setExpressionLanguage(lang);

		// This is required and it is the maximum number of results that we want
		// for this search
		options.setMaxHits(50);

		return options;
	}

	private static void find(CloseableHttpClient httpClient, WSSearchOptions wsso) throws Exception {

		System.out.println("find");
		// CloseableHttpClient httpclient = HttpClients.createDefault();

		HttpPost httppost = new HttpPost(BASE_PATH + "/services/rest/search/find");
		httppost.addHeader(new BasicHeader("Accept", "application/json"));

		// ObjectWriter ow = new
		// ObjectMapper().writer().withDefaultPrettyPrinter();
		ObjectMapper mapper = new ObjectMapper();
		ObjectWriter ow = mapper.writer();
		String jsonStr = ow.writeValueAsString(wsso);
		System.out.println(jsonStr);

		StringEntity entity = new StringEntity(jsonStr, ContentType.create("application/json", CharsetUtil.utf8()));
		httppost.setEntity(entity);

		String body = httpClient.execute(httppost, new StringHttpClientResponseHandler());
		System.out.println(body);
	}

	private static void listDocuments(CloseableHttpClient httpClient, long parentId)
			throws ClientProtocolException, IOException, URISyntaxException {

		System.out.println("listDocuments");

		URIBuilder uriBuilder = new URIBuilder(BASE_PATH + "/services/rest/document/listDocuments");
		uriBuilder.setParameter("folderId", String.valueOf(parentId)).setParameter("fileName", "*");

		HttpGet get = new HttpGet(uriBuilder.build());
		get.setHeader("Accept", "application/json");

		String body = httpClient.execute(get, new StringHttpClientResponseHandler());
		System.out.println(body);
	}

	private static void listChildren(CloseableHttpClient httpclient, long parentId)
			throws ClientProtocolException, IOException, URISyntaxException {

		URIBuilder uriBuilder = new URIBuilder(BASE_PATH + "/services/rest/folder/listChildren");
		uriBuilder.setParameter("folderId", String.valueOf(parentId)).setParameter("fileName", "*");

		HttpGet get = new HttpGet(uriBuilder.build());
		get.setHeader("Accept", "application/json");

		String body = httpclient.execute(get, new StringHttpClientResponseHandler());
		System.out.println(body);
	}

	private static void createDocument(CloseableHttpClient httpClient) throws IOException {

		System.out.println("createDocument(CloseableHttpClient)");
		// CloseableHttpClient httpclient = HttpClients.createDefault();

		// HttpPost httppost = new HttpPost(BASE_PATH +
		// "/services/rest/document/create");
		HttpPost post = new HttpPost(BASE_PATH + "/services/rest/document/createDocument02");

		File f = new File("C:/tmp/InvoiceProcessing01-workflow.png");
		System.out.println(f.getName());

		WSDocument wsDoc = new WSDocument();
		wsDoc.setId(0);
		wsDoc.setCustomId("CustomId-xxxxx4444"); // warning this might produce
													// sql errors: integrity
													// constraint violation:
													// unique constraint or
													// index violation:
													// AK_DOCUMENT; SQL [n/a];
													// constraint [null];
		wsDoc.setTags(Arrays.asList("Invoice", "Processing", "workflow"));
		wsDoc.setFolderId(4L);
		wsDoc.setFileName(f.getName());

		ObjectWriter ow = new ObjectMapper().writer().withDefaultPrettyPrinter();
		String jsonStr = ow.writeValueAsString(wsDoc);

		StringBody jsonPart = new StringBody(jsonStr, ContentType.APPLICATION_JSON);
		FileBody binPart = new FileBody(f);

		HttpEntity reqEntity = MultipartEntityBuilder.create().addPart("document", jsonPart).addPart("content", binPart)
				.build();

		post.setEntity(reqEntity);

		String body = httpClient.execute(post, new StringHttpClientResponseHandler());
		System.out.println(body);
	}

	private static void createFolderSimpleForm(CloseableHttpClient httpClient) throws Exception {

		// This will create a tree starting from the Root folder (not in the
		// Default workspace)
		List<NameValuePair> formparams = new ArrayList<>();
		formparams.add(new BasicNameValuePair("folderPath", "/LogicalDOC/USA/NJ/Fair Lawn/createSimple"));

		UrlEncodedFormEntity entity = new UrlEncodedFormEntity(formparams, CharsetUtil.utf8());

		HttpPost post = new HttpPost(BASE_PATH + "/services/rest/folder/createSimpleForm");
		post.setEntity(entity);

		String respoBody = httpClient.execute(post, new StringHttpClientResponseHandler());
		System.out.println(respoBody);
	}

	private static void createFolderSimpleJSON(CloseableHttpClient httpClient) throws IOException {
		// This will create a tree starting from the Default workspace
		String folderPath = "/Default/USA/NJ/Fair Lawn/createFolder/Simple/JSON";
		String input = "{ \"folderPath\" : \"" + folderPath + "\" }";
		System.out.println(input);

		HttpPost post = new HttpPost(BASE_PATH + "/services/rest/folder/createSimpleJSON");
		StringEntity entity = new StringEntity(input, ContentType.create("application/json", CharsetUtil.utf8()));
		post.setEntity(entity);

		String body = httpClient.execute(post, new StringHttpClientResponseHandler());
		System.out.println(body);
	}

	private static String loginJSON() throws IOException {
		try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
			String input = "{ \"username\" : \"admin\", \"password\" : \"admin\" }";
			System.out.println(input);

			StringEntity entity = new StringEntity(input, ContentType.create("application/json", CharsetUtil.utf8()));
			HttpPost post = new HttpPost(BASE_PATH + "/services/rest/auth/login");
			post.setEntity(entity);

			String body = httpclient.execute(post, new StringHttpClientResponseHandler());
			System.out.println(body);
			return body;
		}
	}

	private static void uploadDocument02(CloseableHttpClient httpClient) throws IOException {

		System.out.println("uploadDocument(CloseableHttpClient)");

		HttpPost post = new HttpPost(BASE_PATH + "/services/rest/document/upload");

		// File file = new File("C:/tmp/InvoiceProcessing01-workflow.png");
		File file = new File("c:/users/shatz/Downloads/SimpleTestProject.zip");
		// File file = new File("C:/tmp/testContent.txt");

		System.out.println(file.getName());

		long folderID = 124358812L;

		StringBody fnamePart = new StringBody(file.getName(), ContentType.TEXT_PLAIN);
		StringBody folderPart = new StringBody(Long.toString(folderID), ContentType.TEXT_PLAIN);
		FileBody binPart = new FileBody(file, ContentType.DEFAULT_BINARY);

		HttpEntity reqEntity = MultipartEntityBuilder.create().addPart("filename", fnamePart)
				.addPart("filedata", binPart).addPart("folderId", folderPart).build();

		post.setEntity(reqEntity);

		/*
		 * int timeout = 5; // seconds HttpParams httpParams =
		 * httpclient.getParams(); HttpConnectionParams.setConnectionTimeout(
		 * httpParams, timeout * 1000); // http.connection.timeout
		 * HttpConnectionParams.setSoTimeout( httpParams, timeout * 1000); //
		 * http.socket.timeout
		 */

		String respoBody = httpClient.execute(post, new StringHttpClientResponseHandler());
		System.out.println(respoBody);
	}

	private static void uploadDocument(CloseableHttpClient httpClient) throws IOException {
		System.out.println("uploadDocument(CloseableHttpClient)");

		HttpPost post = new HttpPost(BASE_PATH + "/services/rest/document/upload");

		File file = new File("C:/tmp/serv.txt");

		System.out.println(file.getName());
		System.out.println(file.getAbsolutePath());

		long folderID = 100L;

		MultipartEntityBuilder builder = MultipartEntityBuilder.create();
		builder.setMode(HttpMultipartMode.EXTENDED);
		builder.addTextBody("filename", file.getName(), ContentType.TEXT_PLAIN);
		builder.addBinaryBody("filedata", file, ContentType.DEFAULT_BINARY, file.getName());
		builder.addTextBody("folderId", Long.toString(folderID), ContentType.TEXT_PLAIN);

		HttpEntity entity = builder.build();
		post.setEntity(entity);

		String body = httpClient.execute(post, new StringHttpClientResponseHandler());
		System.out.println(body);
	}
}