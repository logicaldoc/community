package com.logicaldoc.webservice;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.apache.http.Consts;
import org.apache.http.HttpEntity;
import org.apache.http.HttpStatus;
import org.apache.http.NameValuePair;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicHeader;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSSearchOptions;

public class HttpRestWb {

	public static String BASE_PATH = "http://localhost:8080/logicaldoc";

	public static void main(String[] args) throws Exception {

		//String sid = loginJSON();

		CredentialsProvider credsProvider = new BasicCredentialsProvider();
        credsProvider.setCredentials(AuthScope.ANY, new UsernamePasswordCredentials("admin", "admin"));
        
        CloseableHttpClient httpclient = HttpClients.custom()
                .setDefaultCredentialsProvider(credsProvider)
                .build();
        
		createFolderSimpleForm(httpclient);
		createFolderSimpleJSON(httpclient);

		createDocument( httpclient);
		listDocuments(httpclient, 04L);
		listChildren(httpclient, 04L);
		
		long start_time = System.nanoTime();
		
        WSSearchOptions wsso = buildSearchOptions("en", "document management system");
		find(httpclient, wsso);
		
		/*
        wsso = buildSearchOptions("en", "document management");
		find(sid, wsso);
		
        wsso = buildSearchOptions("en", "Document Management");
		find(sid, wsso);
		
        wsso = buildSearchOptions("en", "Document Management system");
		find(sid, wsso);
		
        wsso = buildSearchOptions("en", "Management system");
		find(sid, wsso);
		
        wsso = buildSearchOptions("en", "document system");
		find(sid, wsso);	
		
        wsso = buildSearchOptions("en", "documental system");
		find(sid, wsso);
		
        wsso = buildSearchOptions("en", "documental system");
		find(sid, wsso);
		
        wsso = buildSearchOptions("en", "electronic document system");
		find(sid, wsso);
		
        wsso = buildSearchOptions("en", "electronic document management system");
		find(sid, wsso);
		
        wsso = buildSearchOptions("en", "electronic system for document management");
		find(sid, wsso);
		
		long end_time = System.nanoTime();
		double difference = (end_time - start_time)/1e6;
		System.out.println("Total Exec. time (ms): " +difference);
		*/
		
		// Total Exec. time (ms): 681.909198
		// Total Exec. time (ms): 737.044408
		// Total Exec. time (ms): 705.149953
		// Total Exec. time (ms): 739.934107
		// Total Exec. time (ms): 767.015352
		
		// Con deserializzazione in Java object e riserializzaione in json (per system out)
		// Total Exec. time (ms): 1114.652089
		// Total Exec. time (ms): 951.179299
		// Total Exec. time (ms): 1080.464628
		// Total Exec. time (ms): 973.297579
		// Total Exec. time (ms): 1062.980412
		// Total Exec. time (ms): 1064.021243
		
		createPath(httpclient, 04L, "/sgsgsgs/Barisoni/rurururu");
		
		logout(httpclient);
	}
	
	private static void logout(CloseableHttpClient httpclient) throws ClientProtocolException, IOException {

		HttpDelete deletem = new HttpDelete(BASE_PATH + "/services/rest/auth/logout");	

		CloseableHttpResponse response = httpclient.execute(deletem);
		try {
			HttpEntity rent = response.getEntity();
			if (rent != null) {
				String respoBody = EntityUtils.toString(rent, "UTF-8");
				System.out.println(respoBody);
			}
		} finally {
			response.close();
		}		
	}

	private static void createPath(CloseableHttpClient httpclient, long parentId, String path) throws Exception {
		
		//CloseableHttpClient httpclient = HttpClients.createDefault();
		
		List<NameValuePair> formparams = new ArrayList<NameValuePair>();
		formparams.add(new BasicNameValuePair("parentId", String.valueOf(parentId)));
		formparams.add(new BasicNameValuePair("path", path));
		
		UrlEncodedFormEntity entity = new UrlEncodedFormEntity(formparams, Consts.UTF_8);
		
		HttpPost httppost = new HttpPost(BASE_PATH + "/services/rest/folder/createPath");
		httppost.setEntity(entity);		

		CloseableHttpResponse response = httpclient.execute(httppost);
		try {
			HttpEntity rent = response.getEntity();
			if (rent != null) {
				String respoBody = EntityUtils.toString(rent, "UTF-8");
				System.out.println(respoBody);
			}
		} finally {
			response.close();
		}		
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
	
	private static void find(CloseableHttpClient httpclient, WSSearchOptions wsso) throws Exception {

		System.out.println("find");
		//CloseableHttpClient httpclient = HttpClients.createDefault();
		
        HttpPost httppost = new HttpPost(BASE_PATH + "/services/rest/search/find");
        httppost.addHeader(new BasicHeader("Accept", "application/json"));

		//ObjectWriter ow = new ObjectMapper().writer().withDefaultPrettyPrinter();
        ObjectMapper mapper = new ObjectMapper();
        ObjectWriter ow = mapper.writer();
		String jsonStr = ow.writeValueAsString(wsso);
		System.out.println(jsonStr);
		
		StringEntity entity = new StringEntity(jsonStr, ContentType.create("application/json", Consts.UTF_8));
		httppost.setEntity(entity);		

		CloseableHttpResponse response = httpclient.execute(httppost);
		
		int code = response.getStatusLine().getStatusCode();
		System.out.println("HTTPstatus code: "+ code);
		
		if (code == HttpStatus.SC_OK) {
		} else {
			//log.warn("status code is invalid: {}", code);
			System.err.println("status code is invalid: "+ code);
			throw new Exception(response.getStatusLine().getReasonPhrase());
		}			
		
		try {
			HttpEntity rent = response.getEntity();
			if (rent != null) {
				String respoBody = EntityUtils.toString(rent, "UTF-8");
				System.out.println(respoBody);
				
				//JSON from String to Object
				//WSSearchResult obj = mapper.readValue(respoBody, WSSearchResult.class);
				//System.out.println(ow.writeValueAsString(obj) );				
			}
		} finally {
			response.close();
		}
	}	
	
	private static void listDocuments(CloseableHttpClient httpclient, long parentId) throws ClientProtocolException, IOException {

		System.out.println("listDocuments");

		List<NameValuePair> params = new LinkedList<NameValuePair>();
		params.add(new BasicNameValuePair("folderId", String.valueOf(parentId)));
		//params.add(new BasicNameValuePair("fileName", "gjhghjgj")); // this should result nothing
		//params.add(new BasicNameValuePair("fileName", "InvoiceProcessing01-workflow.png"));
		//params.add(new BasicNameValuePair("fileName", "*.png"));
		params.add(new BasicNameValuePair("fileName", "*"));

		StringBuilder requestUrl = new StringBuilder(BASE_PATH + "/services/rest/document/listDocuments");
		String querystring = URLEncodedUtils.format(params, "utf-8");
		requestUrl.append("?");
		requestUrl.append(querystring);

		System.out.println(requestUrl);

		//CloseableHttpClient httpclient = HttpClients.createDefault();
		HttpGet method = new HttpGet(requestUrl.toString());
		method.setHeader("Accept", "application/json");

		CloseableHttpResponse response1 = httpclient.execute(method);

		try {
			System.out.println(response1.getStatusLine());
			HttpEntity entity2 = response1.getEntity();

			String respoBody = EntityUtils.toString(entity2, "UTF-8");
			System.out.println(respoBody);

			// do something useful with the response body
			// and ensure it is fully consumed
			EntityUtils.consume(entity2);
		} finally {
			response1.close();
		}
	}	

	private static void listChildren(CloseableHttpClient httpclient, long parentId) throws ClientProtocolException, IOException {

		//System.out.println("sid: " + httpclient);

		List<NameValuePair> params = new LinkedList<NameValuePair>();
		params.add(new BasicNameValuePair("folderId", String.valueOf(parentId)));

		StringBuilder requestUrl = new StringBuilder(BASE_PATH + "/services/rest/folder/listChildren");
		String querystring = URLEncodedUtils.format(params, "utf-8");
		requestUrl.append("?");
		requestUrl.append(querystring);

		System.out.println(requestUrl);

		//CloseableHttpClient httpclient = HttpClients.createDefault();
		HttpGet method = new HttpGet(requestUrl.toString());
		method.setHeader("Accept", "application/json");

		CloseableHttpResponse response1 = httpclient.execute(method);

		try {
			System.out.println(response1.getStatusLine());
			HttpEntity entity2 = response1.getEntity();

			String respoBody = EntityUtils.toString(entity2, "UTF-8");
			System.out.println(respoBody);

			// do something useful with the response body
			// and ensure it is fully consumed
			EntityUtils.consume(entity2);
		} finally {
			response1.close();
		}
	}

	private static void createDocument(CloseableHttpClient httpclient) throws IOException {

		System.out.println("createDocument(CloseableHttpClient)");
		//CloseableHttpClient httpclient = HttpClients.createDefault();
		
        HttpPost httppost = new HttpPost(BASE_PATH + "/services/rest/document/create");

		File f = new File("C:/tmp/InvoiceProcessing01-workflow.png");
		System.out.println(f.getName());
		
		WSDocument wsDoc = new WSDocument();
		wsDoc.setId(0);
		wsDoc.setCustomId("CustomId-xxxxx4444"); // warning this might produce sql errors: integrity constraint violation: unique constraint or index violation: AK_DOCUMENT; SQL [n/a]; constraint [null];
		wsDoc.setTags(new String[] { "Invoice", "Processing", "workflow" });
		wsDoc.setFolderId(4L);
		wsDoc.setFileName(f.getName());

		ObjectWriter ow = new ObjectMapper().writer().withDefaultPrettyPrinter();
		String jsonStr = ow.writeValueAsString(wsDoc);		
        
		StringBody jsonPart = new StringBody(jsonStr, ContentType.APPLICATION_JSON);
        FileBody binPart = new FileBody(f);        

        HttpEntity reqEntity = MultipartEntityBuilder.create()
                .addPart("document", jsonPart)
                .addPart("content", binPart)
                .build();
		
		httppost.setEntity(reqEntity);
		

		CloseableHttpResponse response = httpclient.execute(httppost);
		try {
			HttpEntity rent = response.getEntity();
			if (rent != null) {
				String respoBody = EntityUtils.toString(rent, "UTF-8");
				System.out.println(respoBody);
			}
		} finally {
			response.close();
		}
	}

	private static void createFolderSimpleForm(CloseableHttpClient httpclient) throws Exception { 
		
		// This will create a tree starting from the Root folder (not in the Default workspace)
		List<NameValuePair> formparams = new ArrayList<NameValuePair>();
		formparams.add(new BasicNameValuePair("folderPath", "/LogicalDOC/USA/NJ/Fair Lawn/createSimple"));
		
		UrlEncodedFormEntity entity = new UrlEncodedFormEntity(formparams, Consts.UTF_8);
		
		HttpPost httppost = new HttpPost(BASE_PATH + "/services/rest/folder/createSimpleForm");
		httppost.setEntity(entity);		

		CloseableHttpResponse response = httpclient.execute(httppost);
		int code = response.getStatusLine().getStatusCode();
		System.out.println("HTTPstatus code: "+ code);
		if (code == HttpStatus.SC_OK) {
		} else {
			//log.warn("status code is invalid: {}", code);
			System.err.println("status code is invalid: "+ code);
			throw new Exception(response.getStatusLine().getReasonPhrase());
		}				
		
		try {
			HttpEntity rent = response.getEntity();
			if (rent != null) {
				String respoBody = EntityUtils.toString(rent, "UTF-8");
				System.out.println(respoBody);
			}
		} finally {
			response.close();
		}
	}

	private static void createFolderSimpleJSON(CloseableHttpClient httpclient)
			throws UnsupportedEncodingException, IOException {

//		CloseableHttpClient httpclient = HttpClients.createDefault();

		// This will create a tree starting from the Default workspace
		String folderPath = "/Default/USA/NJ/Fair Lawn/createSimple/JSON";
		String input = "{ \"folderPath\" : \"" + folderPath + "\" }";
		System.out.println(input);

		HttpPost httppost = new HttpPost(BASE_PATH + "/services/rest/folder/createSimpleJSON");
		StringEntity entity = new StringEntity(input, ContentType.create("application/json", Consts.UTF_8));
		httppost.setEntity(entity);

		CloseableHttpResponse response = httpclient.execute(httppost);
		try {
			HttpEntity rent = response.getEntity();
			if (rent != null) {
				String respoBody = EntityUtils.toString(rent, "UTF-8");
				System.out.println(respoBody);
			}
		} finally {
			response.close();
		}
	}

	private static String loginJSON() throws UnsupportedEncodingException, IOException {

		CloseableHttpClient httpclient = HttpClients.createDefault();

		String input = "{ \"username\" : \"admin\", \"password\" : \"admin\" }";
		System.out.println(input);

		StringEntity entity = new StringEntity(input, ContentType.create("application/json", Consts.UTF_8));
		HttpPost httppost = new HttpPost(BASE_PATH + "/services/rest/auth/login");
		httppost.setEntity(entity);

		CloseableHttpResponse response = httpclient.execute(httppost);
		try {
			HttpEntity rent = response.getEntity();
			if (rent != null) {
				String respoBody = EntityUtils.toString(rent, "UTF-8");
				System.out.println(respoBody);
				return respoBody;
			}
		} finally {
			response.close();
		}

		return null;
	}

}
