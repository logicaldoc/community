package com.logicaldoc.webservice.mobile;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URLEncoder;
import java.util.Arrays;
import java.util.List;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;

import org.apache.commons.io.IOUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.logicaldoc.webservice.model.WSBookmark;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSRating;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;
import com.logicaldoc.webservice.rest.client.RestAuthClient;
import com.logicaldoc.webservice.rest.client.RestBookmarkClient;
import com.logicaldoc.webservice.rest.client.RestDocumentClient;
import com.logicaldoc.webservice.rest.client.RestDocumentMetadataClient;
import com.logicaldoc.webservice.rest.client.RestFolderClient;
import com.logicaldoc.webservice.rest.client.RestSearchClient;
import com.logicaldoc.webservice.rest.client.RestTagClient;


import org.apache.http.Consts;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHost;
import org.apache.http.HttpStatus;
import org.apache.http.NameValuePair;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.AuthCache;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.protocol.HttpClientContext;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.entity.mime.HttpMultipartMode;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.auth.BasicScheme;
import org.apache.http.impl.client.BasicAuthCache;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicHeader;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;

/**
 * Created by IntelliJ IDEA. User: Niraj Singh Date: 3/13/13 Time: 4:15 PM To
 * change this template use File | Settings | File Templates.
 */
public class CommentsRestClient {

	public static void main(String[] args) {

		CommentsRestClient restClient = new CommentsRestClient();

		try {
			String sid = restClient.doGetLogin("admin", "admin");
			// String sid = restClient.doPostLogin("admin", "admin");
			System.out.println("sid: " + sid);

			restClient.addCommentJSON(sid, "6750209");
			// restClient.getComments(sid, "6750209");
		} catch (Exception e) {
			e.printStackTrace(); // To change body of catch statement use File |
									// Settings | File Templates.
		}
	}

	private String doPostLogin(String username, String password) throws Exception {
		String output = null;
//		try {
//			String url = "http://localhost:8080/services/rest/auth/login";
//
//			HttpClient client = new HttpClient();
//			PostMethod mPost = new PostMethod(url);
//
//			mPost.addParameter("username", username);
//			mPost.addParameter("password", password);
//
//			Header mtHeader = new Header();
//			mtHeader.setName("content-type");
//			mtHeader.setValue("application/x-www-form-urlencoded");
//			mPost.addRequestHeader(mtHeader);
//
//			mtHeader = new Header();
//			mtHeader.setName("accept");
//			mtHeader.setValue(MediaType.TEXT_PLAIN);
//			mPost.addRequestHeader(mtHeader);
//
//			int statusCode = client.executeMethod(mPost);
//			System.out.println("statusCode: " + statusCode);
//
//			if (statusCode != HttpStatus.SC_OK) {
//				System.err.println("Method failed: " + mPost.getStatusLine());
//			}
//
//			output = mPost.getResponseBodyAsString();
//			mPost.releaseConnection();
//
//			System.out.println("out : " + output);
//		} catch (Exception e) {
//			throw new Exception("Exception in retriving group page info : " + e);
//		}
		return output;
	}

	private String doGetLogin(String username, String password) throws Exception {
		String output = null;
//		try {
//			String url = "http://localhost:8080/services/rest/auth/loginquery";
//
//			HttpClient client = new HttpClient();
//			GetMethod mGet = new GetMethod(url);
//
//			mGet.setQueryString("u=" + username + "&pw=" + password);
//
//			Header mtHeader = new Header();
//			mtHeader.setName("accept");
//			mtHeader.setValue("application/json");
//			// mtHeader.setValue("text/plain");
//			mGet.addRequestHeader(mtHeader);
//
//			int statusCode = client.executeMethod(mGet);
//			System.out.println("statusCode: " + statusCode);
//
//			if (statusCode != HttpStatus.SC_OK) {
//				System.err.println("Method failed: " + mGet.getStatusLine());
//			}
//
//			output = mGet.getResponseBodyAsString();
//			mGet.releaseConnection();
//
//			System.out.println("out : " + output);
//		} catch (Exception e) {
//			throw new Exception("Exception in retriving group page info : " + e);
//		}
		return output;
	}

	public void getComments(String sid, String docId) throws Exception {

		String output = null;
//		try {
//			String url = "http://localhost:8080/services/mobile/comments/getcomments/$SID/$ID";
//			url = url.replace("$SID", sid);
//			url = url.replace("$ID", docId);
//
//			HttpClient client = new HttpClient();
//			GetMethod mGet = new GetMethod(url);
//
//			Header mtHeader = new Header();
//			mtHeader.setName("accept");
//			// mtHeader.setValue("application/xml");
//			mtHeader.setValue("application/json");
//			mGet.addRequestHeader(mtHeader);
//
//			int statusCode = client.executeMethod(mGet);
//			System.out.println("statusCode: " + statusCode);
//
//			if (statusCode != HttpStatus.SC_OK) {
//				System.err.println("Method failed: " + mGet.getStatusLine());
//			}
//
//			output = mGet.getResponseBodyAsString();
//			mGet.releaseConnection();
//
//			System.out.println("out : " + output);
//		} catch (Exception e) {
//			throw new Exception("Exception in retriving group page info : " + e);
//		}
	}

	public void addCommentJSON(String sid, String docId) throws Exception {

//		String output = null;
//		HttpClient client = new HttpClient();
//
//		String url = "http://localhost:8080/services/mobile/comments/addcomment/$SID/$ID";
//		url = url.replace("$SID", sid);
//		url = url.replace("$ID", docId);
//
//		PostMethod mPost = new PostMethod(url);
//
//		try {
//			//Use Jackson to fill just one attribute
//			JSONObject site = new JSONObject();
//			site.put("content", "This is an incomplete CommentVO");
//			String json = site.toString();
//
//			// Just to show Jackson's complete object mapping
//			if (1 == 2) {
//				CommentVO comment = new CommentVO();
//				comment.setContent("content");
//
//				ObjectMapper mapper = new ObjectMapper();
//				// String json = mapper.writeValueAsString(comment);
//			}
//
//			System.out.println("json : " + json);
//
//			Header mtHeader = new Header();
//			mtHeader.setName("content-type");
//			mtHeader.setValue("application/json");
//			mPost.addRequestHeader(mtHeader);
//
//			mtHeader = new Header();
//			mtHeader.setName("accept");
//			mtHeader.setValue("application/json");
//			mPost.addRequestHeader(mtHeader);
//
//			// mPost.setRequestEntity(new StringRequestEntity(site.toString(),
//			// "application/json", "UTF-8"));
//			mPost.setRequestEntity(new StringRequestEntity(json, "application/json", "UTF-8"));
//
//			for (Header header : mPost.getRequestHeaders()) {
//				System.out.println(header.getName() + ": " + header.getValue());
//			}
//
//			System.out.println(" mPost.getRequestEntity(): " + mPost.getRequestEntity().getContentLength());
//			System.out.println(" mPostgetContentType(): " + mPost.getRequestEntity().getContentType());
//
//			int statusCode = client.executeMethod(mPost);
//			System.out.println("statusCode: " + statusCode);
//
//			if (statusCode != HttpStatus.SC_OK) {
//				System.err.println("Method failed: " + mPost.getStatusLine());
//			}
//
//			output = mPost.getResponseBodyAsString();
//			mPost.releaseConnection();
//			System.out.println("output : " + output);
//		} catch (Exception e) {
//			throw new Exception("Exception in adding bucket : " + e);
//		}
	}

	public void addCommentForm(String docId) throws Exception {

		/*
		String output = null;
		try {
			String url = "http://localhost:9080/services/commentservice/addcommentform/";
			url = url + URLEncoder.encode(docId, "UTF-8");

			HttpClient client = new HttpClient();
			PostMethod mPost = new PostMethod(url);

			mPost.addParameter("title", "title");
			mPost.addParameter("content", "content");

			Header mtHeader = new Header();
			mtHeader.setName("content-type");
			mtHeader.setValue("application/x-www-form-urlencoded");
			mPost.addRequestHeader(mtHeader);

			mtHeader = new Header();
			mtHeader.setName("accept");
			// mtHeader.setValue("application/xml");
			mtHeader.setValue("application/json");
			mPost.addRequestHeader(mtHeader);

			for (Header header : mPost.getRequestHeaders()) {
				System.out.println(header.getName() + ": " + header.getValue());
			}

			System.out.println(" mPost.getRequestEntity(): " + mPost.getRequestEntity().getContentLength());
			System.out.println(" mPostgetContentType(): " + mPost.getRequestEntity().getContentType());

			int statusCode = client.executeMethod(mPost);
			System.out.println("statusCode: " + statusCode);

			if (statusCode != HttpStatus.SC_OK) {
				System.err.println("Method failed: " + mPost.getStatusLine());
			}

			output = mPost.getResponseBodyAsString();
			mPost.releaseConnection();
			System.out.println("output : " + output);
		} catch (Exception e) {
			throw new Exception("Exception in adding bucket : " + e);
		}
*/
	}

}