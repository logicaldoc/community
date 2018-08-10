package com.logicaldoc.webservicesamples;

import java.util.ArrayList;
import java.util.List;

import org.apache.http.Consts;
import org.apache.http.HttpEntity;
import org.apache.http.HttpStatus;
import org.apache.http.NameValuePair;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicHeader;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.codehaus.jettison.json.JSONObject;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.logicaldoc.webservice.mobile.CommentListVO;
import com.logicaldoc.webservice.mobile.CommentVO;

/**
 * Created by Alessandro Gasparini Date: 9/25/14 Time: 15:15 PM 
 */
public class CommentsRestClient {

	private static final String BASE_PATH = "http://localhost:8080";

	public static void main(String[] args) {

		CommentsRestClient restClient = new CommentsRestClient();
		
		try {
			String sid = restClient.doPostLogin("admin", "admin123");
			//String sid = restClient.doPostLoginJSON("admin", "admin123");
			System.out.println("sid: " + sid);

			long docID = 25069329L;
			restClient.getComments(sid, docID);
			
			//restClient.addCommentJSON(sid, docID);
			//restClient.addCommentForm(sid, docID);
		} catch (Exception e) {
			e.printStackTrace(); 
		}
	}

	private String doPostLoginJSON(String username, String password) throws Exception {
		
		CloseableHttpClient httpclient = getHTTPClient(username, password);

		String input = "{ \"username\" : \"" +username +"\", \"password\" : \"" +password +"\" }";
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
	
	private String doPostLogin(String username, String password) throws Exception {
		
		CloseableHttpClient httpclient = getHTTPClient(username, password);

		List<NameValuePair> formparams = new ArrayList<NameValuePair>();
		formparams.add(new BasicNameValuePair("username", username));
		formparams.add(new BasicNameValuePair("password", password));
		
		UrlEncodedFormEntity entity = new UrlEncodedFormEntity(formparams, Consts.UTF_8);
		
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

	private CloseableHttpClient getHTTPClient(String username, String password) {
		
		CredentialsProvider credsProvider = new BasicCredentialsProvider();
        credsProvider.setCredentials(AuthScope.ANY, new UsernamePasswordCredentials(username, password));
        
        CloseableHttpClient httpclient = HttpClients.custom()
                .setDefaultCredentialsProvider(credsProvider)
                .build();
        
        return httpclient;
	}	

	
	public void getComments(String sid, Long docID) throws Exception {
	
		String output = null;
		try {
			String url = BASE_PATH + "/services/mobile/comments/getcomments/$SID/$ID";
			url = url.replace("$SID", sid);
			url = url.replace("$ID", docID.toString());
			
			System.out.println(url);

			HttpGet method = new HttpGet(url.toString());
			method.setHeader("Accept", "application/json");
			
			CloseableHttpClient httpclient = getHTTPClient("admin", "admin123");
			CloseableHttpResponse response1 = httpclient.execute(method);

			try {
				int code = response1.getStatusLine().getStatusCode();
				System.out.println("HTTPstatus code: "+ code);
				if (code == HttpStatus.SC_OK) {
				} else {
					//log.warn("status code is invalid: {}", code);
					System.err.println("status code is invalid: "+ code);
					throw new Exception(response1.getStatusLine().getReasonPhrase());
				}	
				
				HttpEntity entity2 = response1.getEntity();

				String respoBody = EntityUtils.toString(entity2, "UTF-8");
				System.out.println(respoBody);
				output = respoBody;

				// do something useful with the response body
				// and ensure it is fully consumed
				EntityUtils.consume(entity2);
			} finally {
				response1.close();
			}			
			
			// deserialize the object obtained
			ObjectMapper mapper = new ObjectMapper();
			CommentListVO cl = mapper.readValue(output, CommentListVO.class);
			if (cl != null) {
				List<CommentVO> comments = cl.getItems();				
				System.out.println("comments : " + comments);
				if (comments != null) {
					for (CommentVO commentVO : comments) {
						System.out.println("comment: " +commentVO.getAuthor()  +", " +commentVO.getContent() +", " + commentVO.getModifiedOn());
					}
				}
			}
			
		} catch (Exception e) {
			throw new Exception("Exception in retriving group page info : " + e);
		}
	}


	public void addCommentJSON(String sid, Long docID) throws Exception {

		String url = BASE_PATH + "/services/mobile/comments/addcomment/$SID/$ID";
		url = url.replace("$SID", sid);
		url = url.replace("$ID", docID.toString());

		CloseableHttpClient httpclient = getHTTPClient("admin", "admin123");

			//Use Jackson to fill just one attribute
			JSONObject site = new JSONObject();
			site.put("content", "This is an incomplete CommentVO");
			String jsonstr = site.toString();

			// Just to show Jackson's complete object mapping
			if (1 == 2) {
				CommentVO comment = new CommentVO();
				comment.setContent("Jackson's complete object mapping");

				ObjectMapper mapper = new ObjectMapper();
				jsonstr = mapper.writeValueAsString(comment);
			}

			System.out.println("json : " + jsonstr);
			
			HttpPost httppost = new HttpPost(url);
			httppost.addHeader(new BasicHeader("Accept", "application/json"));
			
			StringEntity entity = new StringEntity(jsonstr, ContentType.create("application/json", Consts.UTF_8));
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
			} catch (Exception e) {
				throw new Exception("Exception in adding bucket : " + e);
			} finally {
				response.close();
			} 
	}


	public void addCommentForm(String sid, Long docID) throws Exception {

		//String output = null;

			String url = BASE_PATH + "/services/mobile/comments/addcommentform/$SID/$ID";
			url = url.replace("$SID", sid);
			url = url.replace("$ID", docID.toString());
			
			List<NameValuePair> formparams = new ArrayList<NameValuePair>();
			formparams.add(new BasicNameValuePair("content", "This is a comment added via CommentForm"));
			
			UrlEncodedFormEntity entity = new UrlEncodedFormEntity(formparams, Consts.UTF_8);
			
			HttpPost httppost = new HttpPost(url);
			httppost.addHeader(new BasicHeader("Accept", "application/json"));
			//httppost.addHeader(new BasicHeader("Accept", "application/xml"));
			httppost.setEntity(entity);	
			
			CloseableHttpClient httpclient = getHTTPClient("admin", "admin123");
			CloseableHttpResponse response = httpclient.execute(httppost);
			
			try {
				int code = response.getStatusLine().getStatusCode();
				System.out.println("HTTPstatus code: "+ code);
				if (code == HttpStatus.SC_OK) {
					HttpEntity rent = response.getEntity();
					if (rent != null) {
						String respoBody = EntityUtils.toString(rent, "UTF-8");
						System.out.println(respoBody);
					}
				} else {
					//log.warn("status code is invalid: {}", code);
					System.err.println("status code is invalid: "+ code);
					throw new Exception(response.getStatusLine().getReasonPhrase());
				}
			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				response.close();
			}		

	}

}