package com.logicaldoc.webservicesamples;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;

import org.apache.commons.io.IOUtils;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.mime.HttpMultipartMode;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils; 

public class RestJodConverter {
	
	protected static final String ENCODING = "UTF-8"; 
	
	public static void main(String[] args) throws Exception {
		addPartMethod01();
	}
	
    protected static String responseAsString(CloseableHttpResponse response) throws IOException { 
        return streamAsString(response.getEntity().getContent()); 
    } 
    
    protected static String streamAsString(InputStream inputStream) throws IOException { 
        StringWriter writer = new StringWriter(); 
        IOUtils.copy(inputStream, writer, ENCODING); 
        return  writer.toString(); 
    }

	public static void addPartMethod01() throws Exception {
	
		String textFileName = "C:/tmp/AnalisiTraffico.xlsx";
		File file = new File(textFileName);
		
		//HttpPost post = new HttpPost("http://localhost:8080/jodapp/");
        ///jodapp/converted/document.pdf
        HttpPost post = new HttpPost("http://localhost:8080/jodapp/converted/document.pdf");
		
		
		FileBody fileBody = new FileBody(file, ContentType.DEFAULT_BINARY);
		StringBody stringBody1 = new StringBody("pdf", ContentType.DEFAULT_TEXT);
		
		// 
		MultipartEntityBuilder builder = MultipartEntityBuilder.create();
		builder.setMode(HttpMultipartMode.BROWSER_COMPATIBLE);
		builder.addPart("inputDocument", fileBody);
		builder.addPart("outputFormat", stringBody1);
		HttpEntity entity = builder.build();
		
		//
		post.setEntity(entity);
		
		CloseableHttpClient client = HttpClients.createDefault();
		
		CloseableHttpResponse response1 = client.execute(post);
		//Object[] response = null;
		
		try {
	        HttpEntity entity1 = response1.getEntity();
	        //String responseBody = responseAsString(response1);
	        int responseStatus = response1.getStatusLine().getStatusCode();
	        System.out.println("responseStatus: " + responseStatus);
	        
	        
	        //response = new Object [] {responseStatus, responseBody};
	
	        // do something useful with the response body
	        // and ensure it is fully consumed
	        //EntityUtils.consume(entity1);
	        
	        //int length = (int) entity1.getContentLength();
	        System.out.println("length: " + entity1.getContentLength());
	        System.out.println("ctype: " + entity1.getContentType());
	        
	        /*
			if (entity1 != null) {
				String respoBody = EntityUtils.toString(entity1, "UTF-8");
				System.out.println(respoBody);
			} */           
	        
		} finally {
	        response1.close();
		}		
	
	    client.close();
	}

	public static void addPartMethod02() throws Exception {
	
		System.out.println("createDocument(CloseableHttpClient)");
		CloseableHttpClient httpclient = HttpClients.createDefault();
		
        //HttpPost httppost = new HttpPost("http://localhost:8080/jodapp/"); 
        ///jodapp/converted/document.pdf
        HttpPost httppost = new HttpPost("http://localhost:8080/jodapp/converted/document.pdf");

		File f = new File("C:/tmp/AnalisiTraffico.xlsx");
		System.out.println(f.getName());
		
		String jsonStr = "pdf";		
        
		StringBody txtPart = new StringBody(jsonStr, ContentType.DEFAULT_TEXT);
        FileBody binPart = new FileBody(f);        

        HttpEntity reqEntity = MultipartEntityBuilder.create()
                .addPart("outputFormat", txtPart)
                .addPart("inputDocument", binPart)
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

}
