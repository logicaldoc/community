package com.logicaldoc.webservicesamples;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.apache.commons.io.FilenameUtils;
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

public class RestJodConverterStressTest {
	
	protected static final String ENCODING = "UTF-8";

	public static int errors; 
	
	public List<File> documents = new ArrayList<File>();

	public static File outputFolder;
	private static long startTime;
	
	public static void main(String[] args) throws Exception {
		
		startTime = System.currentTimeMillis();
		
		// 1) read all documents in a folder and put pointers in a list
		String sDir = "C:/Users/Alle/Desktop/test-documents";
		//String sDir = "C:/Users/Alle/Desktop/test-documents/Sedacom2";
		//String sDir = "C:/Users/Alle/Desktop/test-documents/Marketing"; 
		
		RestJodConverterStressTest xxx = new RestJodConverterStressTest();
		xxx.importFolder(sDir);
		
		// 2) start the conversion process
		String basePath = "C:/tmp";
		xxx.downloadDocumentsMultiThread(basePath);
				
		System.err.println("Operation completed");
		System.err.println("Total Failed operations: " + errors);
		
		System.err.println("Total tasks: " + xxx.getDocumentsCount());		
		long elapsedTime = System.currentTimeMillis() - startTime;
		double seconds = elapsedTime / 1000;		
		System.err.println("Job ended in: " + Math.round(seconds) + " seconds");
		System.err.println("Job started at: " + new java.util.Date(startTime));
		System.err.println("Job ended at: " + new java.util.Date());			
	}
	
	
	
	private int getDocumentsCount() {
		return documents.size();
	}



	private void importFolder(String sDir) throws Exception {

		File[] faFiles = new File(sDir).listFiles();

		for (File file : faFiles) {

			if (file.isDirectory()) {
				// System.out.println("FOLDER: " + file.getName());	
				importFolder(file.getAbsolutePath());
			} else {
				// add the document to the list
				documents.add(file);
			}
		}
	}	

	private void downloadDocumentsMultiThread(String basePath) throws IOException {
		
		Date date = new Date(); //creates a date based on current date/time
	
		//provides a formatting string for your eventual output
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd"); 		
		System.out.println(sdf.format(date));		
		
		String datePart =  sdf.format(date);
		
		// Create path for the current folder
		String parentPath = basePath + "/mPDFc_" +datePart;
		//check if the dir exists on the filesystem
				
		File file = new File(parentPath);
		if (!file.exists()) {
			file.mkdirs();
		}
		this.outputFolder = file;
		
		// With 5: Total Failed operations: 0
		//ExecutorService executor = Executors.newFixedThreadPool(5);
		// With 10: Total Failed operations: 3
		//ExecutorService executor = Executors.newFixedThreadPool(10);
		// With 15: Total Failed operations: 10
		//ExecutorService executor = Executors.newFixedThreadPool(15);
		
		// Nuxeo 5: Total Failed operations: 2 - OO 4.1.1
		// Nuxeo 10: Total Failed operations: 19 - OO 4.1.1
		// Nuxeo 15: Total Failed operations: 43 - OO 4.1.1
		
		// Jahia 10: Total Failed operations: 4 - OO 4.1.1
		// Jahia 15: Total Failed operations: 12 - OO 4.1.1
		// Jahia 15: Total Failed operations: 10 - OO 4.1.1
		
		// XWiki 15: Total Failed operations: 10 - OO 4.1.1
		// XWiki 15: Total Failed operations: 10 - OO 4.1.1
		
		
		// XWiki 15: Total Failed operations: 2 - LO 5.3.0
		// XWiki 15: Total Failed operations: 5 - LO 5.3.0		
		// Jahia 15: Total Failed operations: 121 - LO 5.3.0
		// Jahia 15: Total Failed operations: 9 - LO 5.3.0
		// Nuxeo 15: Total Failed operations: 33 - LO 5.3.0
		// Nuxeo 15: Total Failed operations: 36 - LO 5.3.0
		// Original 15: Total Failed operations: 7 - LO 5.3.0
		ExecutorService executor = Executors.newFixedThreadPool(15);
	
		System.out.println("TOTAL DOCUMENTS TO PROCESS: " +documents.size());
		
	    for(final File documentFile : documents) {
	        executor.submit(new Runnable() {
	            @Override
	            public void run() {
					doProcessing(documentFile);
	            }
	        });
	    }
	    
	    // http://stackoverflow.com/questions/1250643/how-to-wait-for-all-threads-to-finish-using-executorservice
	    executor.shutdown();
	    try {
	    	//executor.awaitTermination(360, TimeUnit.SECONDS);
	    	executor.awaitTermination(600, TimeUnit.SECONDS);
	    } catch (InterruptedException e) {
	    	e.printStackTrace();
	    }
	}



	protected void doProcessing(File documentFile) {
		// TODO Auto-generated method stub
		try {
			//addPartMethod01(documentFile);
			//addPartMethod02(documentFile);
			addBinaryBody03(documentFile);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}



	public static void addPartMethod02(File documentFile) throws Exception {
	
		//System.out.println("createDocument(CloseableHttpClient)");
		CloseableHttpClient httpclient = HttpClients.createDefault();
		
	    HttpPost httppost = new HttpPost("http://localhost:8080/jodapp/converted/document.pdf");		
	
		//File f = new File("C:/tmp/AnalisiTraffico.xlsx");
	    File f = documentFile;
		System.out.println(f.getName());
	    
		StringBody txtPart = new StringBody("pdf", ContentType.DEFAULT_TEXT);
	    FileBody binPart = new FileBody(f);        
	
	    HttpEntity reqEntity = MultipartEntityBuilder.create()
	            .addPart("outputFormat", txtPart)
	            .addPart("inputDocument", binPart)
	            .build();
		
		httppost.setEntity(reqEntity);
	
		CloseableHttpResponse response = httpclient.execute(httppost);
		try {
	        int responseStatus = response.getStatusLine().getStatusCode();
	        System.out.println("responseStatus: " + responseStatus);
	        
			HttpEntity rent = response.getEntity();
			if (rent != null) {
				/*String respoBody = EntityUtils.toString(rent, "UTF-8");
				System.out.println(respoBody); */
				System.out.println("ctype: " + rent.getContentType());
		        System.out.println("length: " + rent.getContentLength());
		        
		        if (responseStatus == 200) {
					// do something useful with the response body
					// and ensure it is fully consumed
					EntityUtils.consume(rent);	
		        } else {
		        	String respoBody = EntityUtils.toString(rent, "UTF-8");
		        	System.err.println("responseStatus: " + responseStatus);
		        	System.err.println(f.getName());
					System.err.println(respoBody);
		        }
			}
		} finally {
			response.close();
		}
	}



	public static void addPartMethod01(File documentFile) throws Exception {
	
		//String textFileName = "C:/tmp/AnalisiTraffico.xlsx";
		//File file = new File(textFileName);
		File file = documentFile;
		
		//HttpPost post = new HttpPost("http://localhost:8080/jodapp/");
        ///jodapp/converted/document.pdf
        HttpPost httppost = new HttpPost("http://localhost:8080/jodapp/converted/document.pdf");
		
		
		FileBody fileBody = new FileBody(file, ContentType.DEFAULT_BINARY);
		StringBody stringBody1 = new StringBody("pdf", ContentType.DEFAULT_TEXT);
		
		// 
		MultipartEntityBuilder builder = MultipartEntityBuilder.create();
		builder.setMode(HttpMultipartMode.BROWSER_COMPATIBLE);
		builder.addPart("inputDocument", fileBody);
		builder.addPart("outputFormat", stringBody1);
		HttpEntity entity = builder.build();
		
		//
		httppost.setEntity(entity);
		
		CloseableHttpClient httpclient = HttpClients.createDefault();
		
		CloseableHttpResponse response = httpclient.execute(httppost);
		try {
	        int responseStatus = response.getStatusLine().getStatusCode();
	        System.out.println("responseStatus: " + responseStatus);
	        
			HttpEntity rent = response.getEntity();
			if (rent != null) {
				/*String respoBody = EntityUtils.toString(rent, "UTF-8");
				System.out.println(respoBody); */
				System.out.println("ctype: " + rent.getContentType());
		        System.out.println("length: " + rent.getContentLength());
		        
		        if (responseStatus == 200) {
					// do something useful with the response body
					// and ensure it is fully consumed
					EntityUtils.consume(rent);	
		        } else {
		        	String respoBody = EntityUtils.toString(rent, "UTF-8");
		        	System.err.println("responseStatus: " + responseStatus);
		        	System.err.println(file.getName());
					System.err.println(respoBody);
		        }
			}
		} finally {
			response.close();
		}		
	
		httpclient.close();
	}

	public static void addBinaryBody03(File documentFile) throws Exception {
	
		//System.out.println("createDocument(CloseableHttpClient)");
		CloseableHttpClient httpclient = HttpClients.createDefault();
		
        //HttpPost httppost = new HttpPost("http://localhost:8080/jodapp/converted/document.pdf");
		HttpPost httppost = new HttpPost("http://localhost:8080/jodxwiki/converted/document.pdf");
        //HttpPost httppost = new HttpPost("http://ubuntu14:8080/jodapp/converted/document.pdf");
		//HttpPost httppost = new HttpPost("http://ubuntu14:8080/jodxwiki/converted/document.pdf");

		//File f = new File("C:/tmp/AnalisiTraffico.xlsx");
        File f = documentFile;
		System.out.println(f.getName());
        
        MultipartEntityBuilder builder = MultipartEntityBuilder.create();
        builder.setMode(HttpMultipartMode.BROWSER_COMPATIBLE);
        // The one commented below is to run correctly on Windows 10 Professional 64bit
        builder.addBinaryBody("inputDocument", f, ContentType.DEFAULT_BINARY, f.getName()).setCharset(Charset.forName("UTF-8")); 
        //builder.addBinaryBody("inputDocument", f, ContentType.DEFAULT_BINARY, f.getName()); // Unix - Linux
        builder.addTextBody("outputFormat", "pdf", ContentType.DEFAULT_BINARY);
        
        // 
        HttpEntity reqEntity = builder.build();
		httppost.setEntity(reqEntity);

		CloseableHttpResponse response = httpclient.execute(httppost);
		try {
	        int responseStatus = response.getStatusLine().getStatusCode();
	        System.out.println("responseStatus: " + responseStatus);
	        
			HttpEntity rent = response.getEntity();
			if (rent != null) {
				/*String respoBody = EntityUtils.toString(rent, "UTF-8");
				System.out.println(respoBody); */
				System.out.println("ctype: " + rent.getContentType());
		        System.out.println("length: " + rent.getContentLength());
		        
		        if (responseStatus == 200) {
					// do something useful with the response body
					// and ensure it is fully consumed
					EntityUtils.consume(rent);
		        
		        	/*
		            String baseName = FilenameUtils.getBaseName(documentFile.getName());
		            File outputFile = new File(outputFolder, baseName + ".pdf");

		            OutputStream outputStream = null;
		            InputStream inputStream = null;
		            try {
		            	outputStream = new FileOutputStream(outputFile);
		            	inputStream = rent.getContent();
		                IOUtils.copy(inputStream, outputStream);
		            } finally {
		            	IOUtils.closeQuietly(inputStream);
		            	IOUtils.closeQuietly(outputStream);
		            } 
		            */
		            
		        } else {
		        	String respoBody = EntityUtils.toString(rent, "UTF-8");
		        	System.err.println("responseStatus: " + responseStatus);
		        	System.err.println(f.getName());
					System.err.println(respoBody);
					errors++;
		        }
			}
		} finally {
			response.close();
		}
	}    

}
